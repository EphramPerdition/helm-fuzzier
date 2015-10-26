;;; helm-fuzzier.el --- Better fuzzy matching for Helm -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ephram Perdition

;; Author: Ephram Perdition

;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0.1.0

;; Keywords: convenience helm fuzzy
;; Homepage: http://github.com/EphramPerdition/helm-fuzzier

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides more intuitive fuzzy matching behavior for Helm.
;; For best results, you should also install the 'helm-flx' package.
;;
;; Usage:
;;
;;   (require 'helm-fuzzier)
;;   (helm-fuzzier-mode 1)
;;
;; Queries should begin with the same letter as the desired match and
;; should form an abbreviation of two or more word prefixes contained
;; in the match.
;;
;; Examples:
;;
;; - 'emacs-lisp-mode' can be matched by 'el','em', 'elm', 'eli', 'elmo', etc'.
;; - 'helm-candidate-number-limit' can be matched by 'hcn','hnl', 'hecl', etc'.
;; -'package-list-packages' can be matched by 'plp','plpa', 'paclp', etc'.
;;
;;
;; Discussion:
;;
;; (As of Oct 2015) Helm's support for fuzzy matching breaks down when
;; the number of matches exceeds its internal limit
;; 'helm-candidate-number-limit'. Helm will only look until it finds
;; the first LIMIT matches, no matter how good they are and then
;; stop. The result is that the best matches are often not included in
;; the results.

;; Helm additionally separates *matching* from *scoring* into separate
;; phases.  the former simply collects LIMIT matches of whatever
;; quality, the later sorts them from best to worst according to some
;; heuristic.

;; 'helm-fuzzier' augments helm's default *matching* phase with an additional
;; *preferred matching* that runs first, examines _all candidates_ and ensures
;; that any "preferred matches" found make it into the shortlist for scoring
;; and presentation to the user. "preferred matches" are candidates which are
;; not only "matches" in a lexical sense but that are also likely to be scored
;; highly by the *scoring* phase. By giving these matches precedence, and
;; including them in the match list before the quota is filled with low-quality
;; matches, the final quality of the results presented to the user is improved,
;; and your are likely to see that match you were looking for more often and
;; using shorter queries.

;; For preferred matching to produce good results there must be good agreement
;; between what it and what the *scoring* function consider a "good match".
;; 'helm-fuzzier' was written (and tested) for use with the new 'helm-flx' package
;; recently added to MELPA (by @PythonNut), which enhances Helm's *scoring* phase
;; by using @lewang's 'flx' library.
;;
;; It is __highly recommended__ that you use both 'helm-fuzzier' and 'helm-flx'
;; packages together for the best fuzzy matching results.
;;
;; Together they provide excellent fuzzy matching, even for short queries, and without
;; requiring that a large value be set for 'helm-candidate-number-limit'.
;;
;; Your query should begin with the initial letter of the match you're looking for,
;; and should be an abbreviation of the match, formed by concatenating one or more
;; letters from its word prefixes, in order.
;;
;; Examples:
;;
;; Queries matching 'emacs-lisp-mode' : 'el','em', 'elm', 'eli', 'elmo', etc'.
;; Queries matching 'helm-candidate-number-limit' : 'hcn','hnl', 'hecl', etc'.
;; Queries matching 'package-list-packages' : 'plp','plpa', 'paclp', etc'.
;;
;; Take a look at the 'helm-fuzzier-preferred-max-group-length' option for
;; some control over this behavior.

;; Performance:
;;
;; The additional pass is more expensive but I've found that it is hardly
;; noticeable even on modest hardware. helm-fuzzier only kicks in for
;; source for which fuzzy-matching is enabled, so be sure to turn it on
;; using helm's *-fuzzy-match boolean custom options.

;;; Code:

(require 'cl-lib)
(require 'helm) ; we redefine some helm functions, ensure it is loaded first

(defcustom helm-fuzzier-preferred-max-group-length 4
  "Knob controlling regex generation for fuzzier matching.

For a query of \"abc\":
- Value of 1 will generate regex that match \"a.*-b.*-c.*\"
- Value of 2 will generate regex that also match \"ab.*-c.*\" and \"a.*-bc.*\"
- etc"
  :group 'helm-fuzzier
  :type  'integer)


(defcustom helm-fuzzier-word-boundaries "- /:|"
  "List of characters that indicate a word boundary.

The characters are interpolated as-is into a regex inside
a charcater class \"[%s]\" so be careful about quoting.

The default value should work for the conventions common
in Emacs symbols (i.e \"foo/bar-the-baz\") and in filenames
(i.e. \"the file name\")"
:group 'helm-fuzzier
:type  'string)



;; internal variables
(defvar helm-fuzzier-preferred-matches-cache (make-hash-table :test 'equal :size 8192)
  "Caches the preferred matches within a single helm invocation.")
(defvar helm-fuzzier-preferred-candidates-cache (make-hash-table :test 'equal :size 1024)
  "Caches the complete candidates list for locating preferred matches.")
(defvar helm-fuzzier-old-helm-match-fn nil
  "Ref to original helm match function.")

(defun helm-fuzzier--mapconcat-initials-pattern-1 (groups seperators)
  "Construct regexp from GROUPS to match them as separated initials of a string.
e.g (helm-fuzzier--mapconcat-initials-pattern-1 '(\"a\" \"bc\" \"d\") \"-/\")
will return a pattern that matches \"a123/bc45-d\"

SEPERATORS is a string contains one or more word seperators. Any characters
which are not regex-safe should be quoted."
  (concat "\\("
          (format "^%s" (car groups) seperators (car groups))
          (mapconcat (lambda (c)
                       (if (and (string= c "$")
                                (string-match "$\\'" pattern))
                           c (format "\\(.*[%s]%s\\)" seperators c)))
                     (cdr groups) "")
          "\\)"))

(defun helm-fuzzier--explode-pattern-to-fuzzy-initials (query max-length)
  "Takes a string QUERY and return a list \"exploded\" variations of it.

The variations include every way to select one group of 1 to MAX-LENGTH
letters in the string and keep the rests as single letters.

Example: (explode \"abc\" 2) =>
         ((\"a\" \"b\" \"c\") (\"ab\" \"c\") (\"a\" \"bc\"))"
  (let (results)
    (cl-loop
       for len from 1 to (min max-length (1- (length query)))
       do (cl-loop for pos from 0 to (if ( = len 1)
                                         0
                                       (- (length query) len))
             for result = (cl-loop
                             for i = 0 then (+ i (if (= i pos)
                                                     len
                                                   1))
                             until (>= i (length query))
                             collect (substring query i
                                                (+ i (if (= i pos)
                                                         len
                                                       1))))
             do (push result results)))
    results))

(defun helm-fuzzier--mapconcat-initials-pattern (pattern seperators &optional max-group-length)
  "Transform string PATTERN into a regexp for fuzzy matching as initials.

With SEPERATORS as a string of regex-quoted word-boundary characters
(\"- /\"), partition pattern into groups in various ways and construct
a regex pattern that tries to match any of these variations against
the word prefixes in a candidate.

The regex generation variations is controlled by the MAX_GROUP-LENGTH argument:

With MAX-GROUP-LENGTH=1 the pattern generated for \"abc\" will matche
\"a...-b...-c\"

With MAX-GROUP-LENGTH=2 the pattern generated for \"abc\" will matche
\"a...-b...-c...\" or \"ab...-c...\" or \"a...-bc....\"

etc'."
  (mapconcat (lambda (ls) (helm-fuzzier--mapconcat-initials-pattern-1 ls seperators))
             (helm-fuzzier--explode-pattern-to-fuzzy-initials
              pattern
              max-group-length)
             "\\|"))

(defun helm--make-initials-matcher (pattern &optional seperators max-group-length )
  "Constructs a matching function for PATTERN.

See 'helm-fuzzier--mapconcat-initials-pattern' docstring for information
about SEPERATORS and MAX-GROUP-LENGTH"
  (let* ((initials-pat (helm-fuzzier--mapconcat-initials-pattern
                        pattern
                        (or seperators helm-fuzzier-word-boundaries)
                        (or max-group-length
                            helm-fuzzier-preferred-max-group-length)))
         (matcher (lambda (candidate)
                    (string-match initials-pat candidate))))
    matcher))

(defun helm-fuzzier--drop-last-char (s)
  (let ((len (or (length s) 0)))
    (when (> len 0)
      (substring s 0 (1- len)))))

(defun helm-fuzzier--new-nonempty-query-p (source query)
  "Check if query is not-empty and not covered by current cached contents."
  (and  (> (length query) 0)
        (not (string-prefix-p
              (or (gethash  (concat (assoc-default 'name source) "-query")
                            helm-fuzzier-preferred-candidates-cache)
                  "\x00")
              query))))

(defun helm-fuzzier--get-all-source-candidates-no-really-NO-REALLY (source query)
  (if (eq (assoc-default 'candidates source) #'helm-candidates-in-buffer)
      (helm-candidates-in-buffer-1
       (helm-candidate-buffer)
       query
       (or (assoc-default 'get-line source)
           #'buffer-substring-no-properties)
       (or (assoc-default 'search source)
           '(helm-candidates-in-buffer-search-default-fn))
       50000
       (helm-attr 'match-part)
       source)
    (helm-get-candidates source)))

(defun helm-fuzzier--matchfn-stub (&rest args)
  (user-error "I should not have been called"))

(defun helm-fuzzier--get-preferred-matches (cands matchfns match-part-fn limit source)
  ;; when a new query begins we need to reset the caches.
  (when (helm-fuzzier--new-nonempty-query-p source helm-pattern)
    (clrhash helm-fuzzier-preferred-matches-cache)
    (puthash (assoc-default 'name source)
             (helm-fuzzier--get-all-source-candidates-no-really-NO-REALLY source helm-pattern)
             helm-fuzzier-preferred-candidates-cache)
    (puthash (concat (assoc-default 'name source) "-query") helm-pattern helm-fuzzier-preferred-candidates-cache))

  (let* ((source-name (assoc-default 'name source))
         (prelim-matcher (helm--make-initials-matcher helm-pattern))
         (cached-preferred (when (> (length helm-pattern) 1)
                             (gethash (concat source-name (helm-fuzzier--drop-last-char helm-pattern))
                                      helm-fuzzier-preferred-matches-cache)))
         (all-candidates (or cached-preferred  ; matches from but-last prefix query
                             (gethash source-name helm-fuzzier-preferred-candidates-cache) ; all candidates for the source
                             cands))
         (preferred-matches (when (and
                                   (> (length helm-pattern) 1)
                                   (< (length helm-pattern) 6)
                                   (assoc 'fuzzy-match source))
                              (funcall helm-fuzzier-old-helm-match-fn all-candidates
                                       (list prelim-matcher)
                                       match-part-fn limit source)))
         (preferred-count (length preferred-matches)))

    ;; iff helm-fuzzier-preferred-max-group-length=1) and the number of
    ;; preferred matches found is below LIMIT, we can be certain that:
    ;;
    ;; 1. We've found all possible matches.
    ;; 2. Any future query having the current query as its prefix cannot match
    ;;    any candidate which isn't in this group of matches.
    ;; So, we cache the results and use that instead of  scanning all candidates
    ;; if the next query extends the current one.
    ;;
    ;; This won't work with group-length > 1:
    ;;
    ;; For example, with query-length=2 the query results "ab" do not
    ;; include "ab-c" Now, if the user extends the query to "abc", it
    ;; *should* match "ab-c"

    (when (and (< preferred-count limit)
               (= helm-fuzzier-preferred-max-group-length 1)
               (> (length helm-pattern) 1))
      (puthash (concat source-name helm-pattern) preferred-matches helm-fuzzier-preferred-matches-cache))
    preferred-matches))

(defun helm-fuzzier--match-from-candidates (cands matchfns match-part-fn limit source)

  (clrhash helm-match-hash)

  (let* ((with-preferred (member #'helm-fuzzier--matchfn-stub matchfns ))
         (matchfns (cl-remove #'helm-fuzzier--matchfn-stub matchfns))
         (preferred-matches (when with-preferred
                              (helm-fuzzier--get-preferred-matches cands matchfns match-part-fn limit source)))
         (remaining-count (max 0 (- limit (length preferred-matches))))
         (matches (funcall helm-fuzzier-old-helm-match-fn cands
                           matchfns
                           match-part-fn remaining-count source)))
    (append preferred-matches matches)))

(defun helm-fuzzier--advice-helm-compute-matches (orig-fun source)
  (let* ((source-name (assoc-default 'name source))
         (matchfns (helm-match-functions source))
         (matchfns (if (listp matchfns)
                       matchfns
                     (list matchfns)))
         (source-is-fuzzy (assoc 'fuzzy-match source)))

    ;; For every source which has ;; fuzzy-matching enabled We insert
    ;; a canary matchfn.  This also serves to disable an inconvenient
    ;; optimization in helm's 'helm-compute-matches' where if matchfns
    ;; is simply '(identity), the entire magic logic is skipped, and
    ;; we wouldn't get a chance to influence the results.

    (when source-is-fuzzy
      (let ((matchfns (append (list #'helm-fuzzier--matchfn-stub)
                              matchfns)))
        ;; override matchfns with a list containing our canary
        (push (cons 'match matchfns) source)))

    (funcall orig-fun source)))

(define-minor-mode helm-fuzzier-mode
  "helm-flx minor mode"
  :init-value nil
  :group 'helm-fuzzier
  :global t
  (if helm-fuzzier-mode
      (progn

        (when (not helm-fuzzier-old-helm-match-fn)
          (setq helm-fuzzier-old-helm-match-fn (symbol-function #'helm-match-from-candidates)))
        (setf (symbol-function 'helm-match-from-candidates) #'helm-fuzzier--match-from-candidates)
        (advice-add #'helm-compute-matches :around #'helm-fuzzier--advice-helm-compute-matches)

        (when (called-interactively-p 'any)
          (message "helm-fuzzier-mode enabled.")))

    (advice-remove #'helm-compute-matches #'helm-fuzzier--advice-helm-compute-matches)
    (setf (symbol-function 'helm-match-from-candidates) helm-fuzzier-old-helm-match-fn)

    (when (called-interactively-p 'any)
      (message "helm-fuzzier-mode disabled."))))

(provide 'helm-fuzzier)

;;; helm-fuzzier.el ends here
