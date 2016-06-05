;;; helm-fuzzier.el --- Better fuzzy matching for Helm -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ephram Perdition

;; Author: Ephram Perdition
;; Created: 25 Oct 2015

;; Package-Requires: ((emacs "24.3") (helm "1.7.0"))

;; Keywords: convenience helm fuzzy
;; Homepage: http://github.com/EphramPerdition/helm-fuzzier

;; This file is NOT part of GNU Emacs.

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
;; Use in conjunction with the 'helm-flx' package for best results.
;; `helm-fuzzier' improves Helm's "matching", meaning that it tries
;; to ensure the best matches appear on the result list. 'helm-flx',
;; on the other hand, provides on-the-fly highlighting and (dramatically)
;; improves Helm's "scoring" so that the results are shown ordered from
;; "best" to "worst" match.
;;
;; Usage:
;;
;;   (require 'helm-fuzzier)
;;   (helm-fuzzier-mode 1)
;;

;; The queries you currently use should continue to work reliably.
;; To take advantage of 'helm-fuzzier', queries should begin with the
;; same letter as the desired match and should form an abbreviation of
;; two or more word prefixes from the match.
;;
;; Query Examples:
;;
;; - 'emacs-lisp-mode' can be matched by 'el','em', 'elm', 'eli', 'elmo', etc'.
;; - 'helm-candidate-number-limit' can be matched by 'hcn','hnl', 'hecl', etc'.
;; - 'package-list-packages' can be matched by 'plp','plpa', 'paclp', etc'.
;;
;; Discussion:
;;
;; Helm's support (As of Oct 2015) for fuzzy matching breaks down when
;; the number of matches exceeds its internal limit
;; 'helm-candidate-number-limit'. Helm will stop looking once it finds
;; LIMIT matches, even if better matches exist among the remaining candidates.
;; The result is that the best matches are often not included in the results.

;; Helm additionally separates *matching* from *scoring* into separate
;; phases.  the former simply collects LIMIT matches of whatever
;; quality, the later sorts them from best to worst according to some
;; heuristic.

;; 'helm-fuzzier' augments helm's default *matching* phase with an
;; additional *preferred matching* phase which examines _all
;; candidates for matches that are likely to score highly in the
;; *scoring* phase, and makes sure they are included in the result
;; list presented to the user. Without this, the best results are
;; overlooked when there are lots of low-quality fuzzy matches, and
;; this failure occurs most often with short queries, exactly the case
;; users most care about (less keystrokes).

;; For preferred matching to produce good results there must be good
;; agreement between what it and what the *scoring* function consider
;; a "good match".  'helm-fuzzier' was written (and tested) for use
;; with the new 'helm-flx' package recently added to MELPA (by
;; @PythonNut), which enhances Helm's *scoring* phase by using
;; @lewang's 'flx' library.
;;

;;; Code:

(require 'cl-lib)
(require 'helm) ; we redefine some helm functions, ensure it is loaded first

;;; Customization
(defgroup helm-fuzzier nil
  "Better Fuzzy Matching For Helm."
  :group 'helm)

(defcustom helm-fuzzier-preferred-max-group-length 4
  "Knob controlling regex generation for fuzzier matching.

Trade speed for accuracy.
Larger values create increasingly expensive regexs.  Number of clauses
grows very quickly as this value increases, so set this conservatively.

A value of 1 will match:
\"bbs\" -> \"(b)ulletin-(b)oard-(s)ystem\"
but not:
\"bubs\" -> \"(bu)lletin-(b)oard-(s)ystem\"
and not:
\"bubsy\" -> \"(bu)lletin-(b)oard-(sy)stem\"

a value of 2 will match all. a value of 3 will match
\"bulbs\" -> \"(bul)letin-(b)oard-(s)sstem\"

... and so on"
  :group 'helm-fuzzier
  :type  'integer)

(defcustom helm-fuzzier-max-query-len 5
  "Upper limit on query length for preferred matching.

Just as with 'helm-fuzzier-preferred-max-group-length', the number
of clauses in the regex we generate for matching grows quickly
with the length of the query. If the query we are given exceeds
this limit, we recuse ourselves and let helm take over, hoping that
that the query is long enough to reduce the number of candidates
below limit, so the scoring phase will get to see all possible matches."
  :group 'helm-fuzzier
  :type  'integer)


(defcustom helm-fuzzier-word-boundaries "- /:|_"
  "List of characters that indicate a word boundary.

The characters are interpolated as-is into a regex inside
a charcater class \"[%s]\" so be careful about quoting.

The default value should work for the conventions common
in Emacs symbols (i.e \"foo/bar-the-baz\") and in filenames
(i.e. \"the file name\")"
:group 'helm-fuzzier
:type  'string)

(defcustom helm-fuzzier-max-candidates-to-scan 75000
  "Hard limit on number of candidates to test for preferred match.

Generally, we want to examine all possible candidates, and this should be
fast enough. It is prudent none the less to place a hard upper limit on the
number of candidates we scan"
  :group 'helm-fuzzier
  :type  'integer)

(defcustom helm-fuzzier-remaining-matches-quota 10
  "Lower limit on number of matche slots left for Helm to fulfill.

Set this to a (small, positive) value below 'helm-candidate-number-limit'.

'helm-fuzzier' will then provide up to (- LIMIT quota) matches
and leave the remaining slots to be filled by Helm's matching logic."

  :group 'helm-fuzzier
  :type  'integer)

;;; internal variables
(defvar helm-fuzzier-preferred-candidates-cache (make-hash-table :test 'equal :size 1024)
  "Caches the complete candidates list for locating preferred matches.")
(defvar helm-fuzzier-old-helm-match-fn nil
  "Ref to original helm match function.")
(defvar helm-fuzzier-match-hash (make-hash-table :test 'equal)
  "holds matches for dedupe during filtering")

(defun helm-fuzzier--mapconcat-initials-pattern-1 (groups seperators)
  "Construct regexp from GROUPS to match them as separated initials of a string.
e.g (helm-fuzzier--mapconcat-initials-pattern-1 '(\"a\" \"bc\" \"d\") \"-/\")
will return a pattern that matches \"a123/bc45-d\"

SEPERATORS is a string contains one or more word seperators. Any characters
which are not regex-safe should be quoted."
  (concat "\\("
          (format "^%s" (car groups))
          (mapconcat (lambda (c)
                       (if (and (string= c "$")
                                (string-match "$\\'" (apply 'concat groups)))
                           c (format "\\(.*[%s]%s\\)" seperators c)))
                     (cdr groups) "")
          "\\)"))

(defun helm-fuzzier--partition-string (query max-length &optional level)
  "Returns a list of all possible partitions of the QUERY string.

Returns all possible partitions where no substring has length > MAX-LENGTH

Note: Recursive and may blow stack. Caller will normally invoke us with
a short QUERY, so stack overflow shouldn't occur.

Example:

(helm-fuzzier--partition-string \"abcd\" 3)
((\"a\" \"b\" \"c\" \"d\") (\"a\" \"b\" \"cd\") (\"a\" \"bc\" \"d\") (\"a\" \"bcd\") (\"ab\" \"c\" \"d\") (\"ab\" \"cd\") (\"abc\" \"d\"))"

  (let ((qlen (length query)))
    (if (= qlen 1)
        (list (list query))
      (cl-loop
         for group-length from 1 to (min max-length
                                         ;; Forbid first group from consuming entire string
                                         ;; To enforce at least two groups.
                                         (- qlen
                                            (if level
                                                0
                                              1)))
         append
           (if (= group-length qlen)
               (list (list query)) ;; base case
             ;; Break off group from head of string, recurse with the remaining
             ;; string to get a list of lists of string (groups)
             (let* ((string-head (substring query 0 group-length))
                    (string-tail (substring query
                                            group-length
                                            qlen))
                    (partitions (helm-fuzzier--partition-string string-tail
                                                                max-length
                                                                (1+ (or level 0)))))
               ;; Build the result list by prepending the current group
               ;; to every one of the possible partitions of the remaining string
               (mapcar (lambda (other-groups) (cons string-head other-groups))
                       partitions)))))))


(defun helm-fuzzier--mapconcat-initials-pattern (pattern seperators &optional max-group-length)
  "Transform string PATTERN into a regexp for fuzzy matching as initials.

With SEPERATORS as a string of regex-quoted word-boundary characters
(\"- /\"), partition pattern into groups in various ways and construct
a regex pattern that tries to match any of these variations against
the word prefixes in a candidate.

The regex generation variations is controlled by the MAX_GROUP-LENGTH argument:

With MAX-GROUP-LENGTH=1 the pattern generated for \"abc\" will match
\"a...-b...-c\"

With MAX-GROUP-LENGTH=2 the pattern generated for \"abc\" will match
\"a...-b...-c...\" or \"ab...-c...\" or \"a...-bc....\"

etc'."
  (mapconcat (lambda (ls) (helm-fuzzier--mapconcat-initials-pattern-1 ls seperators))
             (helm-fuzzier--partition-string
              pattern
              max-group-length)
             "\\|"))

(defun helm-fuzzier--make-initials-matcher (pattern &optional seperators max-group-length )
  "Constructs a matching function for PATTERN.

See 'helm-fuzzier--mapconcat-initials-pattern' docstring for information
about SEPERATORS and MAX-GROUP-LENGTH"
  (let* ((initials-pat
          (concat
           ;; match prefix of single word match
           (format "\\(^%s[^%s]*$\\)\\|" pattern (or seperators
                                                     helm-fuzzier-word-boundaries))
           ;; match abbreviations
           (helm-fuzzier--mapconcat-initials-pattern
            pattern
            (or seperators helm-fuzzier-word-boundaries)
            (or max-group-length
                helm-fuzzier-preferred-max-group-length))))
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

(defun helm-fuzzier-candidates-in-buffer-search-default-fn (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args.

Preferred matching requires initial letter to match use that to reduce
the number of candidates to check."
  (let ((pat (format "^%c" (aref pattern 0))))
    (condition-case _err
        (re-search-forward pat nil t)
      (invalid-regexp nil))))

(defun helm-fuzzier--get-all-source-candidates-no-really-NO-REALLY (source query)
  "Get all candidates for SOURCE. really. no, REALLY.

'get-cached-candidates' won't work, even if we clrhash first.
Actually, we don't REALLY get all candidates, we do limit the list to
all candidates which match query. We should only be called at the start
of a new query (prefix) and this can cut down the scan list dramatically."

  (let* ((candidates  (if (eq (assoc-default 'candidates source) #'helm-candidates-in-buffer)
                          (helm-candidates-in-buffer-1
                           (helm-candidate-buffer)
                           query
                           (or (assoc-default 'get-line source)
                               #'buffer-substring-no-properties)
                           '(helm-fuzzier-candidates-in-buffer-search-default-fn)
                           helm-fuzzier-max-candidates-to-scan
                           (helm-attr 'match-part)
                           source)
                        (helm-get-candidates source))))
    candidates))

(defun helm-fuzzier--matchfn-stub (&rest _)
  (user-error "I should not have been called"))

(defun helm-fuzzier--get-preferred-matches (cands _ match-part-fn limit source)
  "Perform a scan over all candidates looking for \"Preferred Matches\".

'helm-fuzzier-preferred-candidates-cache' is used to cache a list of
all candidates when a new query is entered. This is exclusively to
support 'source-in-buffer' Helm sources, which perform the optimization
described in the previous paragraph, before we even get to see the
candidates. Because preferred matching relies cruically on examining
all possible candidates, we have to cut through that optimization
or we won't ever see the matches we're trying to boost.

CANDS MATCH-PART-FN LIMIT SOURCE are the same arguments as found
in 'helm-match-from-candidates' ."

  ;; when a new query begins we need to reset the caches.
  (when (helm-fuzzier--new-nonempty-query-p source helm-pattern)
    (puthash (assoc-default 'name source)
             (helm-fuzzier--get-all-source-candidates-no-really-NO-REALLY source helm-pattern)
             helm-fuzzier-preferred-candidates-cache)
    (puthash (concat (assoc-default 'name source) "-query") helm-pattern helm-fuzzier-preferred-candidates-cache)
    ;; (message "Count All Candidates: %d" (length (gethash (assoc-default 'name source)
    ;;                                                      helm-fuzzier-preferred-candidates-cache)))
    )

  (let* ((source-name (assoc-default 'name source))
         (matcher (helm-fuzzier--make-initials-matcher (substring helm-pattern
                                                                  0
                                                                  ;; limit query len to control
                                                                  ;; regex complexity
                                                                  (min (length helm-pattern)
                                                                       helm-fuzzier-max-query-len))))
         (all-candidates (or (gethash source-name helm-fuzzier-preferred-candidates-cache)
                             cands))
         (preferred-matches (when (and
                                   (> (length helm-pattern) 1)
                                   (assoc 'fuzzy-match source))
                              (helm-fuzzier-orig-helm-match-from-candidates all-candidates
                                                                            (list matcher)
                                                                            match-part-fn limit source)))
         ;; when the query exceeds max-query-len, the matcher will truncate
         ;; it to limit the complexity of the regex, so we may end up with
         ;; false positive matches. Weed these out by passing them as
         ;; candidates to helm's fuzzy matcher.
         (result (if (<= (length helm-pattern)
                         helm-fuzzier-max-query-len)
                     preferred-matches
                   (clrhash helm-fuzzier-match-hash)
                   (helm-fuzzier-orig-helm-match-from-candidates preferred-matches
                                                                 (list (symbol-function helm-fuzzy-match-fn))
                                                                 match-part-fn limit source))))
    result))

;; This function copied from Helm commit b1ec2d1ab48f137e712.
;; Modified to use helm-fuzzier-match-hash instead of creating a local hash table
;; so that the dedupe of matches takes place with a hash table which has already
;; been populated by the preferred matches we've found.
(defun helm-fuzzier-orig-helm-match-from-candidates (cands matchfns match-part-fn limit source)
  (condition-case-unless-debug err
      (delq nil
            (cl-loop with hash = helm-fuzzier-match-hash
               with allow-dups = (assq 'allow-dups source)
               with case-fold-search = (helm-set-case-fold-search)
               with count = 0
               for iter from 1
               for fn in matchfns
               when (< count limit) nconc
                 (cl-loop for c in cands
                    for dup = (gethash c hash)
                    while (and (< count limit)
                               ;; When allowing dups check if DUP
                               ;; have been already found in previous loop
                               ;; by comparing its value with ITER.
                               (or (and allow-dups dup (= dup iter))
                                   (null dup)))
                    for target = (helm-candidate-get-display c)
                    for part = (if match-part-fn
                                   (funcall match-part-fn target)
                                 target)
                    when (funcall fn part) do
                      (progn
                        ;; Modify candidate before pushing it to hash.
                        (helm--maybe-process-filter-one-by-one-candidate c source)
                        ;; Give as value the iteration number of
                        ;; inner loop to be able to check if
                        ;; the duplicate have not been found in previous loop.
                        (puthash c iter hash)
                        (cl-incf count))
                    and collect c)))
    (error (unless (eq (car err) 'invalid-regexp) ; Always ignore regexps errors.
             (helm-log-error "helm-match-from-candidates in source `%s': %s %s"
                             (assoc-default 'name source) (car err) (cdr err)))
           nil)))


(defun helm-fuzzier--match-from-candidates (cands matchfns match-part-fn limit source)
  "Substitute function for 'helm-match-from-candidates'.

First perform the \"preferred matches\" pass and then call the default helm
logic to fill the remaining quota with matches using its algo. Returns
the result of both concatenated into one list."

  (clrhash helm-fuzzier-match-hash) ; Clear hashtable used for deduping results across multiple matchfns

  (let* ((with-preferred (member #'helm-fuzzier--matchfn-stub matchfns ))
         (matchfns (cl-remove #'helm-fuzzier--matchfn-stub matchfns))
         (preferred-matches (when with-preferred
                              (helm-fuzzier--get-preferred-matches
                               cands
                               matchfns
                               match-part-fn
                               (max 0 (- limit helm-fuzzier-remaining-matches-quota))
                               source)))
         (remaining-count (max 0 (- limit (length preferred-matches))))
         (matches (helm-fuzzier-orig-helm-match-from-candidates
                   cands
                   matchfns
                   match-part-fn remaining-count source)))
    (append preferred-matches matches)))

(defun helm-fuzzier--advice-helm-compute-matches (orig-fun source)
  (let* ((matchfns (helm-match-functions source))
         (matchfns (if (listp matchfns)
                       matchfns
                     (list matchfns)))
         (source-is-fuzzy (assoc 'fuzzy-match source)))

    ;; For every source which has fuzzy-matching enabled we insert
    ;; a canary matchfn.  This also serves to disable an inconvenient
    ;; optimization in helm's 'helm-compute-matches' where if matchfns
    ;; is simply '(identity) the entire magic logic is skipped which
    ;; means we wouldn't get a chance to influence the results.

    (when source-is-fuzzy
      (let ((matchfns (append (list #'helm-fuzzier--matchfn-stub)
                              matchfns)))
        ;; override matchfns with a list containing our canary
        (push (cons 'match matchfns) source)))

    (funcall orig-fun source)))

;;;###autoload
(define-minor-mode helm-fuzzier-mode
  "helm-fuzzier minor mode"
  :init-value nil
  :group 'helm-fuzzier
  :global t
  (if helm-fuzzier-mode
      (progn
        (when (>= helm-fuzzier-remaining-matches-quota
                  helm-candidate-number-limit)
          (warn "helm-fuzzier-mode: 'helm-fuzzier-remaining-matches-quota'  >= 'helm-candidate-number-limit', helm-fuzzier is effectively disabled."))
        (when (not helm-fuzzier-old-helm-match-fn)
          (setq helm-fuzzier-old-helm-match-fn (symbol-function #'helm-match-from-candidates)))
        (setf (symbol-function 'helm-match-from-candidates) #'helm-fuzzier--match-from-candidates)
        (advice-add #'helm-compute-matches :around #'helm-fuzzier--advice-helm-compute-matches))

    (advice-remove #'helm-compute-matches #'helm-fuzzier--advice-helm-compute-matches)
    (setf (symbol-function 'helm-match-from-candidates) helm-fuzzier-old-helm-match-fn)))

(provide 'helm-fuzzier)

;;; helm-fuzzier.el ends here
