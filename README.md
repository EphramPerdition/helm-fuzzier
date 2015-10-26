### Description

This package provides more intuitive fuzzy matching behavior for Helm.
For best results, you should also install the [helm-flx](https://github.com/PythonNut/helm-flx)
package from MELPA.

### Usage:

```elisp
  (require 'helm-fuzzier)
  (helm-fuzzier-mode 1)
```

Queries should begin with the same letter as the desired match and
should form an abbreviation of two or more word prefixes contained
in the match.

Examples:

- 'emacs-lisp-mode' can be matched by 'el','em', 'elm', 'eli', 'elmo', etc'.
- 'helm-candidate-number-limit' can be matched by 'hcn','hnl', 'hecl', etc'.
-'package-list-packages' can be matched by 'plp','plpa', 'paclp', etc'.

### Requirements

- Tested with recent Helm (Oct 2015)
- Emacs 24.3 (needs cl-lib, nothing earlier tested)

### Notes

See source code for more.
