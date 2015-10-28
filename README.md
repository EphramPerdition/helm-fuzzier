
[Demo Video](https://www.youtube.com/watch?v=8Ceeew4Hdz8)

### Description

This package provides more intuitive fuzzy matching behavior for [Helm](https://github.com/emacs-helm/helm).

Now available on MELPA, install it with `package-install`.

You should also consider installing the [helm-flx](https://github.com/PythonNut/helm-flx)
package from MELPA, for even better results.

### Usage:

```elisp
  (require 'helm-fuzzier)
  (helm-fuzzier-mode 1)
```

`helm-fuzzier` will only enhance matching for sources that have
fuzzy-matching enabled, so be sure to enable fuzzy-matching in Helm
with the appropriate `*-fuzzy-match` option for the sources you're
interested in (`helm-M-x-fuzzy-match`, `helm-mode-fuzzy-match`,
`helm-apropos-fuzzy-match`, etc'). Note that there is no need to adjust
`helm-candidate-number-limit` when using `helm-fuzzier`, the default
value of `100` should be fine.

### Fuzzier Queries

`helm-fuzzier` matching code runs before, but not instead, of Helm's fuzzy-matching
code so any query that currently works for you should generally continue to do so.

However, with `helm-fuzzier` enabled, a query that:

- Begins with the same letter as the desired match.
- Is formed by stringing together two or more prefixes from the words
that make up the match.

... should nearly always result in the match you're looking for appearing in the
top few results.

Some **Example Queries**:

- (In `helm-M-x`) `emacs-lisp-mode` can be matched by `el`, `em`, `elm`, `eli`, `elmo`, etc'.
- (In `helm-M-x`) `package-list-packages` can be matched by `plp`, `plpa`, `paclp`, etc'.
- (In `describe-variable`) `helm-candidate-number-limit` can be matched by `hcn`, `hnl`, `hecl`, etc'.

Remember to turn on fuzzy matching for these sources with:

```elisp
  (setq helm-M-x-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
```

Try toggling `helm-fuzzier-mode` on and off and see the difference.

### Requirements

- Tested with recent [Helm](https://github.com/emacs-helm/helm) (Oct 2015)
- Emacs 24.3 (needs cl-lib, nothing earlier tested)

### Notes

Please report any bugs/issues.

See source code for more detailed information on the implementation.
