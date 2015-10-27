### Description

This package provides more intuitive fuzzy matching behavior for [Helm](https://github.com/emacs-helm/helm).

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

### Fuzzy matching in Helm

#### tl;dr:

- Helm provides "fuzzy matching" out of the box.
- But what many users actually want is "flex matching".
- `helm-fuzzier` and `helm-flx` each improve Helm's matching in distinct ways.
- Together, they finally provide the sort of "flex matching" you want, in Helm,
for any source that has fuzzy matching enabled.

#### The Whole Story

Fuzzy matching in Helm consists of two distinct parts: 'matching' and
'scoring'. The former selects candidates that match a query and the
latter sorts them from best to worse so that the best results appear
first in the result set presented to the user.

Unfortunately, (As of Oct 2015) the way both are implemented in
vanilla Helm does not match the behavior many people have come to expect
from other editors. A distinction is sometimes made between
*fuzzy-matching* (which vanilla Helm provides) and *flex matching*
which, as far as I know, originated from (or at least was popularized
by) a Mac Application called
[Quicksilver](https://en.wikipedia.org/wiki/Quicksilver_%28software%29),
variants of the Quicksilver concept are now quite popular and part
of many software packages, from [smex](https://github.com/nonsequitur/smex)
to "Sublime Text". You can search for "quicksilver algorithm" to see how
far back it goes.

@lewang wrote up the wonderful [flx](https://github.com/lewang/flx)
package in 2013 which finally provided a quicksilver-like *scoring* of matches,
but until recently (Oct. 2015) `flx` only supported `ido`, not Helm.
@PythonNut packaged a snippet from his `.emacs` as `helm-flx` to remedy
this and so the scoring part of Helm now works very well.

Improving Helm's *matching*, the other half of getting good results, is
what `helm-fuzzier` does. Although good scoring ensures
you see the best results first, it can only boost the results it gets
to rank. By default, Helm will find the first 100 "matches" and stop
scanning, so very often `helm-flx` doesn't even get to see the best
matches in order to score them. `helm-fuzzier` addresses this by performing
an initial scan for "Preferred Matches" over *all* candidates, before
handing control back to helm to find further matches with its default
logic.

If you use one but not the other you'll get "better" results but
you'll only fix half the problem. Of course, because both `flx` and
`helm-fuzzier` rely on heuristics to define a "good" candidate, you
might want to replace one or the other with some other set of heuristics
You prefer. Currently, there's not much choice out there but you could
probably adapt the code in [fuzzy-el](https://github.com/auto-complete/fuzzy-el),
[smex](https://github.com/nonsequitur/smex) or maybe even [grizzl](https://github.com/d11wtq/grizzl)
to work with `helm-fuzzier`. I currently use `helm-fuzzier` with `helm-flx`
and am extremely happy with the results.

To summarise, `helm-flx` and `helm-fuzzier` complement one another,
neither strictly requires the other, neither is a substitute for the
other and, currently, using them both will give you the best
experience.  It's about bloody time, Emacs.
