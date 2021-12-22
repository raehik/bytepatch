## Unreleased
  * clean up FunctorRec into standalone HFunctorList module

## 0.3.1 (2021-12-21)
  * packaging improvements (documentation, CI)

## 0.3.0 (2021-12-18)
Large internal refactoring.

  * Various concepts surrounding stream patching have been decoupled and
    parameterized. Pre-apply patch transformations are modelled as "metadata
    layers" which can be stripped off, and patch application functions require
    that no irrelevant metadata is present.
  * A new CLI inspired by the original tool exposes the useful bits.
  * Internally, the package is split into streampatch and bytepatch. If I could
    be bothered, they would be different packages, streampatch not requiring
    things like Aeson and Megaparsec.

## 0.2.1 (2021-12-03)
  * large internal refactoring (still more to come!)
  * schema refactoring (patch -> edit, offsets -> at)
  * Hackage metadata improvements (initial release was a bit wonky)

## 0.2.0 (2021-12-03)
Initial release.

  * extracted & rewrote tool (library + CLI) from gtvm-hs
