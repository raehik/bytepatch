## 0.3.0 (Unreleased)
Large internal refactoring. Various concepts surrounding stream patching have
been decoupled and parameterized. A CLI interface can't expose all the internals
in a useful way any longer, so I intend to rebrand this as streampatch and write
a CLI elsewhere that focuses just on binary file patches.

## 0.2.1 (2021-12-03)
  * large internal refactoring (still more to come!)
  * schema refactoring (patch -> edit, offsets -> at)
  * Hackage metadata improvements (initial release was a bit wonky)

## 0.2.0 (2021-12-03)
Initial release.

  * extracted & rewrote tool (library + CLI) from gtvm-hs
