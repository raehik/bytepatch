Features:

  * Concat patches
    * only really for performance. cba because I can just pipe things
  * Process patches without applying
    * Especially, convert exacts to hashes!

Improvements:

  * Combined tests
  * QuickCheck tests (a pain...)

Thoughts:

  * Patch-time meta for checking equality, external to Binary. The Binary stuff
    is special because it can do prefix checks and check for expected spare
    nulls (useful for my needs). Prefix checks might be good to generalize.
    Though honestly, the point of this is to write a hyper generic base, and add
    binrep stuff on. The base without binrep remains rather useless.

Partly related:

  * Add `patch-dir` command that concatenates all `.bin.yaml` in a directory
    (recursively) and applies, then for `.text.yaml`. So no pain over patch
    ordering!
  * Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere
  * csv-patch: add switch to discard safety info when generating patch
  * patching: newtype for display-as-hex ints?
