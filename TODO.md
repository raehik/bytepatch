Refactoring to streampatch:

  * redo Linear. Don't try to recover, just `Either Error a` it. It should work
    on all types that have you can 'length' on.
  * consider splitting out some of the binary stuff from Apply? If possible.
    Though I'll still need stream patchers for binary. Maybe keep it with Binary
    modules instead. Binary.Patch, Binary.Apply? hmm.

Later extensions:

  * QuickCheck tests (would be very fun!)

Partly related:

  * Add `patch-dir` command that concatenates all `.bin.yaml` in a directory
    (recursively) and applies, then for `.text.yaml`. So no pain over patch
    ordering!
  * Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere
  * csv-patch: add switch to discard safety info when generating patch
  * patching: newtype for display-as-hex ints?
