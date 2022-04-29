## aeson instances for `Patch`
I had great initial success via aeson's & vinyl's generics!! But not enough:

  * A type-level list means the list order is crucial, but that's not really
    convenient for a user. Better would be `patch[0].meta.compare` etc.
  * A small issue: it prints `- []` at the end. The empty type list is handled
    fine, where it's just the empty list.

Nice idea, but I think it's best to leave it. A simple exchange type means I get
to design the internals one way, and the user interface another. More code, but
more consistency.

This would be worth considering again if:

  * I refactor to use a type-level map rather than a list, meaning we locate
    things via a user-specified name rather than a type. This is more overhead
    for the programmer, but should simplify some bits -- we should be able to
    stop caring about type-level list order on the term level.
  * And I consider extending seeks. For example, a seek storing line number &
    column could be used to patch a text file. This would mean upending much of
    the internals!

## Other
Improvements:

  * Combined tests
  * QuickCheck tests (a pain...)
  * Errors via union types. Because my types are so good now, we can delimit
    what sort of errors are possible for different usages.

Partly related:

  * Add `patch-dir` command that concatenates all `.bin.yaml` in a directory
    (recursively) and applies, then for `.text.yaml`. So no pain over patch
    ordering!
  * Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere
