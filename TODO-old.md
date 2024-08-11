## Make aligning much smaller
Aligning then linearizing does unnecessary work. If you linearize first, you
only need to align the first patch. Rejig things to use this: if we're
linearizing, do it first. Still need to check all the expected align values
though lol.

## Simplify HFunctorList stuff
Can't I just give in and go to a regular heterogeneous list? Simplifies a lot.
Means I give up the `Functor` instance. But I'm really only using it for
`Compare`. It's feels nice to thread the type variable through, but it's not
perfect, because we can't do the "expected" thing per layer (because things
don't compose that way).

I don't know how I feel about this either way.

## Compare code is real shit
Needs redesign.

## Extensions
### CLI
#### Generic option parser helper
Should sketch elsewhere. Would be super useful for this. Like parsing
`text-bin:utf8,c` to `CDataTextBin BR.Text.Encoding BR.ByteString.Rep`, and
handling inner records like `text-bin:ascii,(pascal:4,le)`. Dunno though.

### Internal
  * Move stream-time binary meta to prep-time. Can't I just extend the compare
    value with the nulls? It would break equality comparisons, but they're silly
    anyway for null-terminated strings. So it'd be a "good" breakage.
    * Also, I should definitely change to "succeeding nulls" rather than "null
      terminates at index". Safer, more obvious
  * Clean up Vinyl usage. They have Unicode shorthands for common type-level
    propositions (constraints). e.g. `type (∈) r rs = RElem r rs (RIndex r rs)`

### Usability
  * default schema options: no align, type=text-bin:utf8:c, compare=eq:prefix
  * explain relationship between compare, type: some pairings are a bit
    meaningless
    * key point: compare data is obtained by checking the length of the patch
      data, and grabbing the length from the stream stream. thus, changing the
      length of your patch data changes how the comparison takes 
    * `compare:hash=FUNC` usually means you're doing doing static byte patches
    * also needs work on internals

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
