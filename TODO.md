  * [ ] Refactored codebase to focus more on arbitrary edits, which we then
        build on by saying "they're at an offset", "they're in-place". Should
        keep thinking about what I can/should model.
  * [ ] QuickCheck tests for LinearPatch (would be very fun!)

Partly related:

  * [ ] Add `patch-dir` command that concatenates all `.bin.yaml` in a directory
    (recursively) and applies, then for `.text.yaml`. So no pain over patch
    ordering!
  * [ ] Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere
  * [ ] csv-patch: add switch to discard safety info when generating patch
  * [ ] patching: newtype for display-as-hex ints?

Longform:

#### Generalize, generalize, generalize
I'm slowly approaching a general "patch a stream S of type T" patch algorithm.
It's worth investigating further, see what I can figure out. I feel like it
could be massaged into a useful library+CLI tool. It gives fun safety guarantees
and enables pure and impure patcher implementations.

I feel like the logical conclusion to the algorithm is something that allows
arbitrary forwards-only stream operations. So:

```haskell
type PatchScript a = [Op a]
data Op a
  = OpCopy Int
  | OpAdd a
  | OpDel Int
```

My current approach essentially combines all these into a single step, where
OpDel is the length of OpAdd (thus in-place). I could keep them squished into
one step for type-enforced normalization, just would need to add an `Int` for
`OpDel` (instead of using the length from `OpAdd`). Buuuut at the end of the
day, there's no further use for a data type like this. What use other than
binary-based replacements would actually be useful? It'd be nice to maximally
generalize the stream concept, but I sadly don't see it being useful -- at least
not yet.
