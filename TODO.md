## Parsing
* There's no need to parse things like `HexByteString` directly, because
  `parseJSON` works on `Value`s. Parsing `Text` and transforming afterwards is
  identical in terms of performance. That seems useful, because bytepatch data
  types will probably always be `Text`-based.
  * Ack, but the check will probably be fairly dynamic. Also, delaying the maps
    to the patch application point can let us do nice errors easily.

## Transforming patchscripts
### Transform equality checks to digests
This is an excellent idea. One thing: the data we check is dependent on the
length of the patch data. This is a simple convenience that allows changing
patch data (specifically, its length) without necessarily having to change the
check data.

This doesn't work when we're checking digests. If we check less than the
hashed data, it will fail.

Ah, to be clear, this is only true when we're permitting prefix equality. Which
I think I usually was.

An interesting way to potentially fix this is to state a check length for each
seek. Not a max length (though `check length <= max length`). Then instead of
grabbing the length of the patch data, we grab that.

Uhhh, hold on. That's what `null_terminates` is. It's the point where the seek
data ends.

I think I want a set up where you can state check length, or omit it and it's
automated. For equality checks, take the length of given seek data (and always
be permitted prefix equality). For digest checks, take the length of the patch
data.
