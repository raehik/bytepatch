# bytepatch
A Haskell library and CLI tool for patching byte-representable data in a
bytestream. Write **patchscripts** (in-place edits to a file) using a convenient
YAML format, and apply them with a safe patching algorithm. You choose how
explicit to be: edits may be written as a simple `(offset, content)` tuple, or
you can provide metadata regarding expected original data, so you know you're
patching the right file.

Intended as a general tool for making simple binary editing more manageable.

## What?
If you're modifying binaries, you often end up needing to make edits in a hex
editor. This is fun for a very short while, then you realise how easy it is to
mess up. bytepatch provides a simple, human read-writeable format for defining
such edits, and can apply them for you.

bytepatch is intended as a developer tool for writing a *patchscript*. It's not
a binary patch tool like IPS, BPS: these take an input and a result file, and
generate a patch file that can be applied to the input in order to recreate the
result. By itself, the patch file isn't useful, being instructions to the tool
telling how to edit the input file. bytepatch uses a human-readable patch file
format that describes in plain terms the edits to make, so developers can read
and write patches in a structured, useable format.

## When might I want to use this?
You might find bytepatch useful if you want to define edits to be made on binary
files (especially executables) that are *static* and *in-place*.

### Less relevant use cases
You can't use bytepatch directly for edits which are dynamic in nature (e.g.
you need to read a file header in order to know what to change). *(You could
have a patch preparer that does the dynamic work and emits a bytepatch patch,
but this tool is largely focused the patch representation, so you wouldn't be
gaining much.)*

You can't use bytepatch to make edits that aren't in-place. Edits are located
using a byte offset. Allowing edits to change the length of the segment they
replace would introduce issues for following edits: do we use the original
offset, or do we implicitly shift them so they still write to the "same place"?
It may be possible to design a convenient format with useful behaviour. I'd like
to look into it at some point.

### Original rationale
I'm working on some binary files, and need to patch strings, machine code and
other data. I make some changes, I test them, they work. Then I note down the
changes I made. It's tedious and repetitive, but I really don't want to lose
track of what got changed how, and it's the clearest way to share my work
others. But if I want to test more stuff, I may have to make those changes
again. I'm gonna make a mistake eventually.

Instead, I described my patches in YAML, and wrote a tool to apply them. Now my
changes and my notes are the same file, which can be checked for correctness and
applied automatically.

## License
Provided under the MIT license. Please see `LICENSE` for the full license text.