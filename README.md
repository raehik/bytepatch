# bytepatch
A Haskell library and CLI tool for patching data in a stream. Write
**patchscripts** (in-place edits to a file) using a convenient YAML format, and
apply them with a safe patching algorithm. On the library side, define explicit
patches over any type, apply them to pure/impure streams, and write further
patch types and streams to extend as you like.

bytepatch can handle a few different types of data (see `bytepatch --help`),
including:

  * Binary via pretty hex bytes `00 FF 1234`
  * Binary via text, interpreted as null-terminated UTF-8 strings
  * Binary via ARMv8 Thumb (LE) assembly (more possible, ask raehik)

bytepatch is most useful when handling byte-representable data, but you can also
apply patches of a given type over a stream of the same type -- for example, to
patch a textual stream without considering the individual bytes, just the UTF-8
characters.

The executable is intended as a general tool for making simple binary/ASM
editing more manageable. Over time, I think it's evolved into an actually useful
little program. The codebase is really a fun investigation into some type-level
Haskell features. Regardless of which you're interested in, I hope you might
find some use from this project!

## What?
If you're modifying binaries, you often end up needing to make edits in a hex
editor. This is fun for a very short while, then you realise how easy it is to
mess up. bytepatch provides a simple, human read-writeable format for defining
such edits, and can apply them for you.

bytepatch is intended as a developer tool for writing a *static patchscript*.
It's not a binary patch tool like IPS, BPS: these take an input and a result
file, and generate a patch file that can be applied to the input in order to
recreate the result. By itself, the patch file isn't useful, being instructions
to the tool telling how to edit the input file. bytepatch uses a human-readable
patch file format that describes in plain terms the edits to make, so developers
can read and write patches in a structured, useable format.

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
using stream offsets. Allowing edits to change the length of the segment they
replace would introduce issues for following edits: do we use the original
offset, or do we implicitly shift them so they still write to the "same place"?
There *is* some support in the library for such edits, however, and it would be
interesting to explore.

### Original rationale
I'm working on some binary files, and need to patch strings, machine code and
other data. I make some changes, I test them, they work. Then I note down the
changes I made. It's tedious and repetitive, but I really don't want to lose
track of what got changed how, and it's the clearest way to share my work
others. But if I want to test more stuff, I may have to make those changes
again. I'm gonna make a mistake eventually.

Instead, I described my patches in YAML and wrote a tool to apply them. Now my
changes and my notes (thanks YAML comments!) are the same file, which can be
checked for correctness and applied automatically.

## License
Provided under the MIT license. Please see `LICENSE` for the full license text.
