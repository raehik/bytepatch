# bytepatch
A Haskell library and CLI tool for writing declarative patches over streams.
Write **patchscripts** (in-place edits to a file) using a configurable YAML
schema, and apply them with a safe patching algorithm. On the library side,
define patches of various forms over any type, apply them to pure/impure
streams, and write further patch data types and streams to extend as you like.

bytepatch works with many data types:

  * read pretty binary `00 FF 1234`, parse to bytes
  * read text, re-encode, null-terminate to generate a C-style string
  * read assembly, assemble down to machine code

The schema itself is highly configurable:

  * align: add a value (set in patch file) to each patch offset
  * compare: bytepatch can check expected data against actual during patching,
    to assert that you're patching the expected file. You can select how this
    check works: compare equality (exact, or match prefix), hashes
  * seek type: what the patch offsets mean. You'll likely want absolute
    offsets - but you may also write them as linear offsets, forward from the
    previous.

You may also ask bytepatch to *compile* a patchscript, which reads it and then
outputs a YAML file with a simplified schema: pre-aligned, compare via hashes,
forward seeks.

The bytepatch executable aims to be a general tool for reverse engineers, hoping
to make simple binary & assembly editing more manageable. The Haskell library is
quite sprawling, and has personally made for a fun investigation into type-level
Haskell.

## What?
If you're modifying binaries, you often end up needing to make edits in a hex
editor. This is fun for a very short while, then you realise how easy it is to
mess up.

bytepatch is primarily intended as a developer tool for writing a
*human-friendly static patchscript*. It provides a nice schema for defining
edits to a binary file, so that developers can read and write patches in a
structured, readable format. If one wanted, they could very easily read the
patch file and use it to make the changes manually (though bytepatch wants to do
this for you).

bytepatch is not aimed at replacing binary patch tools such as IPS, BPS: these
take an input and a result file, and generate a patch file that can be applied
to the input in order to recreate the result. You need the result file in the
first place. These tools both generate the patch file and apply it; bytepatch
only applies the patch file, and leaves the writing to the user.

## When might I want to use this?
You might find bytepatch useful if you want to define edits to be made on binary
files (especially executables) that are *static* and *in-place*. Examples are:

  * instruction patching (via assembly)
  * string patching (via text)

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

## License
Provided under the MIT license. Please see `LICENSE` for the full license text.
