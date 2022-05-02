# Using bytepatch
*This is a shoddy tutorial because bytepatch's UI is a bit too overwhelming at
the moment. Sorry.*

bytepatch enables you to define simple patches over streams using a declarative
YAML syntax. Let's say you have the given file

```
00000000  53 74 72 69 6e 67 20 31  00 00 00 00 00 00 00 00  |String 1........|
00000010  41 6e 64 20 73 74 72 69  6e 67 20 32 00 00 00 00  |And string 2....|
```

You want to replace the second string `And string 2` with `REPLACED`.
Rather than opening up your hex editor and carefully making the change, you
instead write a bytepatch patchscript:

```yaml
- data:      REPLACED
  at:
  - seek: 0x10
    compare: And string 2
```

Then apply it with `bytepatch --type text-bin,utf8,c --compare prefix --seek
abs patchscript.yaml patch strings.bin -o strings-patched.bin`:

```
00000000  53 74 72 69 6e 67 20 31  00 00 00 00 00 00 00 00  |String 1........|
00000010  52 45 50 4c 41 43 45 44  00 67 20 32 00 00 00 00  |REPLACED.g 2....|
```

bytepatch will encode your string as UTF-8 (which does nothing for ASCII),
null-terminate, and write over the original string.

The `compare` field allows for a convenient sanity check: if present, bytepatch
will also compare the actual string present to the one given, and fail if they
don't match. (Note that you can provide different comparison strategies to
bytepatch.)

What if you tried to patch over something with a larger string?

```yaml
- data:      REPLACED LONG
  at:
  - seek: 0x10
    compare: And string 2
```

Here, bytepatch would give a comparison error. The "actual string" compared is
retrieved by grabbing the same number of bytes as the replacement string. In
this case, the actual string would have a bunch of null bytes after it.
(regardless of if you did exact or
prefix equality).
