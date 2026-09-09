# variant [![CI](https://github.com/yglukhov/variant/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/yglukhov/variant/actions/workflows/test.yml) [![nimble](https://img.shields.io/badge/nimble-black?logo=nim&style=flat&labelColor=171921&color=%23f3d400)](https://nimble.directory/pkg/variant)
Variant type and type matching for Nim

```nim
import variant

var v = newVariant(5)
assert v.ofType(int)
assert v.get(int) == 5

v = newVariant(3.0)
assert v.ofType(float)
assert v.get(float) == 3.0

v = newVariant(@[1, 2, 3])
assert v.ofType(seq[int])
assert v.get(seq[int])[1] == 2
```

Matching:
```nim
var v = newVariant(@[1, 2, 3])
assert v.ofType(seq[int])
variantMatch case v as u
of int:
    echo "u is int: ", u
of seq[int]:
    echo "u is seq[int]: ", u
else:
    echo "dont know what v is"
```
Will output:
```
u is seq[int]: @[1, 2, 3]
```

Type IDs are opaque, nonzero values derived from type identity. They are not
sequential and should not be persisted across compiler versions or builds.
The macro cache records type identities so incremental compilation can reuse
modules without resetting a global type-ID counter.

To test cross-module type IDs with a compiler that supports `nim ic`, run:

```sh
nim ic -f -r tests/ttypeids.nim
nim ic -r tests/ttypeids.nim
```
