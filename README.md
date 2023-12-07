# variant [![Build Status](https://github.com/yglukhov/variant/workflows/CI/badge.svg?branch=master)](https://github.com/yglukhov/variant/actions?query=branch%3Amaster) [![nimble](https://img.shields.io/badge/nimble-black?logo=nim&style=flat&labelColor=171921&color=%23f3d400)](https://nimble.directory/pkg/variant)
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
