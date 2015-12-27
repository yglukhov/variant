# variant [![nimble](https://raw.githubusercontent.com/yglukhov/nimble-tag/master/nimble_js.png)](https://github.com/yglukhov/nimble-tag)
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
match v:
    like int as i:
        echo "v is int: ", i
    like seq[int] as s:
        echo "v is seq[int]: ", s
    like:
        echo "dont know what v is"
```
Will output:
```
v is seq[int]: @[1, 2, 3]
```
