import ../variant
import typeids/[first, second, shared]

type SharedAlias = Shared

# Neither sibling imports the other: replaying a shared counter cannot
# coordinate their allocations, even when IC builds them one at a time.
doAssert first.sameNameId != second.sameNameId,
  "sibling modules reused type ID " & $first.sameNameId
doAssert first.errorId != second.boolId
doAssert first.sharedId == second.sharedId
doAssert first.sharedId == getTypeId(SharedAlias)
doAssert first.sequenceId == second.sequenceId
doAssert first.sequenceId == getTypeId(seq[int])
doAssert getTypeId(seq[int]) != getTypeId(seq[string])
doAssert first.errorId != 0

let error = first.packed()
let success = second.packed()
doAssert error.ofType(first.Error)
doAssert not error.ofType(bool)
doAssert not success.ofType(first.Error)
doAssert success.ofType(bool)
doAssert error.get(first.Error).code == 42
doAssert success.get(bool)

type
  FirstDistinct = distinct int
  SecondDistinct = distinct int
  Box[T] = object
    value: T

doAssert getTypeId(FirstDistinct) != getTypeId(int)
doAssert getTypeId(FirstDistinct) != getTypeId(SecondDistinct)
doAssert getTypeId(seq[FirstDistinct]) != getTypeId(seq[SecondDistinct])
doAssert getTypeId(Box[FirstDistinct]) != getTypeId(Box[SecondDistinct])

let callable = newVariant(
  proc(x: int): int =
    x + 1
)
variantMatch case callable as fn
of (proc(y: int): int):
  doAssert fn(41) == 42
else:
  doAssert false

doAssert first.genericDistinctId == second.genericDistinctId
echo "type IDs and variants are consistent"
