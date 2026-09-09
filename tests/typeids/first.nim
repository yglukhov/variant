import ../../variant
import shared

type
  SameName* = object
    value*: int

  Error* = ref object
    code*: int

const
  sameNameId* = getTypeId(SameName)
  sharedId* = getTypeId(Shared)
  sequenceId* = getTypeId(seq[int])
  errorId* = getTypeId(Error)

proc packed*(): Variant =
  newVariant(Error(code: 42))

const genericDistinctId* = getTypeId(DistinctBox[int])
