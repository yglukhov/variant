import ../../variant
import shared

type SameName* = object
  value*: string

const
  sameNameId* = getTypeId(SameName)
  sharedId* = getTypeId(Shared)
  sequenceId* = getTypeId(seq[int])
  boolId* = getTypeId(bool)

proc packed*(): Variant =
  newVariant(true)

const genericDistinctId* = getTypeId(DistinctBox[int])
