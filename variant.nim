import macros
import tables
import hashes
import strutils

var typeIds {.compileTime.} = initTable[int, string]()

proc mangledNameAux(t: NimNode): string =
    case t.typeKind
    of ntyAlias:
        let impl = t.getTypeImpl()
        if impl.kind == nnkSym:
            result = mangledNameAux(impl)
        else:
            # Old Nim support. May be removed eventually.
            assert impl.kind == nnkBracketExpr
            assert impl.len == 1
            result = mangledNameAux(impl[^1])
    of ntyBool, ntyChar, ntyString, ntyCString,
            ntyInt, ntyInt8, ntyInt16, ntyInt32, ntyInt64,
            ntyFloat32, ntyFloat128,
            ntyUInt, ntyUInt8, ntyUInt16, ntyUInt32, ntyUInt64:
        result = $t
    of ntyFloat64, ntyFloat:
        result = "float"
    of ntyObject:
        assert(t.kind == nnkSym)
        result = "object[" & $t & "]"
    of ntyRef:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkRefTy
        assert impl.len == 1
        result = "ref[" & mangledNameAux(impl[^1]) & "]"
    of ntyPtr:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkPtrTy
        assert impl.len == 1
        result = "ptr[" & mangledNameAux(impl[^1]) & "]"
    of ntyDistinct:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkDistinctTy
        assert impl.len == 1

        assert t.kind == nnkSym

        result = "distinct[" & $t & ":" & mangledNameAux(impl[^1]) & "]"
    of ntySequence:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkBracketExpr
        assert impl.len == 2
        result = "seq[" & mangledNameAux(impl[^1]) & "]"

    of ntySet:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkBracketExpr
        assert impl.len == 2
        result = "set[" & mangledNameAux(impl[^1]) & "]"

    of ntyArray:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkBracketExpr
        assert impl.len == 3
        let rng = impl[1]
        result = "array[" & $rng[1].intVal & ".." & $rng[2].intVal & "," & mangledNameAux(impl[^1]) & "]"
    of ntyGenericInst:
        # TODO: Figure out how to get rid of `getType`. Related nim issue: #5788
        result = mangledNameAux(getType(t))

    of ntyTuple:
        let impl = t.getTypeImpl()
        impl.expectKind({nnkTupleTy, nnkPar})

        result = "tuple["
        var i = 0
        for identDefs in impl:
            case identDefs.kind
            of nnkIdentDefs:
                let typ = mangledNameAux(identDefs[^2])
                for j in 0 ..< identDefs.len - 2:
                    if i > 0: result &= ","
                    result &= typ
                    inc i
            else:
                if i > 0: result &= ","
                result &= mangledNameAux(identDefs)
                inc i

        result &= "]"

    of ntyProc:
        let impl = t.getTypeImpl()
        assert impl.kind == nnkProcTy

        let params = impl[0]

        result = "proc["
        if params[0].kind == nnkEmpty:
            result &= "void"
        else:
            result &= mangledNameAux(params[0])

        for p in 1 ..< params.len:
            let typ = mangledNameAux(params[p][^2])
            for j in 0 ..< params[p].len - 2:
                result &= ","
                result &= typ
        result &= "]"
    of ntyEnum:
        let inst = t.getTypeInst()
        assert inst.kind == nnkSym
        result = "enum[" & $inst & "]"

    else:
        echo "kind: ", t.typeKind
        echo "t: ", treeRepr(t)
        echo "getTypeInst: ", treeRepr(t.getTypeInst())
        echo "getTypeImpl: ", treeRepr(t.getTypeImpl())
        assert(false)


proc mangledName(t: NimNode): string =
    mangledNameAux(getTypeImpl(t)[1])

macro getMangledName(t: typed): string = mangledName(t)

type TypeId* = Hash

macro getTypeId*(t: typed): TypeId =
    let name = mangledName(t)
    var h = hash(name) mod 2147483645
    while true:
        if h in typeIds:
            if typeIds[h] == name: break
            h = (h *% 2) mod 2147483645
        else:
            typeIds[h] = name
            break
    return h

const debugVariantTypes = defined(variantDebugTypes)

proc canCastToPointer[T](): bool {.compileTime.} =
    when compiles(
        proc() =
            const s = sizeof(T)):
        return sizeof(T) <= sizeof(pointer) and compiles(
            proc() =
                var val: T
                discard cast[pointer](val))
    else:
        return false

type Variant* = object
    typeId*: TypeId
    when defined(js):
        refval {.exportc.}: ref RootObj
    else:
        case isRef: bool
        of true:
            refval: ref RootObj
        of false:
            val: pointer
    when debugVariantTypes:
        mangledName*: string

template ofType*(v: Variant, t: typedesc): bool = v.typeId == getTypeId(t)

proc newVariant*(): Variant = discard

proc castFromPointer[T](p: pointer): T {.inline.} =
    type Conv {.union.} = object
        p: pointer
        v: T
    var v: Conv
    v.p = p
    return v.v

proc newVariant*[T](val: T): Variant =
    result.typeId = getTypeId(T)
    when debugVariantTypes:
        result.mangledName = getMangledName(T)
    when defined(js):
        var valCopy = val
        {.emit: "`result`.refval = `valCopy`;".}
    else:
        when T is proc {.closure.}:
            let pt = T.new()
            pt[] = val
            result.isRef = true
            result.refval = cast[ref RootObj](pt)
        elif T is (proc):
            esult.isRef = false
            result.val = cast[pointer](val)
        elif T is ref:
            # T is already a ref, so just store it as is
            result.isRef = true
            result.refval = cast[ref RootObj](val)
        elif canCastToPointer[T]():
            # T is good enough to be stored inside a pointer value. E.g.: ints, floats, enums, etc.
            result.isRef = false
            result.val = cast[pointer](val)
        else:
            let pt = T.new()
            pt[] = val
            result.isRef = true
            result.refval = cast[ref RootObj](pt)

proc get*(v: Variant, T: typedesc): T =
    if getTypeId(T) != v.typeId:
        when debugVariantTypes:
            raise newException(Exception, "Wrong variant type: " & v.mangledName & ". Expected type: " & getMangledName(T))
        else:
            raise newException(Exception, "Wrong variant type. Compile with -d:variantDebugTypes switch to get more type information.")
    when defined(js):
        {.emit: "`result` = `v`.refval;".}
    else:
        when T is proc {.closure.}:
            if v.isRef:
                result = cast[ref T](v.refval)[]
            else:
                let p = v.val
                {.emit: """
                *(void**)(&`result`->ClP_0) = `p`;
                """.}
        elif T is (proc):
            result = cast[T](v.val)
        elif T is ref:
            # T is already a ref, so just store it as is
            result = cast[T](v.refval)
        elif canCastToPointer[T]():
            result = castFromPointer[T](v.val)
        else:
            result = cast[ref T](v.refval)[]

proc getProc*(v: Variant, T: typedesc[proc]): T {.deprecated, inline.} =
    ## Same as `get` but designed for proc types to better handle
    ## closure vs non-closure interop. Still not fully implemented.
    v.get(T)

template isEmpty*(v: Variant): bool = v.typeId == 0

template getTn(v: Variant): TypeId = v.typeId

macro variantMatch*(body: untyped): untyped =
    expectKind(body, nnkCaseStmt)
    var defaultUnpackSym : NimNode
    var variantNode = body[0]
    if body[0].kind == nnkInfix and $body[0][0] == "as":
        variantNode = body[0][1]
        defaultUnpackSym = body[0][2]

    result = newNimNode(nnkCaseStmt)
    result.add(newCall(bindSym "getTn", variantNode))

    for i in 1 ..< body.len:
        let c = body[i]
        case c.kind
        of nnkOfBranch:
            expectLen(c, 2)
            var unpackSym = defaultUnpackSym
            var typeNode = c[0]
            if c[0].kind == nnkInfix and $c[0][0] == "as":
                typeNode = c[0][1]
                unpackSym = c[0][2]
            expectKind(unpackSym, nnkIdent)

            result.add(newNimNode(nnkOfBranch).add(
                newCall(bindSym "getTypeId", typeNode),
                newStmtList(
                    newLetStmt(unpackSym, newCall(bindSym "get", variantNode, typeNode)),
                    c[1]))
                )
        of nnkElse:
            result.add(c)
        else:
            error "Unexpected node type in variant case: " & c.lineinfo

when isMainModule:
    type Obj = object
        a: int

    type RefObj = ref object
        a: int

    type DistinctInt = distinct int
    type DistinctInt2 = distinct int

    type SeqOfInt = seq[int]
    type IntPtr = distinct ptr int
    type IntPtr2 = IntPtr
    type GenericTest[T] = seq[T]
    type ConcreteTest = GenericTest[int]

    type GenericTupleWithClosures[T] = tuple[setter: proc(v: T), getter: proc(): T]

    type SomeEnum* = enum
        someVal1
        someVal2

    type ParTuple = (int, float)

    # Int should be castable to pointer
    const itop = canCastToPointer[int]()
    doAssert(itop)
    # Float should be castable to pointer
    const ftop = canCastToPointer[int]()
    doAssert(ftop)

    block: # Test mangling
        doAssert getMangledName(int) == "int"
        doAssert getMangledName(DistinctInt) == "distinct[DistinctInt:int]"
        doAssert getMangledName(DistinctInt2) == "distinct[DistinctInt2:int]"
        doAssert getMangledName(float) == "float"
        doAssert getMangledName(seq[int]) == "seq[int]"
        doAssert getMangledName(SeqOfInt) == "seq[int]"
        doAssert getMangledName(ptr int) == "ptr[int]"
        doAssert getMangledName(IntPtr) == "distinct[IntPtr:ptr[int]]"
        doAssert getMangledName(IntPtr2) == getMangledName(IntPtr)
        doAssert getMangledName(GenericTest[float]) == "seq[float]"
        doAssert getMangledName(ConcreteTest) == "seq[int]"
        doAssert getMangledName(tuple[x, y: int]) == "tuple[int,int]"
        doAssert getMangledName(tuple[x: int, y: float]) == "tuple[int,float]"
        doAssert getMangledName(Obj) == "object[Obj]"
        doAssert getMangledName(RefObj) == "ref[object[RefObj:ObjectType]]"
        doAssert getMangledName(array[3, float]) == "array[0..2,float]"
        doAssert getMangledName(array[0..2, float]) == "array[0..2,float]"
        doAssert getMangledName(GenericTupleWithClosures[int]) == "tuple[proc[void,int],proc[int]]"

        doAssert getMangledName(SomeEnum) == "enum[SomeEnum]"
        doAssert getMangledName(set[SomeEnum]) == "set[enum[SomeEnum]]"

        doAssert getMangledName(ParTuple) == "tuple[int,float]"


    block: # Test variant
        var v = newVariant(5)
        doAssert v.ofType(int)
        doAssert v.get(int) == 5
        when debugVariantTypes:
            doAssert v.mangledName == "int"
        v = newVariant(3.0)
        doAssert v.ofType(float)
        doAssert v.get(float) == 3.0
        when debugVariantTypes:
            doAssert v.mangledName == "float"
        v = newVariant(@[1, 2, 3])
        doAssert v.ofType(seq[int])
        doAssert v.get(seq[int])[1] == 2
        when debugVariantTypes:
            doAssert v.mangledName == "seq[int]"

        v = newVariant(RefObj.new())
        when debugVariantTypes:
            doAssert v.mangledName == getMangledName(RefObj)

    block: # Test match
        var v = newVariant(@[1, 2, 3])
        doAssert v.ofType(seq[int])
        variantMatch case v:
            of int as i: doAssert(false and i == 0)
            of seq[int] as s: doAssert s[1] == 2
            else: doAssert false

        variantMatch case v as u
        of int: doAssert(false and u == 0)
        of seq[int]: doAssert(u[1] == 2)
        else: doAssert false

        v = newVariant(5.3)
        doAssert v.ofType(float)
        variantMatch case v:
            of int as i: doAssert(false and i == 0)
            of float as f: doAssert f == 5.3
            else: doAssert false

    block: # Test gneric types
        type SomeGeneric[T] = tuple[a: T]
        var sng : SomeGeneric[int]
        sng.a = 5
        let v = newVariant(sng)
        doAssert(v.get(type(sng)).a == 5)

    block: # Test closures
        proc foon(b: int): int = b + 5
        proc fooc(b: int): int {.closure.} = b + 6
        var v = newVariant(foon)
        doAssert(v.get(proc(b: int): int)(6) == 11)
        v = newVariant(fooc)
        doAssert(v.get(proc(b: int): int)(6) == 12)

    block: # Test char
        let v = newVariant('a')
        doAssert(v.get(char) == 'a')
