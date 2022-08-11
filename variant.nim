import macros, tables, hashes, strutils

var typeIds {.compileTime.} = initTable[int, string]()

proc mangledNameAux(t: NimNode): string =
    case t.typeKind
    of ntyAlias:
        assert(t.kind == nnkSym)
        let impl = t.symbol.getImpl()
        assert(impl.kind == nnkTypeDef)
        result = mangledNameAux(impl[^1])
    of ntyBool, ntyChar, ntyString, ntyCString,
            ntyInt, ntyInt8, ntyInt16, ntyInt32, ntyInt64,
            ntyFloat32, ntyFloat128,
            ntyUInt, ntyUInt8, ntyUInt16, ntyUInt32, ntyUInt64:
        result = $t
    of ntyFloat64, ntyFloat:
        result = "float"
    of ntyObject:
        result = "object["
        result &= t.getTypeInst.repr
        result &= "]"

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

        result = "distinct[" & mangledNameAux(impl[^1]) & "]"

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
        # TODO: Uncomment the following line when nim 0.19 is out,
        # nnkTupleConstr is not defined in older nim.
        # impl.expectKind({nnkTupleTy, nnkPar, nnkTupleConstr})

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
        
    of ntyNone:
        result = "none"

    else:
        echo "kind: ", t.typeKind
        echo "t: ", treeRepr(t)
        echo "getTypeInst: ", treeRepr(t.getTypeInst())
        echo "getTypeImpl: ", treeRepr(t.getTypeImpl())
        assert(false)


proc mangledName(t: NimNode): string =
    mangledNameAux(getTypeImpl(t)[1])

macro getMangledName(t: typed): string =
  result = newLit(mangledName(t))

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
    result = newLit(h)

const debugVariantTypes = defined(variantDebugTypes)

proc canCastToPointer[T](): bool {.compileTime.} =
    when compiles(static(sizeof(T))):
        return sizeof(T) <= sizeof(pointer) and compiles(cast[pointer](default(T)))
    else:
        return false

when defined(gcDestructors):
    type
        Variant* {.inheritable.} = ref object
            typeId*: TypeId
            when debugVariantTypes:
                mangledName*: string
        VariantConcrete[T] = ref object of Variant
            val: T
else:
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
    when defined(gcDestructors):
        result = VariantConcrete[T](val: val)
    elif defined(js):
        var valCopy = val
        {.emit: "`result`.refval = `valCopy`;".}
    else:
        when T is proc {.closure.}:
            let pt = T.new()
            pt[] = val
            result = Variant(isRef: true, refval: cast[ref RootObj](pt))
        elif T is (proc):
            result = Variant(isRef: false, val: cast[pointer](val))
        elif T is ref:
            # T is already a ref, so just store it as is
            result = Variant(isRef: true, refval: cast[ref RootObj](val))
        elif canCastToPointer[T]():
            # T is good enough to be stored inside a pointer value. E.g.: ints, floats, enums, etc.
            result = Variant(isRef: false, val: cast[pointer](val))
        else:
            let pt = T.new()
            pt[] = val
            result = Variant(isRef: true, refval: cast[ref RootObj](pt))

    result.typeId = getTypeId(T)
    when debugVariantTypes:
        result.mangledName = getMangledName(T)

proc get*(v: Variant, T: typedesc): T =
    when defined(gcDestructors):
        if v.isNil:
            raise newException(Exception, "Wrong variant type: " & "nil" & ". Expected type: " & getMangledName(T))

    if getTypeId(T) != v.typeId:
        when debugVariantTypes:
            raise newException(Exception, "Wrong variant type: " & v.mangledName & ". Expected type: " & getMangledName(T))
        else:
            raise newException(Exception, "Wrong variant type. Compile with -d:variantDebugTypes switch to get more type information.")
    when defined(gcDestructors):
        result = VariantConcrete[T](v).val
    elif defined(js):
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

template isEmpty*(v: Variant): bool =
    when defined(gcDestructors):
        v.isNil
    else:
        v.typeId == 0

template getTn(v: Variant): TypeId =
    when defined(gcDestructors):
        if v.isNil:
            TypeId(0)
        else:
            v.typeId
    else:
        v.typeId

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
    import unittest

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
    # float32 should be castable to pointer
    const ftop = canCastToPointer[float32]()
    doAssert(ftop)

    suite "Variant": # Test mangling
        test "Mangling":
            check getMangledName(int) == "int"
            check getMangledName(DistinctInt) == "distinct[int]"
            check getMangledName(DistinctInt2) == "distinct[int]"
            check getMangledName(float) == "float"
            check getMangledName(seq[int]) == "seq[int]"
            check getMangledName(SeqOfInt) == "seq[int]"
            check getMangledName(ptr int) == "ptr[int]"
            check getMangledName(IntPtr) == "distinct[ptr[int]]"
            check getMangledName(IntPtr2) == getMangledName(IntPtr)
            check getMangledName(GenericTest[float]) == "seq[float]"
            check getMangledName(ConcreteTest) == "seq[int]"
            check getMangledName(tuple[x, y: int]) == "tuple[int,int]"
            check getMangledName(tuple[x: int, y: float]) == "tuple[int,float]"
            check getMangledName(Obj) == "object[Obj]"
            check getMangledName(RefObj) == "ref[object[RefObj:ObjectType]]"
            check getMangledName(array[3, float]) == "array[0..2,float]"
            check getMangledName(array[0..2, float]) == "array[0..2,float]"
            check getMangledName(GenericTupleWithClosures[int]) == "tuple[proc[void,int],proc[int]]"

            check getMangledName(SomeEnum) == "enum[SomeEnum]"
            check getMangledName(set[SomeEnum]) == "set[enum[SomeEnum]]"

            check getMangledName(ParTuple) == "tuple[int,float]"


        test "Variant":
            var v = newVariant(5)
            check v.ofType(int)
            check v.get(int) == 5
            when debugVariantTypes:
                check v.mangledName == "int"
            v = newVariant(3.0)
            check v.ofType(float)
            check v.get(float) == 3.0
            when debugVariantTypes:
                check v.mangledName == "float"
            v = newVariant(@[1, 2, 3])
            check v.ofType(seq[int])
            check v.get(seq[int])[1] == 2
            when debugVariantTypes:
                check v.mangledName == "seq[int]"

            v = newVariant(RefObj.new())
            when debugVariantTypes:
                check v.mangledName == getMangledName(RefObj)

        test "Match":
            var v = newVariant(@[1, 2, 3])
            check v.ofType(seq[int])
            variantMatch case v:
                of int as i: check(false and i == 0)
                of seq[int] as s: check s[1] == 2
                else: check false

            variantMatch case v as u
            of int: check(false and u == 0)
            of seq[int]: check(u[1] == 2)
            else: fail()

            v = newVariant(5.3)
            check v.ofType(float)
            variantMatch case v:
                of int as i: check(false and i == 0)
                of float as f: check f == 5.3
                else: fail()

        test "Generic types":
            type SomeGeneric[T] = tuple[a: T]
            var sng : SomeGeneric[int]
            sng.a = 5
            let v = newVariant(sng)
            check(v.get(type(sng)).a == 5)

        test "Closures":
            proc foon(b: int): int = b + 5
            proc fooc(b: int): int {.closure.} = b + 6
            var v = newVariant(foon)
            check(v.get(proc(b: int): int)(6) == 11)
            v = newVariant(fooc)
            check(v.get(proc(b: int): int)(6) == 12)

        test "Char":
            let v = newVariant('a')
            check(v.get(char) == 'a')

