import macros
import tables
import hashes
import strutils

var typeIds {.compileTime.} = initTable[int, string]()

template push(s, v: NimNode) = s.add(v)

proc pop(s: NimNode): NimNode {.discardable.} =
    result = s[^1]
    s.del(s.len - 1)

proc getLastSym(s: NimNode): NimNode =
    var i = s.len - 1
    while i >= 0:
        if s[i].kind == nnkSym: return s[i]
        dec i

proc mangledName(t: NimNode, parents: NimNode): string =
    case t.kind
    of nnkBracketExpr:
        case $t[0]
        of "seq", "ref", "ptr", "tuple", "array", "proc", "set":
            result = $t[0] & "["
            for i in 1 ..< t.len:
                parents.push(t[i])
                if i != 1: result &= ","
                result &= mangledName(getType(t[i]), parents)
                parents.pop()
            result &= "]"
        of "distinct":
            parents.push(t[1])
            result = "distinct[" & mangledName(getType(t[1]), parents) & ":" & t.lineinfo & "]"
            parents.pop()
        of "range":
            result = "range[" & $t[1].intVal & "," & $t[2].intVal & "]"
        else:
            echo treeRepr(t)
            assert(false)
    of nnkSym:
        let tt = getType(t)
        if tt.kind == nnkSym and $tt == $t:
            result = $t
        else:
            parents.push(tt)
            result = mangledName(tt, parents)
            parents.pop()
    of nnkObjectTy:
        let ls = getLastSym(parents)
        if ls.isNil:
            echo "NO PARENT SYM!"
            echo "STACK: ", treeRepr(parents)
        result = "object[" & $ls & "]"
    of nnkEnumTy:
        result = "enum["
        let ty = t[0]
        for i in 1 ..< t.len:
            if i != 1: result &= ","
            result &= $t[i]
        result &= "]"
    else:
        parents.push(t)
        result = mangledName(getType(t), parents)
        parents.pop()

proc mangledName(t: NimNode): string =
    var parents = newStmtList(t)
    mangledName(getType(t)[1], parents)

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

macro variantMatch*(body: untyped): stmt =
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

macro match*(v: Variant, body: untyped): stmt {.deprecated.} =
    result = newNimNode(nnkCaseStmt)
    result.add(newCall(bindSym "getTn", v))
    for c in body:
        if c.kind == nnkCommand:
            result.add(newNimNode(nnkOfBranch).add(newCall(bindSym "getTypeId", c[1][1]),
                newStmtList(
                    newLetStmt(c[1][2], newCall(bindSym "get", v, c[1][1])),
                    c[2]))
                )
        elif c.kind == nnkCall:
            result.add(newNimNode(nnkElse).add(c[1]))

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

    type SomeEnum = enum
        someVal1
        someVal2

    # Int should be castable to pointer
    const itop = canCastToPointer[int]()
    doAssert(itop)
    # Float should be castable to pointer
    const ftop = canCastToPointer[int]()
    doAssert(ftop)

    block: # Test mangling
        doAssert getMangledName(int) == "int"
        doAssert getMangledName(DistinctInt).startsWith("distinct[int:")
        doAssert getMangledName(DistinctInt2).startsWith("distinct[int:")
        #doAssert getMangledName(DistinctInt) != getMangledName(DistinctInt2) # Depends on Nim pr 3667
        doAssert getMangledName(float) == "float"
        doAssert getMangledName(seq[int]) == "seq[int]"
        doAssert getMangledName(SeqOfInt) == "seq[int]"
        doAssert getMangledName(ptr int) == "ptr[int]"
        doAssert getMangledName(IntPtr).startsWith("distinct[ptr[int]:")
        doAssert getMangledName(IntPtr2) == getMangledName(IntPtr)
        doAssert getMangledName(GenericTest[float]) == "seq[float]"
        doAssert getMangledName(ConcreteTest) == "seq[int]"
        doAssert getMangledName(tuple[x, y: int]) == "tuple[int,int]"
        doAssert getMangledName(tuple[x: int, y: float]) == "tuple[int,float]"
        doAssert getMangledName(Obj) == "object[Obj]"
        doAssert getMangledName(RefObj) == "ref[object[RefObj:ObjectType]]"
        doAssert getMangledName(array[3, float]) == "array[range[0,2],float]"
        doAssert getMangledName(array[0..2, float]) == "array[range[0,2],float]"
        doAssert getMangledName(GenericTupleWithClosures[int]) == "tuple[proc[void,int],proc[int]]"

        doAssert getMangledName(SomeEnum) == "enum[someVal1,someVal2]"
        doAssert getMangledName(set[SomeEnum]) == "set[enum[someVal1,someVal2]]"

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
