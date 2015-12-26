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
        of "seq", "ref", "ptr", "distinct", "tuple", "array", "proc":
            result = $t[0] & "["
            for i in 1 ..< t.len:
                parents.push(t[i])
                if i != 1: result &= ","
                result &= mangledName(getType(t[i]), parents)
                parents.pop()
            result &= "]"
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
    else:
        parents.push(t)
        result = mangledName(getType(t), parents)
        parents.pop()

proc mangledName(t: NimNode): string =
    var parents = newStmtList(t)
    mangledName(getType(t)[1], parents)

macro getMangledName(t: typed): string = mangledName(t)

when defined(js):
    type TypeId* = int64
else:
    type TypeId* = int

macro getTypeId*(t: typed): TypeId =
    let name = mangledName(t)
    var h = hash(name)
    while true:
        if h in typeIds:
            if typeIds[h] == name: break
            h = h *% 2
        else:
            typeIds[h] = name
            break
    return h

const debugVariantTypes = defined(variantDebugTypes)

type Variant* = object
    tn: TypeId
    val {.exportc.}: pointer
    when debugVariantTypes:
        mangledName*: string

template getTn(v: Variant): TypeId = v.tn

template ofType*(v: Variant, t: typedesc): bool = v.tn == getTypeId(t)

proc needsCopy[T](): bool {.compileTime.} =
    when T is ref | ptr | SomeInteger:
        return false
    else:
        return true

proc newVariant*(): Variant = discard

proc newVariant*[T](val: T): Variant =
    result.tn = getTypeId(T)
    when debugVariantTypes:
        result.mangledName = getMangledName(T)
    when defined(js):
        {.emit: """
        `result`.val = `val`;
        """.}
    else:
        when needsCopy[T]():
            let pt = T.new()
            pt[] = val
            result.val = cast[pointer](pt)
        else:
            result.val = cast[pointer](val)

proc get*(v: Variant, T: typedesc): T =
    if getTypeId(T) != v.tn:
        when debugVariantTypes:
            raise newException(Exception, "Wrong variant type: " & v.mangledName & ". Expected type: " & getMangledName(T))
        else:
            raise newException(Exception, "Wrong variant type. Compile with -d:variantDebugTypes switch to get more type information.")
    when defined(js):
        {.emit: """
        `result` = `v`.val;
        """.}
    else:
        when needsCopy[T]():
            result = cast[ref T](v.val)[]
        else:
            result = cast[T](v.val)

template empty*(v: Variant): bool = v.tn != 0

macro match*(v: Variant, body: untyped): stmt =
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

    type SeqOfInt = seq[int]
    type IntPtr = distinct ptr int
    type IntPtr2 = IntPtr
    type GenericTest[T] = seq[T]
    type ConcreteTest = GenericTest[int]

    type GenericTupleWithClosures[T] = tuple[setter: proc(v: T), getter: proc(): T]

    block: # Test mangling
        doAssert getMangledName(int) == "int"
        doAssert getMangledName(DistinctInt) == "distinct[int]"
        doAssert getMangledName(float) == "float"
        doAssert getMangledName(seq[int]) == "seq[int]"
        doAssert getMangledName(SeqOfInt) == "seq[int]"
        doAssert getMangledName(ptr int) == "ptr[int]"
        doAssert getMangledName(IntPtr) == "distinct[ptr[int]]"
        doAssert getMangledName(IntPtr2) == "distinct[ptr[int]]"
        doAssert getMangledName(GenericTest[float]) == "seq[float]"
        doAssert getMangledName(ConcreteTest) == "seq[int]"
        doAssert getMangledName(tuple[x, y: int]) == "tuple[int,int]"
        doAssert getMangledName(tuple[x: int, y: float]) == "tuple[int,float]"
        doAssert getMangledName(Obj) == "object[Obj]"
        doAssert getMangledName(RefObj) == "ref[object[RefObj:ObjectType]]"
        doAssert getMangledName(array[3, float]) == "array[range[0,2],float]"
        doAssert getMangledName(array[0..2, float]) == "array[range[0,2],float]"
        doAssert getMangledName(GenericTupleWithClosures[int]) == "tuple[proc[void,int],proc[int]]"

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
        match v:
            like int as i: doAssert(false and i == 0)
            like seq[int] as s: doAssert s[1] == 2
            like: doAssert false

        v = newVariant(5.3)
        doAssert v.ofType(float)
        match v:
            like int as i: doAssert(false and i == 0)
            like float as f: doAssert f == 5.3
            like: doAssert false
