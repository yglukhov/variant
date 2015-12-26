import macros
import tables
import hashes
import strutils

var typeIds {.compileTime.} = initTable[int, string]()

proc mangledName(t: NimNode): string =
    case t.kind
    of nnkSym:
        let impl = getImpl(t.symbol)
        case impl.kind
        of nnkNilLit:
            let tt = getType(t)
            if tt.kind == nnkSym and $tt == $t:
                result = $t
            else:
                result = mangledName(tt)
        of nnkTypeDef:
            result = mangledName(impl[2])
        else:
            echo "Unknown sym typ"
            echo treeRepr(t)
            assert(false)
    of nnkBracketExpr:
        var tt : NimNode = nil
        case $t[0]
        of "typeDesc":
            tt = t[1]
        of "range":
            result = "range[" & mangledName(t[1]) & "," & mangledName(t[2]) & "]"
        else:
            tt = getType(t)
            if tt.kind == nnkBracketExpr and $tt[0] == "typeDesc":
                result = mangledName(tt)
                tt = nil
        if tt != nil:
            if tt.kind == nnkSym:
                result = $tt
            else:
                case $tt[0]
                of "seq":
                    result = "seq[" & mangledName(tt[1]) & "]"
                of "array":
                    result = "array[" & mangledName(tt[1]) & "," & mangledName(tt[2]) & "]"
                of "ref":
                    result = "ref[" & mangledName(tt[1]) & "]"
                of "ptr":
                    result = "ptr[" & mangledName(tt[1]) & "]"
                else:
                    echo treeRepr(t)
                    assert(false)

    of nnkPtrTy:
        result = "ptr[" & mangledName(t[0]) & "]"
    of nnkRefTy:
        result = "ref[" & mangledName(t[0]) & "]"
    of nnkDistinctTy:
        result = "distinct[" & mangledName(t[0]) & "]"
    of nnkObjectTy:
        result = "object[" & t.lineinfo & "]"
    of nnkTupleTy:
        let tt = getType(t)
        result = "tuple["
        for i in 1 ..< tt[1].len:
            if i != 1: result &= ","
            result &= mangledName(tt[1][i])
        result &= "]"
    of nnkIntLit:
        result = $t.intVal
    else:
        echo "Unknown typ"
        echo treeRepr(t)
        assert(false)

macro getMangledName*(t: typed): string =
    mangledName(t)

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

#    macro dt(t: typed): stmt =
#        echo treeRepr(t)
#        echo treeRepr(getType(t))

#    dt(Obj)

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
        doAssert getMangledName(Obj).startsWith("object[variant.nim(")
        doAssert getMangledName(RefObj).startsWith("ref[object[variant.nim(")
        doAssert getMangledName(array[3, float]) == "array[range[0,2],float]"
        doAssert getMangledName(array[0..2, float]) == "array[range[0,2],float]"

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
