import std/[macros, macrocache, hashes, algorithm]

type TypeId* = int

const variantTypeIds = CacheTable"variant.typeIds"

const debugVariantTypes = defined(variantDebugTypes)

type
  Variant* {.inheritable.} = ref object
    typeId*: TypeId
    when debugVariantTypes:
      mangledName*: string
  VariantConcrete[T] = ref object of Variant
    val: T

proc getProcTy(t: NimNode): NimNode =
  var t = getTypeInst(t)
  while true:
    if t.kind == nnkBracketExpr and $t[0] == "typeDesc" and t[1].kind == nnkProcTy:
      t = t[1]
    elif t.kind == nnkBracketExpr and $t[0] == "typeDesc":
      t = getTypeInst(t[1])
    elif t.kind == nnkSym:
      t = getTypeImpl(t)
    elif t.kind == nnkProcTy:
      return t
    else:
      echo treeRepr(t)
      assert(false)

iterator combinations[T](s: openArray[T], k: int): seq[T] =
  let n = len(s)
  assert k >= 0 and k <= n
  var pos = newSeq[int](k)
  var current = newSeq[T](k)
  for i in 0..k-1:
    pos[k-i-1] = i
  var done = false
  while not done:
    for i in 0..k-1:
      current[i] = s[pos[k-i-1]]
    yield current
    var i = 0
    while i < k:
      pos[i] += 1
      if pos[i] < n-i:
        for j in 0..i-1:
          pos[j] = pos[i] + i - j
        break
      i += 1
    if i >= k:
      break

iterator allCombinations[T](s: openArray[T]): seq[T] =
  for i in 1 .. s.len:
    for c in combinations(s, i):
      yield c

proc getProcAttrs(ti: NimNode): tuple[isgcsafe, isclosure, isnosideeffect: bool] =
  var isnimcall = false
  for p in ti[1]:
    if p.kind == nnkIdent:
      case $p
      of "gcsafe": result.isgcsafe = true
      of "closure": result.isclosure = true
      of "noSideEffect": result.isnosideeffect = true
      of "nimcall": isnimcall = true
      else: discard

  if result.isclosure and isnimcall:
    assert(false, "proc cannot be closure and nimcall at the same time")
  if not isnimcall:
    result.isclosure = true

proc typeKey(t: NimNode): string =
  # getType resolves aliases and removes procedure parameter names. Hash actual
  # type symbols, not the typedesc parameter of getTypeId (older compilers hash
  # that parameter's identity instead of the concrete type it denotes).
  var typ = t
  if t.kind in {nnkSym, nnkBracketExpr, nnkProcTy, nnkTupleTy}:
    typ = t.getType
    if typ.kind == nnkBracketExpr and typ[0].eqIdent("typeDesc"):
      typ = typ[1]
  result = $typ.kind & "("
  case typ.kind
  of nnkSym:
    let inst = typ.getTypeInst
    if inst.kind == nnkBracketExpr:
      result.add inst[0].signatureHash
      for i in 1 ..< inst.len:
        result.add ":" & typeKey(inst[i])
    else:
      result.add typ.signatureHash
  of nnkBracketExpr:
    result.add typ[0].repr
    for i in 1 ..< typ.len:
      result.add ":" & typeKey(typ[i])
    let impl = typ.getTypeImpl
    if impl.kind == nnkProcTy:
      let attrs = getProcAttrs(impl)
      result.add ":closure=" & $attrs.isclosure
      var pragmas: seq[string] = @[]
      for pragma in impl[1]:
        if not pragma.eqIdent("closure") and not pragma.eqIdent("nimcall"):
          let key = pragma.repr
          if key notin pragmas: pragmas.add key
      pragmas.sort()
      for pragma in pragmas: result.add ":" & pragma
  of nnkEnumTy:
    for field in typ:
      if field.kind == nnkSym: result.add ":" & field.signatureHash
  else:
    result.add typ.repr
  result.add ")"

macro cachedTypeId(t: typedesc): untyped =
  # Independent IC modules must agree without sharing an allocation counter.
  # Keep zero reserved for an empty variant, and reject hash collisions through
  # the macro cache, whose entries are replayed when importing cached modules.
  let signature = typeKey(t)
  const maxId = when defined(js): int(high(int32)) else: high(int)
  let hashed = hash(signature) and maxId
  let id = if hashed == 0: 1 else: hashed
  let key = $id
  if variantTypeIds.hasKey(key):
    if variantTypeIds[key].strVal != signature:
      error("Variant type ID collision", t)
  else:
    variantTypeIds[key] = newLit(signature)
  result = newLit(id)

proc getTypeId*(t: typedesc): TypeId =
  cachedTypeId(t)

proc applyPragma(ti: NimNode, pr: string) =
  if ti[1].kind == nnkEmpty: ti[1] = newNimNode(nnkPragma)
  var prs = ti[1]
  if pr == "nimcall":
    # make sure there's no closure pragma
    for i in 0 ..< prs.len:
      if prs[i].kind == nnkIdent and $prs[i] == "closure":
        prs.del(i)
  prs.add(ident(pr))

proc procUpcastPermutations(ti: NimNode): seq[NimNode] =
  let attrs = getProcAttrs(ti)
  var attrA = newSeq[string]()
  if attrs.isclosure: attrA.add("nimcall")
  if not attrs.isnosideeffect: attrA.add("noSideEffect")
  if not attrs.isgcsafe: attrA.add("gcsafe")
  result.add(newCall("typeof", ti))
  for c in allCombinations(attrA):
    let tic = copyNimTree(ti)
    for i in c:
      applyPragma(tic, i)
    result.add(newCall("typeof", tic))

macro uniqueTypeIds(ids: static[seq[TypeId]]): untyped =
  result = newNimNode(nnkBracket)
  var seen: seq[TypeId] = @[]
  for id in ids:
    if id notin seen:
      seen.add id
      result.add newLit(id)

macro procTypeIdArray(t: typedesc[proc]): untyped =
  result = newNimNode(nnkBracket)
  let getTypeId = bindSym"getTypeId"
  for p in procUpcastPermutations(getProcTy(t)):
    result.add(newCall(getTypeId, p))
  result = newCall(bindSym"uniqueTypeIds", newTree(nnkPrefix, ident"@", result))

macro procTypeIdSwitchStmt(subj: untyped, t: typedesc[proc]): untyped =
  # Equivalent upcast permutations can share an ID after effect inference.
  # An if chain handles those without generating duplicate case labels.
  result = newNimNode(nnkIfStmt)
  let getTypeId = bindSym"getTypeId"
  for p in procUpcastPermutations(getProcTy(t)):
    let id = newCall(getTypeId, newCall("typeof", p))
    let ofBody = quote do:
      return cast[VariantConcrete[`p`]](v).val
    result.add(newTree(nnkElifBranch, newCall("==", subj, id), ofBody))

template getMangledName(t: typedesc): string = $t

proc ofType*(v: Variant, t: typedesc): bool {.inline.} =
  if not v.isNil:
    when t is (proc):
      return v.typeId in procTypeIdArray(t)
    else:
      return v.typeId == getTypeId(t)

proc newVariant*(): Variant {.inline.} = discard

proc newVariant*[T](val: T): Variant =
  result = VariantConcrete[T](val: val, typeId: getTypeId(T))
  when debugVariantTypes:
    result.mangledName = getMangledName(T)

proc isEmpty*(v: Variant): bool =
  result = v == nil or v.typeId == 0

proc get*(v: Variant, T: typedesc): T =
  if v.isNil:
    raise newException(Exception, "Wrong variant type: " & "nil" & ". Expected type: " & getMangledName(T))

  when T is (proc):
    procTypeIdSwitchStmt(v.typeId, T)
  else:
    if getTypeId(T) == v.typeId:
      return cast[VariantConcrete[T]](v).val

  when debugVariantTypes:
    raise newException(Exception, "Wrong variant type: " & v.mangledName & ". Expected type: " & getMangledName(T))
  else:
    raise newException(Exception, "Wrong variant type. Compile with -d:variantDebugTypes switch to get more type information.")

proc getTn(v: Variant): TypeId {.inline.} =
  if v.isNil: TypeId(0)
  else: v.typeId

template matchConst(a: typed): untyped =
  when a is (proc): procTypeIdArray(a)
  else: getTypeId(a)

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
        newCall(bindsym"matchConst", typeNode),
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

  type RefObj = ref object
    a: int

  type SomeEnum* = enum
    someVal1
    someVal2

  type SomeProcType = proc(a: int)

  suite "Variant": # Test mangling
    test "Common":
      var v = newVariant(5)
      check v.ofType(int)
      check v.get(int) == 5
      when debugVariantTypes:
        check v.mangledName == "int"
      v = newVariant(3.0)
      check v.ofType(float)
      check v.get(float) == 3.0
      when debugVariantTypes:
        check v.mangledName == "float64"
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
        of SomeProcType as p: p(0)
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
      check(v.get(proc(b: int): int {.gcsafe, noSideEffect.})(6) == 12)

    test "Char":
      let v = newVariant('a')
      check(v.get(char) == 'a')
