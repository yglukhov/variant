# Package

version       = "0.3.1"
author        = "Yuriy Glukhov"
description   = "Variant type and type matching"
license       = "MIT"

task test, "Run tests":
  exec "nim c -r --mm:refc variant"
  exec "nim c -r --mm:refc -d:variantDebugTypes variant"
  exec "nim c -r --mm:orc variant"
  exec "nim c -r --mm:orc -d:variantDebugTypes variant"
  # TODO: JS is broken...
  # exec "nim js -r variant"
  # exec "nim js -r -d:variantDebugTypes variant"
  exec "nim cpp -r variant"
  exec "nim cpp -r -d:variantDebugTypes variant"
  exec "nim c -r tests/ttypeids"
  exec "nim js -r tests/ttypeids"
  exec "nim cpp -r tests/ttypeids"

task testIC, "Run incremental compilation regression (requires nim ic)":
  exec "nim ic -f -r tests/ttypeids"
  exec "nim ic -r tests/ttypeids"
