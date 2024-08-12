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
  exec "nim js -r variant"
  exec "nim js -r -d:variantDebugTypes variant"
  exec "nim cpp -r variant"
  exec "nim cpp -r -d:variantDebugTypes variant"
