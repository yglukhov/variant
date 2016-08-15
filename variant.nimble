# Package

version       = "0.2.0"
author        = "Yuriy Glukhov"
description   = "Variant type and type matching"
license       = "MIT"

# Dependencies

requires "nim >= 0.12.1"

task tests, "Run tests":
    exec "nim c -r variant"
    exec "nim c -r -d:variantDebugTypes variant"
    exec "nim js -r variant"
    exec "nim js -r -d:variantDebugTypes variant"
