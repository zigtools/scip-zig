# scip-zig

[SCIP](https://github.com/sourcegraph/scip) indexer for [Zig](https://ziglang.org). Experimental.

## Supported features

Our benchmark for `scip-zig` is its ability to index `std`; unfortunately importing external packages via `addPackage` isn't supported *yet*.

- [x] Globals
- [ ] Properly creating multiple indices for different packages
- [x] Imports
- [x] Namespaced declarations (structs, enums, unions)
- [x] Functions
  - [ ] Arguments
  - [x] Return values
  - [x] Bodies
- [x] Locals
  - [x] With explicit typing
  - [ ] With inferred typing
- [ ] Easy integration into `build.zig` / CI
- [ ] `usingnamespace`
- [ ] `comptime`

## Installing

To install `scip-zig`, simply `git clone` this repository and run `zig build`; you'll find the indexer in `zig-out/bin`!

## Usage

```bash
# To index std; currently uses about a 400mbs of memory
# Windows: should finish in under 10 seconds in release-fast, under a minute in debug
# WSL: should finish in under 3 seconds in release-fast
scip-zig --pkg std /path/to/std/std.zig --root std
```
