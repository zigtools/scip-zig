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
scip-zig --root-path /path/to/zig --pkg std /path/to/zig/lib/std/std.zig --root-pkg std
src code-intel upload -github-token=$(cat tok) -file=index.scip
```

For example, let's index this very repo:

```bash
zig-out/bin/scip-zig --root-path $(pwd) --pkg scip-zig $(pwd)/src/main.zig --root-pkg scip-zig
scip convert --from index.scip
src code-intel upload -github-token=$(cat tok) -file=index.scip
```
