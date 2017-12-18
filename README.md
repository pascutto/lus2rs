# lus2rs

A Lustre to Obj compiler


## Compiling

##### Dependencies
The `menhir` library is required for parsing. You can find it on OPAM repository and install it with the following command, supposing you have `opam` installed.
```
opam install menhir
```
Alternatively, you can check the [Menhir project page](gallium.inria.fr/~fpottier/menhir).

Any recent version of `ocaml` and `menhir` should do.
#### Compilation
To compile the project, just use
```
make compile
```
This will produce a `lus2rs` binary file in the `src/` folder.
All created files may be cleaned using `make clean`.

## Usage
```
usage: src/lus2rs [options] file.lus
  --parse-only    -p    stops after parsing
  --type-only     -t    stops after typing
  --clock-only    -c    stops after clocking
  --schedule-only -s    stops after scheduling
  --norm-only     -n    stops after normalization
  --obj-only      -o    stops after obj translation
  --verbose       -v    print intermediate transformations
  --help          -help Display this list of options
```

## Testing
Some test files are provided in the `examples/` folder; you can manually test the compiler. Some of these files come from [this repository](https://github.com/jahierwan/lustre-examples), others were manually typed.
Alternatively, you can run the test scrip with
```
make test
```

Also, you can both compile and test by just typing `make`.

## Project files

All source files are contained in `src/` folder.

| File           | Purpose        | LOC |
| -------------- | -------------- |     |
| ast_types_lustre.mli | Auxiliary file for common elements (types, operators...) | 24 |
| ast_*_lustre.mli | ASTs for lustre/typed lustre/typed and clocked lustre | 155 |
| lexer_lustre.mll | Self explanatory | 100 |
| parser_lustre.mly | Self explanatory | 170 |
| sugar.ml | Handles syntactic sugar such as global constants, `->` or `if . the . else .` | 54 |
| typer.ml | Typing step | 463 |
| clocker.ml | Clocking step | 147 |
| normalizer.ml | Normalization step | 147 |
| scheduler.ml | Scheduling step | 84 |
| translater.ml | Translation to obj step | 138 |
| ident.ml | Auxiliary module to handle unique identifiers | 33 |
| printer_*.ml | Printers for these asts | 499 |
| ast_obj.mli | AST for Obj | 29 |


## What works (and what does not)

#### Implemented features
  - [x] Core (Nodes, logic and arithmetic operations)
  - [x] `->` and `fby`
  - [x] `pre`
  - [x] `current`
  - [x] `merge`
  - [x] `reset`
  - [ ] Initialization analysis (bugfixing, will be merged soon)
  - [ ] State machines (will be done)
  - [ ] Advanced types : enum, records, arrays...

#### Bugs and TODOs
##### Bugs
  - Clocks signatures for nodes might contain a bug when output clocks depend on input clocks (not on all examples though, can't find example of such a bug yet)
  - Calls of some nodes with multiple outputs might not o through the Obj generation for the translater does not know (yet) when to reset it (see fails on the test sample).
##### TODOs
  - There might me quite some ununsed functions, particularly in the clocker.
  - Write better printer for Obj.
