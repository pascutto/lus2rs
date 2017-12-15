# lus2rs

A Lustre to Rust compiler

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
Some test files are provided in the `examples/` folder; you can manually test the compiler.
Alternatively, you can run the test scrip with
```
make test
```

Also, you can both compile and test by just typing `make`.
