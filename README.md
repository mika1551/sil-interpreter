# Simple Imperative Language interpreter

Starter project for a coursework interpreter implemented through an AST.

## What works now

- AST for expressions, statements, loops, conditions and assignments;
- JSON generation from AST;
- interpreter with variables, input, output and basic runtime errors;
- a sample factorial program written in SIL.

## Command-line interface

Install GHC and Cabal (for macOS, the usual option is GHCup), then run:

```sh
cd sil-interpreter
cabal run sil-interpreter -- --help
```

Execute a SIL program. Values passed to `write` are printed one per line:

```console
$ cabal run sil-interpreter -- run program.sil --input 3 5 4 3
120
24
6
```

Inspect the parsed AST as compact or formatted JSON:

```sh
cabal run sil-interpreter -- ast program.sil
cabal run sil-interpreter -- ast program.sil --pretty
```

Check syntax without executing the program:

```sh
cabal run sil-interpreter -- check program.sil
```

The `--input`/`-i` option accepts space-separated or comma-separated integers
that are consumed by `read` statements in order. These are equivalent:

```sh
cabal run sil-interpreter -- run program.sil --input 3 5 4 3
cabal run sil-interpreter -- run program.sil -i 3,5,4,3
```

Running the executable without arguments displays the help text.

The parser is also available as `parseProgram :: String -> Either ParseError Program`.
