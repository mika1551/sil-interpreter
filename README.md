# Simple Imperative Language interpreter

Starter project for a coursework interpreter implemented through an AST.

## Supported language features

The interpreter supports a compact imperative language with integer arithmetic and control flow:

- variable assignment and reassignment
- arithmetic operators: `+`, `-`, `*`, `/`, `%`
- comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
- boolean operators: `&&`, `||`
- input/output statements: `read`, `write`
- flow control: `if ... else`, `while`, `do ... while`, `for`
- blocks: `{ ... }`
- comments: `-- ...` and `(* ... *)`
- empty statement: `skip`
- runtime checks for undefined variables, exhausted input, and division by zero
- JSON rendering of the parsed AST for inspection

The program is a block with one or more statements inside it:

```sil
{
    read(n);
    if (n > 0) {
        write(n);
    } else {
        write(0);
    }
}
```

## Example program

```sil
{
    read(rep);

    while (rep > 0) {
        rep -= 1;
        read(n);

        f = 1;

        while (n > 0) {
            f *= n;
            n -= 1;
        }

        write(f);
    }
}
```

This example reads numbers, computes factorials, and prints each result as a separate line.

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

## Test suite

The project includes an HUnit-based test suite in [tests/Tests.hs](tests/Tests.hs). It is wired into the Cabal project and can be executed with:

```sh
cabal test sil-interpreter-tests --test-show-details=direct
```

The test suite covers the full implemented functionality of the interpreter:

- parsing of all statement kinds: `read`, `write`, assignments, `if`, `while`, `do while`, `for`, blocks, `skip`
- AST construction and operator precedence rules
- all arithmetic, comparison, and boolean operators
- runtime evaluation of expressions and variable lookups
- assignment operators: `+=`, `-=`, `*=`, `/=`, `%=`
- `if` with and without an `else` branch
- `for`, `while`, and `do while` loop semantics
- runtime error handling for undefined variables, exhausted input, and division by zero
- CLI argument parsing for `run`, `ast`, `check`, and help output
- JSON generation for compact and pretty output

This ensures the interpreter behavior stays aligned with the actual AST, parser, evaluator, and command-line interface.
