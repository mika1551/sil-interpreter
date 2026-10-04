# Simple Imperative Language interpreter

Starter project for a coursework interpreter implemented through an AST.

## Supported language features

The interpreter supports a compact imperative language with integer arithmetic and control flow:

- variable assignment and reassignment
- arithmetic operators: `+`, `-`, `*`, `/`, `%`
- comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
- boolean operators: `&&`, `!!`
- input/output statements: `read`, `write`
- flow control: `if ... else`, `while`, `do ... while`, `for`
- blocks: `{ ... }`
- comments: `-- ...` and `(* ... *)`
- empty statement: `skip`
- runtime checks for undefined variables, exhausted input, and division by zero
- JSON parsing and rendering of the AST

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

Install GHC and Cabal (for macOS, the usual option is GHCup), then run. Cabal will fetch the Megaparsec parser dependency:

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

To execute the same source through the abstract machine, add `--machine`.
The program is compiled in memory and passed directly to the machine
interpreter; no `.sam` file is needed:

```sh
cabal run sil-interpreter -- run program.sil --machine --input 3 5 4 3
```

This also works for `.json` AST input. Existing `.sam` files always run on the
machine, with or without `--machine`.

The `run`, `check`, and `ast` commands also accept `.json` files containing
the JSON AST. The file extension selects the parser:

```sh
cabal run sil-interpreter -- run program.json --input 3 5 4 3
cabal run sil-interpreter -- check program.json
cabal run sil-interpreter -- ast program.json --pretty
```

Machine programs use `.sam` files containing JSON instruction arrays. Run or
check one with the same commands and input option:

```sh
cabal run sil-interpreter -- run program.sam --input 3 5 4 3
cabal run sil-interpreter -- check program.sam
```

Compile SIL source or its JSON AST to a SAM program. Without `-o`, the
instruction array is printed to standard output:

```sh
cabal run sil-interpreter -- compile program.sil -o /tmp/program.sam
cabal run sil-interpreter -- run /tmp/program.sam --input 3 5 4 3
cabal run sil-interpreter -- compile program.json
```

The compiler is also available as `compileProgram :: Program -> MachineProgram`.
It emits `CONST` or `LD` for expression leaves, then evaluates binary operands
left to right before `BINOP`. Each statement consumes any value it produces:
`READ` is followed by `ST`, and `write` ends with `WRITE`. Compound assignments
load the old value before evaluating the right operand. `if` uses `JZ` to
select the else branch; loops use `JNZ` to repeat while their condition is
nonzero. `for` compiles as initialization followed by a `while` whose body
ends with the step. Generated labels have unique numeric suffixes, so nested
control flow cannot reuse a target. The resulting instruction sequence may
use different label names from the example `.sam` files while behaving the
same way.

`ast` applies only to `.sil` and `.json` files. The machine runner executes
the instructions in a `.sam` file as written.

The JSON format has a statement at its root. Statements include `"skip"`,
`{"read":"name"}`, `{"write":expression}`,
`{"assn":{"dst":"name","src":expression}}`, and
`{"seq":{"left":statement,"right":statement}}`. Control flow uses `while`
with `cond` and `body`, `do` with `body` and `cond`, or `if` with `cond`,
`then`, and `else`. Expressions are `{"var":"name"}`, `{"const":integer}`,
or an object with `binop`, `left`, and `right`. The `ast` command prints this
format from either input type.

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

The parsers are also available as `parseProgram :: String -> Either ParseError Program`,
`parseJsonProgram :: String -> Either String Program`, and
`parseMachine :: String -> Either String MachineProgram`.

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
- JSON parsing, execution, and validation

This ensures the interpreter behavior stays aligned with the actual AST, parser, evaluator, and command-line interface.
