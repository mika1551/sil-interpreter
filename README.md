# Simple Imperative Language interpreter

Starter project for a coursework interpreter implemented through an AST.

## Supported language features

The interpreter supports a compact imperative language with integer arithmetic and control flow:

- strings, nested array literals, indexing, indexed assignment, and `.length`
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
It emits `CONST` or `LD` for numeric expression leaves, then evaluates binary operands
left to right before `BINOP`. Each statement consumes any value it produces:
`READ` is followed by `ST`, and `write` ends with `WRITE`. Compound assignments
load the old value before evaluating the right operand. `if` uses `JZ` to
select the else branch when both branches contain code. If the then branch
is empty, it uses `JNZ` to skip the else branch; if the else branch is empty,
it omits the jump after the then branch. Nested statements can use their
enclosing statement's exit label. Loops use `JNZ` to repeat while their
condition is nonzero. `for` compiles as initialization followed by a `while`
whose body ends with the step. Generated labels have unique numeric suffixes,
so nested control flow cannot reuse a target. The resulting instruction
sequence may use different label names from the example `.sam` files while
behaving the same way.

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

Strings and arrays work in SIL, the JSON AST, and compiled SAM programs:

```sil
{
    text = "hello";
    text[0] = 72;
    values = [text, [1, 2], []];
    values[1][0] += 9;
    write(text);              -- Hello
    write(values[1]);         -- [10; 2]
    write(values.length);     -- 3
}
```

Indexing starts at zero. String elements are Unicode character codes; assigning
an element requires a valid Unicode scalar value. `.length` counts array elements
or Unicode characters. Strings use JSON
escapes, including `\n`, `\"`, `\\`, and `\u0041`. Arrays can contain any supported
value. Assignment copies a reference, so mutations are visible through aliases.
Each literal evaluation allocates a new object. `+` concatenates arrays into a
new array, preserving references to nested objects. `write` resolves references
at program completion, matching the direct interpreter; cyclic arrays produce
an error.

The JSON expression forms are `{"string":"hello"}`,
`{"array":[expression, ...]}`, `{"elem":expression,"index":expression}`, and
`{"length":expression}`. Indexed writes use
`{"setindex":{"target":expression,"index":expression,"value":expression}}`;
compound writes use `setindexop` with the same fields plus `"op":"+"` (or
another supported assignment operator). Both compact and pretty AST output
support these forms.

The compiler follows the instruction names in `examples/abstract_machine.txt`:
`STRING text` allocates a string; `ARRAY n` pops the last `n` values into an
array in evaluation order; `LENGTH` replaces an object with its length;
`ELEM` pops an index and object and pushes the element; `SET` pops a value,
index, and object and updates that element without pushing a result. In SAM
JSON these are `{"STRING":"text"}`, `{"ARRAY":2}`, `"LENGTH"`, `"ELEM"`, and
`"SET"`. Indexed compound assignments use generated temporary variables whose
names cannot collide with program variables. Bounds, types, and character codes
are checked at runtime.

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

Input values can also be JSON strings or JSON-style arrays containing integers,
strings, and other arrays, including empty arrays. Quote each array to protect
brackets and spaces from the shell.
Each top-level value is consumed by one `read`; an array is a single value:

```sh
cabal run sil-interpreter -- run list_example.json --input '[1, 2, 3]' '[10, 20]'
```

This example pairs elements up to the shorter array's length and prints the
linked list `[[1; 10]; [[2; 20]; 0]]`. Scalars and arrays can be mixed,
for example `--input 3 '[4, 5]' '[]'`. Strings and arrays are also accepted by `.sam` files and `--machine`.

For string input, keep the JSON double quotes inside shell single quotes.
For example, save `{ read(text); write(text); }` as `echo.sil`, then run:

```sh
cabal run sil-interpreter -- run echo.sil --input '"hello world"'
```

This prints `hello world`. Empty strings (`'""'`), Unicode text, and JSON
escapes such as `\n`, `\"`, `\\`, and `\u0041` are supported. Strings can
also appear in arrays, for example `--input '["hello", ["world", 42]]'`.
Each top-level string is consumed by one `read`, just like an array or integer.

The library provides `Input.parseInputValues` and
`runProgramInput :: [InputValue] -> Program -> Either StateError [OutputValue]`.
The machine provides `runMachineInput` with the same input and output types,
and `runMachineValues` for integer inputs with general output. `runMachine`
retains its integer-only input/output API.

## Tagged expressions

`TaggedExpr.runTagged` accepts a JSON expression with tags `0` (constant),
`1` (variable), or `2` (binary operation), plus an associative list of
integer variables. The list uses `[1, [name, value], rest]` for a binding and
`[0]` for its end. It evaluates the expression directly and through the
existing compiler and stack machine, returning both results and the expression
instructions.

```sh
cabal repl lib:sil-interpreter
```

```haskell
import TaggedExpr
runTagged "[2,\"+\",[2,\"*\",[1,\"a\"],[0,5]],[2,\"-\",[0,0],[0,1]]]" "[1,[\"a\",4],[0]]"
```

Both results are `19`. The direct evaluator and machine runner reuse the
project's existing numeric semantics, including undefined-variable and
division-by-zero errors.

Running the executable without arguments displays the help text.

The parsers are also available as `parseProgram :: String -> Either ParseError Program`,
`parseJsonProgram :: String -> Either String Program`, and
`parseMachine :: String -> Either String MachineProgram`.

## S-expressions and lists

Constructors begin with an uppercase letter and accept zero or more arguments:
`Nil`, `Node()`, `Node(1, Leaf(2))`. Constructor names are case sensitive.
Character literals such as `'A'` evaluate to their integer character codes.

Brace lists lower to lowercase `cons`/`nil` S-expressions. The right-associative
`:` operator prepends a value; it binds below arithmetic and above comparisons.

```sil
{
  x = Node(1, Nil);
  xs = {1, 2, 3};
  write(x);
  write(0 : xs);
  write({1, 2, 3, 4});
}
```

Output, with either the direct interpreter or `--machine`:

```text
Node(1, Nil)
{0, 1, 2, 3}
{1, 2, 3, 4}
```

Run the included example using `cabal run sil-interpreter -- run sexpressions.sil`.
Proper `cons(head, tail)` chains ending in `nil` print with braces, including
the empty list `{}`. Other constructors and improper lists print by name,
for example `cons(1, 2)`. `Nil` remains distinct from the list terminator `nil`.

S-expression fields support zero-based indexing and indexed assignments, with
the same reference and bounds-checking behavior as arrays. `.length` returns
the constructor's arity, so a `cons` cell has length two. Cyclic objects cannot
be printed.

AST JSON uses `{"sexp":"Node","args":[{"const":1},{"sexp":"Nil","args":[]}]}`.
Lists and `:` use the same representation with names `cons` and `nil`.
The compiled instruction `{"SEXP":["Node",2]}` consumes two stack values in
source argument order and pushes the new object. Both JSON parsers validate
the new forms, and both serializers emit them.

## Pattern matching

Pattern matching is available in the direct interpreter and the abstract machine
for both SIL source and JSON programs:

```sil
{
  value = Pair(11, 22);
  switch (value) {
    case whole @ Pair(first, second) -> write(first); write(second);
    | case _ -> skip;
  }
}
```

The switch expression is evaluated once. Cases are tried in order, and only the
first matching branch executes. If none matches, execution reports a runtime
error. Each switch needs at least one case and each case needs a statement.

Patterns support `_`, variable bindings, aliases (`name @ pattern`), strings,
characters, arrays, constructors, lists (`{head, next}` or `head : tail`),
parentheses, and the type patterns `#int`, `#array`, `#string`, and `#sexp`.
Array patterns require an exact length; constructor patterns require an exact
name and arity. Strings match by content, and characters match integer character
codes. Integer literal patterns are accepted in JSON; the SIL pattern grammar
uses character literals instead.

Bindings shadow outer variables only for the selected branch. Assignments to
other variables and mutations of shared objects persist. Repeated names are
allowed: siblings bind from left to right, so the later binding wins. An alias
binds after its inner pattern, so `x @ Leaf(x)` binds `x` to the whole object.
Failed cases leave no bindings behind.

JSON uses `{"switch":{"scrutinee":expression,"branches":[...]}}`, with each
branch containing `pattern` and `branch`. Patterns use `{"wildcard":[]}`,
`{"bind":{"pattern":pattern,"name":"x"}}`, `{"array":[patterns]}`, or
`{"sexp":"Name","args":[patterns]}`. Literals are bare strings or integers;
type patterns are the strings `"#int"`, `"#array"`, `"#string"`, and `"#sexp"`.
These four strings are reserved as type patterns when decoding JSON, as in the
target format; that format cannot distinguish them from literal string patterns.

`compile` and `run --machine` support switches, including nested switches and
switches inside loops. `compileProgramChecked` retains its existing `Either`
API and accepts all AST statement forms.

The compiler uses an ordered pattern matrix. Rows identify source branches;
columns identify paths into the scrutinee. It tests the leftmost constrained
column of the first row and recursively builds a specialized matrix for a
successful test and a default matrix for a failed test. Array and constructor
specialization expands their children into columns. Wildcards and compatible
type patterns participate in each applicable specialization, preserving branch
priority. Common constructor tests are shared; each branch body is emitted once.
Only successful matches install bindings, in the same order as the direct
interpreter. Each selected branch enters a machine binding scope; bindings use
source variable names. All successful paths leave that scope before continuing
after the switch. Nested switches enter nested scopes, including when the
compiler reuses a surrounding loop or conditional continuation.

Seven SAM instructions support the generated decision trees and binding scopes:

| JSON instruction | Stack effect and behavior |
| --- | --- |
| `{"ISTYPE":"int"}` | Consume one value; push integer `1` if its type matches, otherwise `0`. Types are `int`, `array`, `string`, and `sexp`. |
| `{"ISTAG":["Pair",2]}` | Consume one value; push `1` for an S-expression with this exact name and arity, otherwise `0`. Arity must be nonnegative. |
| `{"STREQ":"hello"}` | Consume one value; push `1` for a string with this content, otherwise `0`. |
| `"SCOPE"` | Enter an empty binding scope; leave the value stack unchanged. |
| `{"BIND":"name"}` | Consume one value; define or replace the name in the current scope, shadowing an outer binding. |
| `"ENDSCOPE"` | Leave the current scope, restoring outer bindings; leave the value stack unchanged. An attempt to leave the global environment reports a type error. |
| `"MATCHFAIL"` | Stop with `NoMatchingPattern` (`no matching switch case` in the CLI). |

The predicates return `0` for incompatible types and report stack underflow if
no value is available. Existing integer comparisons, `LENGTH`, `ELEM`, and
conditional jumps perform the remaining tests and field access. Shape checks
precede field access, so an incompatible pattern fails without raising a type
or bounds error. Compact and pretty SAM serialization support all seven forms.

`ST` updates the nearest existing binding. A new variable assigned with `ST`
is introduced outside binding scopes, so it persists after the switch, matching
the direct interpreter. `BIND` defines a name in the current environment even
if an outer binding exists; repeated `BIND`s replace it in traversal order.
`SCOPE` and `ENDSCOPE` do not undo changes to shared heap objects.

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
