# Simple Imperative Language interpreter

Starter project for a coursework interpreter implemented through an AST.

## What works now

- AST for expressions, statements, loops, conditions and assignments;
- JSON generation from AST;
- interpreter with variables, input, output and basic runtime errors;
- a factorial program built directly as an AST.

## Run

Install GHC and Cabal (for macOS, the usual option is GHCup), then run:

```sh
cd sil-interpreter
cabal run
```

Expected result: JSON for the sample AST and `Right [120,24,6]`.

## Team split

1. One person: `src/AST.hs` and `src/Json.hs`.
2. One person: add `src/Parser.hs` using Megaparsec and connect it to `app/Main.hs`.
3. One person: extend `src/Interpreter.hs` and add tests for errors and edge cases.

The next major milestone is a parser that converts source text to `Program`.
