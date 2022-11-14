# TODO List

## Goals

- [ ] Demonstrate Turning completeness
- [ ] Comprehensive Tests
- [ ] Compile (not goals just ideas)
  - [ ] C(++) Transpilation
  - [ ] LLVM
  - [ ] JVM
  - [ ] DotNet
  - [ ] Native (x86)

## Features

- [x] Multiline REPL statements
- [x] Immutable variables
- [ ] Everything expression bases
  - [x] `if` expressions
  - [x] `while` expressions
  - [ ] Blocks as expressions
  - [x] Allow expressions in `AstNode`s
- [ ] Arrays
- [ ] User defined types
- [ ] Early return
- [ ] Comments
- [ ] Negation operator
- [ ] Question "not null" operator
- [ ] Named arguments
- [ ] Default arguments

## Improvements

- [x] Call lambda without binding to variable name
- [x] else if
- [ ] Better error reporting
  - [x] Enumerated error types
  - [ ] Location + snippet
- [ ] Rework `Context` + scope
  - How would you look up anonymous Lvalue eg array index
- [ ] Handle escape sequences in strings
- [x] No longer evaluate statements to values
- [ ] Get rid of REPL macros or make them better

## Bugs

- [x] ~~If return value is undefined, closures will return the return value of the parent environment~~
- [x] Unary negate operator doesn't work
- [x] Body of lambdas do not return value of body
- [x] `RuntimeValue`: "private type `FunctionType` in public interface"
- [ ] `loc` in lexer is not accurate
