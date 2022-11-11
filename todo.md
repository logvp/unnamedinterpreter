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

- [ ] Everything expression bases
  - [x] `if` expressions
  - [x] `while` expressions
  - [ ] Blocks as expressions
  - [x] Allow expressions in `AstNode`s
- [ ] Arrays
- [ ] User defined types
- [ ] Immutable variables
- [ ] Early return
- [ ] Multiline REPL statements
- [ ] Comments
- [ ] Negation operator
- [ ] Question "not null" operator

## Improvements

- [x] Call lambda without binding to variable name
- [ ] Better error reporting
  - [x] Enumerated error types
  - [ ] Location + snippet
- [ ] Rework `Context` + scope
  - [ ] More "proper" return values(?)
  - How would you look up anonymous Lvalue eg array index
- [ ] Handle escape sequences in strings
- [x] No longer evaluate statements to values

## Bugs

- [x] If return value is undefined, closures will return the return value of the parent environment
- [x] Unary negate operator doesn't work
- [ ] `RuntimeValue`: "private type `FunctionType` in public interface"
- [ ] `loc` in lexer is not accurate
- [ ] Body of lambdas do not return value of body
  - Requires revisiting return values
