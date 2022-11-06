# TODO List

## Goals

- [ ] Demonstrate Turning completeness
- [ ] Compile (not goals just ideas)
  - [ ] C(++) Transpilation
  - [ ] LLVM
  - [ ] JVM
  - [ ] DotNet
  - [ ] Native (x86)

## Features

- [ ] Everything expression bases
  - [ ] `if` expressions
  - [ ] `while` expressions
  - [ ] Blocks as expressions
  - [ ] Allow expressions in `AstNode`s
- [ ] Arrays
- [ ] User defined types
- [ ] Immutable variables
- [ ] Early return
- [ ] Multiline REPL statements
- [ ] Comments
- [ ] Negation operator
- [ ] Question "not null" operator

## Improvements

- [ ] Rework `Context` + scope
  - [ ] More "proper" return values(?)
  - How would you look up anonymous Lvalue eg array index
- [ ] Call lambda without binding to variable name
- [ ] Better error reporting
  - [ ] Enumerated error types
  - [ ] Location + snippet
- [ ] Handle escape sequences in strings
- [ ] Tests

## Bugs

- [x] If return value is undefined, closures will return the return value of the parent environment
- [ ] `RuntimeValue`: "private type `FunctionType` in public interface"
- [ ] `loc` in lexer is not accurate
