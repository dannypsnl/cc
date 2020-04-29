# cc

A compiler of C subset. Precisely, a C subset with `bool` type extension for convenience, but without control flow since its not the point I create this toy project. The most important stuff is understanding how x86-64 instructions work with call convention and convert structure type into a memory thunk(in progress). Therefore, skip the control flow implementation, but still welcome to any pull-requests for control flow.

### Purpose

- [x] type id preparation in parsing
- [x] check via rule emit
  - [x] traveling on AST to emit rules
  - [x] rule-based checker
- [ ] compile to x86
  - [ ] global variabel
  - [ ] structure
  - [x] integer arithmetic
    - [x] add
    - [x] sub
    - [x] mul
    - [x] div
  - [x] function
    - [x] call convection

### Build

```sh
git clone git@github.com:dannypsnl/cc.git
raco pkg install --auto
# Test
raco test .
# Build
raco exe main.rkt
```
