# cc

![Build Status](https://github.com/dannypsnl/cc/workflows/Racket/badge.svg?branch=develop)

A compiler of C subset. Precisely, a C subset with `bool` type extension for convenience, but without control flow since that is not the point of this toy project. The most important stuff is understanding how x86-64 instructions work with call convention. Therefore, skip the control flow implementation, but still welcome to any pull-requests for control flow.

### Purpose

- [x] type id preparation in parsing
- [x] check via rule emit
  - [x] traveling on AST to emit rules
  - [x] rule-based checker
- [x] compile to x86
  - [x] global variable
  - [x] integer arithmetic
    - [x] add
    - [x] sub
    - [x] mul
    - [x] div
  - [x] function
    - [x] call convention(parameter)
    - [x] call convention(argument)

### Non-interest part

I may do these features in the future or not. But welcome any PRs for these.

- [ ] structure
- [ ] control flow
  - [ ] if
  - [ ] switch
  - [ ] loop
    - [ ] for
    - [ ] while
    - [ ] do while

### Usage

```sh
git clone https://github.com/racket-tw/cc.git
raco pkg install --auto
# Test
raco test .
# Run
racket main.rkt <c-file>
```

### Error Reporting

```sh
racket main example/should-report/list-fail.c
```

![image](https://user-images.githubusercontent.com/22004511/102698284-2db04400-4277-11eb-93d8-8bf9c4f4dc5a.png)
