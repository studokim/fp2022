# An implementaion of Lua mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in mini-language

Author: Muravev Kirill, studokim@gmail.com

## Features

There are no plans to support `<const>` and other new features. The reference specification was Lua 5.0.

Features done:

- AST
- Parser
- Interpreter
  - variables
  - nil, boolean, numeric, string literals
  - arithmetic and logic operations
  - assignment
  - function definitions, calls (with recursion) and `return`
  - builtin functions: `print(args, ...)`, `dofile(path)`, `__show_env()`
  - `if ... then ... else ... end`
- REPL
- Ability to calculate the factorial of 170

Features in progress (and TODOs):

- TODO: get rid of implicit dependences:
  - Chunk (function call, `then`-part etc.) behaves correctly iff `Statement.execute` produces `Literal Nil` in all cases but `Return`.
  - Assignment and `print` behave correctly iff `Expression.execute` produces nothing but `Literal` or `Identifier`.
- Currently binary operations must be enclosed in parens, even if there's only one.
  - E.x. `print(x + 2)` won't work, but `print((x + 2))` will.
- Currently no support for unary operators, `not` is implemented as a function.
- Currently no loops, no `break`s, no `do`-blocks.
- Currently no support for the `local` keyword, all functions and variables are global.
- Currently functions are [not anonymous](https://www.lua.org/pil/6.html).
  - I.e. `foo = function (x) return 2*x end` won't work.
- Currently only nil, booleans, numbers and strings are supported.
  - No support for escaped chars. String literals are parsed from `"` to `"`.
  - No support for scientific e-notation in numbers.
  - No arrays, no tables.
  - No data structures.

Limitations that differ this implementation from standard Lua:

- Comments are parsed as statements, therefore may only be placed in-between of other statements.
  - E.x. `print(x, --[[comment--]] y)` won't work.
  - Line-long comments (those starting with `--`) are not supported, because line breaks are ignored.
- Every expression is also a statement.
  - E.x. one can write `print() (x+1) y=2` (or `print(); (x+1); y=2`).
  - They'll be executed, but won't modify the program state though.
- Tables are not supported yet, therefore no direct access to `_G` ([table with globals](https://www.lua.org/pil/14.html)).
  - But `__show_env()` will print all declared variables with their values and all function declarations.
- Anything else?

## Usage

After compilation, run `./_build/default/bin/main.exe` to enter interactive (REPL) mode. Input is executed chunk-by-chunk, each chunk may consist of several statements. Input is multi-line: a new chunk is indicated by `>` prompt, all lines having `>>` prompt are appended to this chunk. Empty input indicates the end of chunk. To exit REPL, send `EOF` (usually `Ctrl+D`).

To execute single program, run `./_build/default/bin/main.exe path-to-program.lua` (file extension doesn't matter). The whole program is interpreted as a chunk.

To import a program to REPL, call `dofile("path-to-program.lua")`. All the chunk will be executed immediately, all variables and functions will become available in REPL (hiding those having same names).
