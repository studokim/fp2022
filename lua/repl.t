  $ ./REPL.exe
  Hello from Lua!
  >  
  $ ./REPL.exe test/data-types.lua
  -----
  a = 1.
  b = 1.5
  c = -2.
  d = -2.55555
  e = true
  f = false
  g = "string"
  h = "string with spaces"
  j = nil
  -----
  $ ./REPL.exe test/return-a-function.lua
  -----
  f ()
  g ()
  -----
  $ ./REPL.exe test/comments-only.lua
  $ ./REPL.exe test/print-function-call.lua
  square area is 25.
  and its perimeter is 20.
  $ ./REPL.exe test/shadowing.lua
  -----
  x = 3.
  f ()
  -----
  -----
  x = 1.
  z = 3.
  f ()
  -----
  $ ./REPL.exe test/comments.lua
  -----
  x = 1.
  z = 3.
  f ()
  -----
  $ ./REPL.exe test/comments-in-between.lua
  Fatal error: exception Lua_lib.Parser.Error(": end_of_input")
  [2]
  $ ./REPL.exe test/factorial-definition.lua
  $ ./REPL.exe test/factorial-execution.lua
  5.42391066613e+295
  $ ./REPL.exe test/dofile-not-exist.lua
  Fatal error: exception Failure("file doesn't exist")
  [2]
  $ ./REPL.exe test/dofile.lua
  true
  $ ./REPL.exe test/logical.lua
  -----
  a = true
  b = false
  c = false
  d = true
  e = false
  x = 1.
  -----
  $ ./REPL.exe test/condition.lua
  -----
  a00 = true
  a01 = true
  a02 = true
  a10 = true
  a11 = true
  a20 = false
  a21 = false
  a22 = true
  a30 = false
  a31 = true
  x = 1.
  f ()
  -----
