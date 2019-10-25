# Crafting Interpreters

This is an interpreter for the Lox programming language from the book [Crafting
Interpreters](https://craftinginterpreters.com/).  The book uses Java but I am
using a more functional style of OCaml to write the interpreter.

## Advantages of OCaml

Ocaml is a really nice language for this project because it has the following
features:

- Interactive development with a REPL (`utop`).  I can write a single function and
  send it to `utop` which compiles the function and allows me to test it.  This
  makes it a lot easier to write code incrementally, and using pure functions
  allows me to compose these functions in the REPL and see how they work.
- Powerful type system.  OCaml's type system is fantastic and pattern matching
  lends itself well to the task of writing an interpreter because the compiler
  makes sure the matches are exhaustive.  Optional types are helpful because
  the compiler forces you to handle the None case, making it impossible to
  run into null-related exceptions at runtime.
