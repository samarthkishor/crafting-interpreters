# Crafting Interpreters

This is a tree-walking interpreter for a subset of the Lox programming language
from the book [Crafting Interpreters](https://craftinginterpreters.com/). The
book uses Java but I'm using a mix of imperative- and functional-style OCaml to
implement the interpreter.

## Usage

Run `dune build @install` to compile the OCaml code. You might need to install
the relevant dependencies via `opam`.

Run `dune exec ./bin/main.exe` from the root directory to launch the interpreter.

Tests are executed by running `dune runtest`.

## Why OCaml?

### Advantages

- Interactive development with a REPL (`utop`). I can write a single function and
  send it to `utop` which compiles the function and allows me to test it. This
  makes it a lot easier to write code incrementally, and using pure functions
  allows me to compose these functions in the REPL and see how they work.
- Powerful type system. OCaml's type system is fantastic and pattern matching
  lends itself well to the task of writing an interpreter because the compiler
  makes sure the matches are exhaustive. Optional types are helpful because
  the compiler forces you to handle the None case, making it impossible to
  run into null-related exceptions at runtime.
- Editor integration. OCaml's integration with Emacs is amazing with Merlin
  acting as a language server to provide autocompletion support, type hints in
  the minibuffer, and lots of other nice IDE-like features. I'm not sure if
  support for other editors is that great (Merlin should make the experience
  pretty consistent in theory) but I'm guessing most people in the OCaml
  community use Emacs or Vim anyways.
- Strange syntax for imperative code. This might seem like more of a drawback
  but honestly I like how imperative style code tends to stick out in ML
  languages and it encourages a more functional style.

### Disadvantages

- Debugging. Even though Emacs integrates reasonably well with `ocamldebug`,
  debugger support is pretty lackluster and requires an executable. I tend to
  mostly use print statement debugging and resort to `#trace` when I need it.
  `ppx-deriving.show` helps with pretty-printing user-defined types.
- Error messages. When my code encounters a run-time exception (which happens a
  lot since this is a new language for me), it just prints the exception and the
  name of the function that threw the exception. Something like a stack trace
  would be a lot more helpful.
