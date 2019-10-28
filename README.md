# Crafting Interpreters

This is an interpreter for the Lox programming language from the book [Crafting
Interpreters](https://craftinginterpreters.com/). The book uses Java but I'm
using a more functional style of OCaml to write the interpreter.

## Advantages of OCaml

- Interactive development with a REPL (`utop`). I can write a single function and
  send it to `utop` which compiles the function and allows me to test it. This
  makes it a lot easier to write code incrementally, and using pure functions
  allows me to compose these functions in the REPL and see how they work.
- Powerful type system. OCaml's type system is fantastic and pattern matching
  lends itself well to the task of writing an interpreter because the compiler
  makes sure the matches are exhaustive. Optional types are helpful because
  the compiler forces you to handle the None case, making it impossible to
  run into null-related exceptions at runtime.
- Symbolic computation. It's easy to represent abstract things like an
  expression as a union type of symbols (similar to an `enum` in the C family)
  which is another reason why the language is great for writing an interpreter.
- Editor integration. OCaml's integration with Emacs is amazing with Merlin
  acting as a language server to provide autocompletion support, type hints in
  the minibuffer, and lots of other nice IDE-like features. I'm not sure if
  support for other editors is that great (Merlin should make the experience
  pretty consistent in theory) but I'm guessing most people in the OCaml
  community use Emacs or Vim anyways.
- Strange syntax for imperative code. This might seem like more of a drawback
  but honestly I like how imperative style code tends to stick out in ML
  languages and it encourages a more functional style.

## Disadvantages of OCaml

- Debugging. Even though Emacs integrates reasonably well with `ocamldebug`,
  debugger support is pretty lackluster and requires an executable. I tend to
  mostly use print statement debugging and resort to `#trace` when I need it.
- Error messages. When my code encounters a run-time exception (which happens a
  lot since this is a new language for me), it just prints the exception and the
  name of the function that threw the exception. Something like a stack trace
  would be a lot more helpful.
- Poor standard library. This isn't super important since I could always use
  something like Jane Street's Core or a similar alternative standard library. I
  doubt I'll need any fancy data structures and functions for this interpreter.
  Sometimes standard library functions are strange: for example, `String.sub`
  does not behave like the substring function in most mainstream programming
  languages which was extremely confusing at first.
