# Dead Code Elimination Showcase 

In this repository I'd like to show how to implement a very simple approach to
dead code elimination. The "core" of this analysis will be [live variable
analysis](https://en.wikipedia.org/wiki/Live_variable_analysis) based on a
classical data flow analysis approach, as presented in Nielson's _Principles of
Program Analysis_.

This application is written in the [OCaml](https://ocaml.org/) functional language. 
It consists of following pieces:

* Parser and lexer of a toy language called
  [While](https://en.wikipedia.org/wiki/While_loop#While_programming_language).
* An unparser (program which transforms AST into the formatted code).
* The analysis code.
* Interpreter (?).

## Requirements

## Compilation

## License

MIT. See [LICENSE](LICENSE) file for details.
