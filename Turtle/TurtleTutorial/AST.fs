module AST

type arg = int
type command =
   | Forward of arg
   | Turn of arg
   | Repeat of arg * command list