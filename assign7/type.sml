(*  Increase print depth so abstract syntax trees get displayed completely.  *)
Control.Print.printDepth:= 100;

datatype typ = VAR of string | INT | BOOL | ARROW of typ * typ

(*  Convert a typ to a string, using as few parentheses as possible.  *)

fun typ2str (VAR a)		= "'" ^ a
|   typ2str INT			= "int"
|   typ2str BOOL		= "bool"
|   typ2str (ARROW(t1 as ARROW(_, _), t2)) =	(*  the tricky bit  *)
      "(" ^ typ2str t1 ^ ") -> " ^ typ2str t2
|   typ2str (ARROW(t1, t2))	= typ2str t1 ^ " -> " ^ typ2str t2


datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
  | AST_FUN of (string * typ * term) | AST_APP of (term * term) 
  | AST_SUCC | AST_PRED | AST_ISZERO
  | AST_IF of (term * term * term) | AST_REC of (string * typ * term)
  | AST_LET of (string * typ * term * term)

