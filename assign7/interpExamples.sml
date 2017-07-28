(*
Here is some sample input/output for your interpreter:

In a call-by-value statically-scoped interpreter:

interp(AST_NUM 1, new_env());
(* "1" *)
val it = RES_NUM 1 : result

interp(AST_APP(AST_SUCC, AST_NUM 1), new_env());
(* "succ 1" *)
val it = RES_NUM 2 : result

interp(AST_LET("x", INT, AST_NUM 2, AST_ID "x"), new_env());
(* "let x : int = 2 in x" *)
val it = RES_NUM 3 : result

interp(AST_LET("f", ARROW(INT, INT),
                      AST_FUN("x", INT, AST_APP(AST_SUCC, AST_ID "x")),
                      AST_APP(AST_ID "f", AST_NUM 4)), new_env());
(* "let f : int -> int = (fn x : int => succ x) in (f 4) end" *)
val it = RES_NUM 5 : result

interp(AST_APP(AST_FUN("x", INT,
                 AST_APP(AST_FUN("y", INT, AST_APP (AST_SUCC, AST_ID "x")),
                         AST_NUM 0)),
               AST_NUM 7), new_env());
(* "(fn x : int => ((fn y : int => succ x) 0)) 7" *);
val it = RES_NUM 8 : result

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a dynamically-scoped interpreter:

interp(AST_LET("x", INT, AST_NUM 21,
       AST_LET("foo", ARROW(INT, INT), AST_FUN("y", INT, AST_ID "x"),
       AST_LET("x", INT, AST_NUM 13,
       AST_APP(AST_ID "foo", AST_NUM 0)))), new_env());
(* "let x = 21 in let foo = fn y => x in let x = 13 in foo 0 end end end" *)
val it = RES_NUM 13 : result

But in a statically-scoped interpreter:

interp(AST_LET("x", INT, AST_NUM 21,
       AST_LET("foo", ARROW(INT, INT), AST_FUN("y", INT, AST_ID "x"),
       AST_LET("x", INT, AST_NUM 13,
       AST_APP(AST_ID "foo", AST_NUM 0)))), new_env());
val it = RES_NUM 21 : result

Do you see why?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In any call-by-value interpreter:

interp(AST_APP(AST_FUN("x", INT, AST_NUM 34),
               AST_APP(AST_ISZERO, AST_BOOL true)), new_env());
(* "(fn x => 34) (iszero true)" *)
val it = RES_ERROR "some complaint about applying true to iszero..." : result

But in a call-by-name, or call-by-need interpreter:

interp(AST_APP(AST_FUN("x", INT, AST_NUM 34),
               AST_APP(AST_ISZERO, AST_BOOL true)), new_env());
val it = RES_NUM 34 : result

Do you see why?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
