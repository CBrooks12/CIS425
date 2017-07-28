use "type.sml";
   
(* Here is a basic environment implementation *)
exception not_found;
datatype env = Env of (string * typ) list

fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;

exception TypeError

(* typeOf : term * env -> typ *)
fun typeOf (tm, env) = 

  case tm of
    AST_NUM  x             => INT
  | AST_BOOL b             => BOOL
  | AST_SUCC               => ARROW (INT, INT)
  | AST_PRED               => ARROW (INT, INT)
  | AST_ISZERO             => ARROW (INT, BOOL)
  | AST_IF (tm1, tm2, tm3) => let val ty1 = typeOf (tm1, env)
                              in if ty1 = BOOL
                                 then
                                   let val ty2 = typeOf (tm2, env)
                                       val ty3 = typeOf (tm3, env)
                                   in if ty2 = ty3
                                      then ty2
                                      else raise TypeError
                                   end
                                 else raise TypeError
                              end
  | AST_APP (tm1, tm2)     => let val ty1 = typeOf (tm1, env)
                              in case ty1 of
                                   ARROW(argTy, resTy) =>
                                     let val ty2 = typeOf (tm2, env)
                                     in if ty2 = argTy
                                        then resTy
                                        else raise TypeError
                                     end
                                 | _ => raise TypeError
                              end
  | AST_ID name            => lookup_env (env, name)
  | AST_FUN (var, ty, tm1) => let val newEnv = extend_env(env, var, ty)
                                  val resTy  = typeOf (tm1, newEnv)
                              in ARROW(VAR "t1", VAR "t2")
                              end
  | _                      => (* not implementing AST_LET, AST_REC *)
                              raise TypeError

(*
Some sample functions translated into abstract syntax for you to test
your typechecker on:
*)

(* fn f => fn x => f (f x) *)
val test1 = AST_FUN("f", ARROW(VAR "a", VAR "a"),
                AST_FUN("x", VAR "a",
                    AST_APP(AST_ID "f",
                        AST_APP(AST_ID "f", AST_ID "x"))));

(* fn f => fn g => fn x => f (g x) *)
val test2 = AST_FUN("f", ARROW(VAR "b", VAR "c"),
                AST_FUN("g", ARROW(VAR "a", VAR "b"),
                    AST_FUN("x", VAR "a",
                        AST_APP(AST_ID "f",
                            AST_APP(AST_ID "g", AST_ID "x")))));

(* fn b => if b then 1 else 0 *)
val test3 = AST_FUN("b", BOOL,
                AST_IF(AST_ID "b", AST_NUM 1, AST_NUM 0));


(* feel free to write your own test expressions! *)
