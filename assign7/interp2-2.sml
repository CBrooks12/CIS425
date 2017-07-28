(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

use "type.sml";

exception not_found;
datatype env = Env of (string * result) list
and
(* Here is a result datatype *)
result =
    RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (string * term * env);

(* Here is a basic environment implementation *)


fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;

(*  Here's a partial skeleton of interp : (term * env) -> result.
    I've done the first case for you
*)
fun interp (exp, env) = 

  case exp of
    AST_NUM  x                    => RES_NUM x
  | AST_BOOL b                    => RES_BOOL b
  | AST_SUCC                      => RES_SUCC
  | AST_PRED                      => RES_PRED
  | AST_ISZERO                    => RES_ISZERO
  | AST_ID name                   => lookup_env(env, name)
  | AST_FUN (var, ty, exp)        => RES_CLOSURE(var, exp, env)
  | AST_LET (var, ty, exp1, exp2) => let val v1 = interp(exp1, env)
                                        in case v1 of
                                            RES_ERROR string => v1
                                        | _ => interp(exp2,extend_env(env,var,v1))
                                     end
  | AST_REC (var, ty, exp)        => (* You don't need to implement this *)
                                     RES_ERROR "Not yet implemented"
  | AST_IF (exp1, exp2, exp3)     => let    val v1 = interp(exp1, env)
                                            val v2 = interp(exp2, env)
                                            val v3 = interp(exp3, env)
                                     in
                                            if v1 = RES_BOOL true
                                                then v2
                                            else if v1 = RES_BOOL false
                                                then v3
                                            else RES_ERROR "boolean error"
                                     end
  | AST_APP (exp1, exp2)          => case (interp(exp1, env), interp(exp2,env)) of
                                        (RES_ERROR string, _)   => RES_ERROR string
                                        |(_,RES_ERROR string)   => RES_ERROR string
                                        |(RES_SUCC, RES_NUM n)  => RES_NUM(n+1)
                                        |(RES_PRED, RES_NUM n)  => if n = 0
                                                                   then RES_NUM 0
                                                                   else RES_NUM(n-1)
                                        |(RES_ISZERO, RES_NUM n)=> if n = 0
                                                                   then RES_BOOL true
                                                                   else RES_BOOL false
                                        |(RES_CLOSURE(var, exp, static_env),e2) => let val newEnv = extend_env(static_env, var, e2)
                                                                                    in
                                                                                        interp(exp, newEnv)
                                                                                    end
                                        |(_,_)                  =>  RES_ERROR "bad input"  
