(*Chris Brooks
Spring 2017

Assignment 5

1.

*)
datatype 'a tree = LEAF of 'a |
NODE of 'a tree * 'a tree;

fun reduce f (LEAF x) = x
    | reduce f (NODE (e1,e2)) = f(reduce f e1, reduce f e2);
    
(* reduce takes in a function and a tree.
to break down the tree, there are two possible data types, leaf and node. if the
value is leaf, return the value, if it's a node, apply the function and take in 
both sides of the node as the parameters, but for each of those sides, apply
reduce to the values. It climbs back up when it hits a leaf. *)
(*2.
*)
fun Curry f = fn x => fn y => f(x,y);
fun UnCurry f = fn(x,y) => f x y;

(*
Curry is a way to take in arguments and apply them as parameters to a function.
UnCurry is the opposite. it takes in two parameters and passes them in as
arguments into the function.

in the case of 'UnCurry(Curry(f)) = f'
if Curry(f) constructs f to take arguments as parameters, UnCurry(Curry(f)) takes
the parameters and deconstructs them back into arguments, resulting in f.

in the case of 'Curry(UnCurry(g)) = g'
simply the opposite. deconstructs parameters into arguments and then arguments
into parameters.

*)
(*5
sort has the type ('a * 'a -> bool) * 'a list -> 'a list.

sort takes in a list and a comparator function.
the comparator function takes in two arguments and returns a bool.
('a * 'a -> bool)

insert takes in the first element of sorts second argument, the list,
as its first argument and returns the modified list.
Therefore, inserts type is ('a * 'a list) -> 'a list

therefore, sort takes in a comparator, ('a * 'a -> bool), and a list, 'a list,
and returns the list sorted.

*)

(*6
fun f(g,h) = g(h) + 2

f(x,y) = (X * Y) -> Z
g(x) = int -> int
h = int

f(x,y) = (int -> int * int) -> int

*)

(*7
fun f(g) = g(g) + 2

f(g) = g -> int
g(x) = x -> int
g = x-> int

there is a mismatch error for g because it would need to take in two different
types, an int and a function. 

*)

(*8
fun append(x,y) is supposed to take two lists and return one, or 
'a list * 'b list -> 'c list

but append is breaking down the first list removing the head until it is nil
and then returns the second list back up the stack without actually appending
anything from the first list to the second.

'a list * 'b list -> 'b list.

however, plugging it into ml shows that it doesnt recognize the second element
as a list, which results in 'a list * 'b -> 'b. I think this is because there is
no indicator for the second parameter that says it is a list (because it's not
actually appending to it). it just returns whatever type that value is.
*)
