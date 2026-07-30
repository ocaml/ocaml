(* TEST
 toplevel;
*)

(* Record the treatment of the wildcard "_" in expression positions. *)

(* Section 1: forms involving "_" which are valid. Note that the lexer
   already accepts "_" as a label name in the unspaced forms "~_:" and
   "?_:". *)

let _ = 1;;

let h (_ : int) (y : _) = y;;

let g ~_:x = x;;

g ~_:3;;

let o ?_:(x = 0) () = x;;

o ?_:(Some 5) ();;

(* Section 2: "_" where a general expression can start. The parser
   recognizes these forms in order to report an ad hoc error. *)

let x = _;;

(_, 0);;

type r = { a : int };;

{ a = _ };;

1 + _;;

if _ then 0 else 1;;

(_ : int);;

fun () -> _;;

_.a;;

(* Section 3: "_" where only a simple expression is allowed
   (function-argument positions). *)

_ 0;;

let f x = x;;

f _;;

Some _;;

lazy _;;

f ~x:_;;

g ~_;;

g ~_:_;;

o ?_;;

o ?_:_;;

(* Section 4: forms which are always syntax errors. *)

let x = ~_;;

f ~ _ : 3;;
