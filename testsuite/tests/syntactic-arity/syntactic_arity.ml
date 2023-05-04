(* TEST *)

(* A few simple tests to demonstrate basic properties of arity.
   For more exhaustive tests, look at [measure_arity.ml].
*)

let def fun_name param_name =
  Printf.printf "Evaluating param %s to %s.\n" param_name fun_name

(* No optional argument defaults are evaluated until the function is
   fully saturated a number of arguments equalling its syntactic arity.
*)
let f ?opt1:(() = def __FUNCTION__ "opt1") ()
      ?opt2:(() = def __FUNCTION__ "opt2") () = ();;

print_endline "f, 1 argument:";;
f ();;
print_endline "f, 2 arguments:";;
f () ();;

(* The arity is the same even in the presence of active patterns, e.g. lazy. *)
let f_lazy ?opt1:(() = def __FUNCTION__ "opt1") (lazy ())
      ?opt2:(() = def __FUNCTION__ "opt2") () = ();;

print_endline "f_lazy, 1 argument:";;
f_lazy (lazy ());;
print_endline "f_lazy, 2 arguments:";;
f_lazy (lazy ()) ();;

(* Mutable fields aren't read until the function is fully saturated.  *)
let g { contents = x1 } { contents = x2 } () = x1 + x2
let r = ref 1;;
let g' = g r;;
r := 2;;
let g'' = g' r;;
r := 3;;
let result = g'' ();;
assert (result = 6);;

(* Patterns and optional arguments are considered left-to-right to preserve
   binding structure.
*)
type t = ..;;
module type S = sig
  type t += E
end;;

module M1 : S = struct
  type t += E
end;;

module M2 : S = struct
  type t += E
end;;

let binding1 (module M : S) ?(opt = M.E) () = opt;;

match binding1 (module M1) () with
| M1.E -> ()
| _ -> assert false;;

let binding2 ?opt:((module M) = (module M1 : S)) M.E = ()
[@@ocaml.warning "-8"];;

binding2 M1.E;;
binding2 ~opt:(module M2) M2.E;;

match binding2 M2.E with
| exception Match_failure _ -> ()
| _ -> assert false;;
