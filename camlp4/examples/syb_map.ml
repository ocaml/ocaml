type variable = string
 and term =
  | Var   of variable
  | Lam   of variable * term
  | App   of term * term
  | Const of constant
 and constant =
  | CInt    of int
  | CString of string
 and program =
  | Def of string * term
  | Seq of program list
;;

class map = Camlp4Filters.GenerateMap.generated;;
(* class map = Camlp4MapGenerator.generated;; *)

let map_term f = object
  inherit map as super
  method term t = f (super#term t)
end;;

let map_term' f = object (self)
  inherit map as super
  method term t = f t self#term super#term
end;;

(* Suppress calls to the identity function... *)
let suppress_id =
  map_term begin function
    | App(Lam(v, Var(v')), t) when v = v' -> t
    | x -> x
  end;;

(* Substitute blindly all occurences of v by t *)
let raw_subst v t =
  map_term' begin fun t' _ next ->
    match t' with
    | Var(v') when v = v' -> t
    | x -> next x
  end;;

let id = Lam("x", Var"x");;
let _42 = Const(CInt 42);;
let prog =
  Seq[Def("foo", App(id, _42)); Def("bar", App(id, id))];;

let prog2 = suppress_id#program prog;;
let term3 = suppress_id#term (App(id, _42));;

let term4 = (raw_subst "x" _42)#term (App(Var"succ", Var"x"));;
