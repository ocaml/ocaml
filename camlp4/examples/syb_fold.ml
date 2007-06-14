type variable = string
 and term =
  | Var   of variable
  | Lam   of variable * term
  | App   of term * term
  | Const of constant
 and constant =
  | CInt    of int
  | CString of string
;;

class fold = Camlp4Filters.GenerateFold.generated;;
(* class fold = Camlp4FoldGenerator.generated;; *)

module VarSet = Set.Make(String);;

(* Compute free variables with the fold class *)
let free_variables_v1 =
  let o =
    object (self)
      inherit fold as super
      val fv = VarSet.empty
      method fv = fv
      method empty_fv = {< fv = VarSet.empty >}
      method term t =
        match t with
        | Var(v) -> {< fv = VarSet.add v fv >}
        | Lam(v, t) ->
            let fv1 = VarSet.remove v (self#empty_fv#term t)#fv in
            {< fv = VarSet.union fv fv1 >}
        | _ -> super#term t
    end
  in fun t -> VarSet.elements (o#term t)#fv
;;

(* Let's try to abstract that a little *)

let fold_term f t init =
  let o =
    object (self)
      inherit fold as super
      val acc = init
      method get = acc
      method reset = {< acc = init >}
      method term t =
        {< acc = f t acc (fun t -> (self#reset#term t)#get)
                         (fun t -> (super#term t)#get) >}
    end
  in
  (o#term t)#get
;;

(* A nicer version of free_variables *)
let free_variables_v2 t =
  VarSet.elements begin
    fold_term begin fun t fv self next ->
      match t with
      | Var(v)    -> VarSet.add v fv
      | Lam(v, t) -> VarSet.union fv (VarSet.remove v (self t))
      | _         -> next t
    end t VarSet.empty
  end
;;

let term1 =
  App(
    App(Var"x1",
      Lam("x",
        App(Var"x", App(Var"y", (Lam("y", Lam("z", (App(Var"y", App(Var"x4",Var"z")))))))))),
    Var"x3")

;;

let fv1 = free_variables_v1 term1;;
let fv2 = free_variables_v2 term1;;

(* Low cost syntax *)
let ( ^-> ) v t = Lam(v, t)
let ( @ ) t1 t2 = App(t1, t2)
let ( ! ) s = Var s

let term2 =
  !"x1" @
  ("x" ^-> !"x" @ !"y" @ ("y" ^-> ("z" ^-> !"y" @ !"x4" @ !"z"))) @
  !"x3"

;;

let fv1' = free_variables_v1 term2;;
let fv2' = free_variables_v2 term2;;
