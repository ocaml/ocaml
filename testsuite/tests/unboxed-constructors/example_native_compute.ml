(* TEST
   flags = "-dlambda -dno-locations -dcanonical-ids";
   expect;
*)

type t =
  | Var of int [@unboxed]
  | App of t * t
  | Lam of (t -> t) [@unboxed]
[%%expect {|
0
type t = Var of int [@unboxed] | App of t * t | Lam of (t -> t) [@unboxed]
|}]

let app t v =
  match t with
  | Lam f -> f v
  | ne -> App (ne, v)
[%%expect {|
(let
  (app/0 =
     (function t/0 v/0
       (catch
         (if (isint t/0) (exit 2)
           (if (>= (caml_obj_tag t/0) 247) (apply t/0 v/0) (exit 2)))
        with (2) (makeblock 0 t/0 v/0))))
  (apply (field_mut 1 (global Toploop!)) "app" app/0))
val app : t -> t -> t = <fun>
|}]
