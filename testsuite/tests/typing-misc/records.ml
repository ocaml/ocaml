(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
fun {x=3;z=2} -> ();;

(* mixed labels *)
{x=3; contents=2};;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
fun x -> x.u <- 3;;
