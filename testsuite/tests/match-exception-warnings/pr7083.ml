(* TEST
   * expect
*)

let f x =
  match x with
  | `A -> ()
  | exception Not_found -> ()
;;

[%%expect{|
val f : [< `A ] -> unit = <fun>
|}]
