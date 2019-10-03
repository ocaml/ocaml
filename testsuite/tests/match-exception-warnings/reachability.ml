(* TEST
   * expect
*)

let f x =
  match x with
  | _ -> ()
  | exception _ -> .
;;

[%%expect{|
Line 4, characters 14-15:
4 |   | exception _ -> .
                  ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: _
|}]
;;

let f x =
  match x with
  | _ -> ()
  | None | exception _ -> .
;;

[%%expect{|
Line 4, characters 21-22:
4 |   | None | exception _ -> .
                         ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: _
|}]
;;


let f x =
  match x with
  | _ -> ()
  | exception Not_found | None -> .
;;


[%%expect{|
Line 4, characters 14-23:
4 |   | exception Not_found | None -> .
                  ^^^^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: Not_found
|}]
;;

let f x =
  match x with
  | _ | exception _ -> ()
  | exception Not_found -> .
;;

[%%expect{|
val f : 'a -> unit = <fun>
|}]
;;
