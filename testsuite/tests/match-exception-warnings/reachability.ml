(* TEST
   * expect
*)

let f x =
  match x with
  | _ -> ()
  | exception _ -> .
;;

[%%expect{|
Line _, characters 14-15:
    | exception _ -> .
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
Line _, characters 11-22:
    | None | exception _ -> .
             ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;


let f x =
  match x with
  | _ -> ()
  | exception Not_found | None -> .
;;


[%%expect{|
Line _, characters 4-23:
    | exception Not_found | None -> .
      ^^^^^^^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x with
  | _ | exception _ -> ()
  | exception Not_found -> .
;;

[%%expect{|
Line _, characters 8-19:
    | _ | exception _ -> ()
          ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;
