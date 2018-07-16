(* TEST
   * expect
*)

let test f =
  match f () with exception Not_found -> ()
;;

[%%expect{|
Line _, characters 2-43:
    match f () with exception Not_found -> ()
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: None of the patterns in this 'match' expression match values.
|}]
;;
