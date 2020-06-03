(* TEST
   * expect
*)

let test f =
  match f () with exception Not_found -> ()
;;

[%%expect{|
Line 2, characters 2-43:
2 |   match f () with exception Not_found -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: None of the patterns in this 'match' expression match values.
|}]
;;
