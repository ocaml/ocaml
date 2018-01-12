(* TEST
   * expect
*)
exception Exit

let r = ref ""

let guarded f =
  match f () with
  | true | exception Exit when r := "hello"; true -> !r
  | _ -> "other"
;;

[%%expect{|
exception Exit
val r : string ref = {contents = ""}
Line _, characters 11-25:
    | true | exception Exit when r := "hello"; true -> !r
             ^^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
