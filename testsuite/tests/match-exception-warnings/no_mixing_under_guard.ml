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
Line _, characters 4-25:
    | true | exception Exit when r := "hello"; true -> !r
      ^^^^^^^^^^^^^^^^^^^^^
Error: Mixing value and exception patterns under when-guards is not supported.
|}]
