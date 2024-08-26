(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression Env.initial pe in
  let ute = Untypeast.untype_expression te in
  Format.printf "%a@." Pprintast.expression ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;
