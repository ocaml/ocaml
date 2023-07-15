(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression Env.initial pe in
  let ute = Untypeast.untype_expression te in
  Format.asprintf "%a" Pprintast.expression ute
;;

[%%expect{|
val run : string -> string = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
- : string = "match None with | Some (Some _) -> () | _ -> ()"
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
- : string = "fun x y z -> function | w -> x y z w"
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
- : string = "fun x y z -> (function | w -> x y z w)"
|}];;
