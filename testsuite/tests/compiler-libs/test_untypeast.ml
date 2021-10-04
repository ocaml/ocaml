(* TEST
   flags = "-I ${ocamlsrcdir}/typing \
    -I ${ocamlsrcdir}/parsing"
   include ocamlcommon
   * expect
*)

let res =
  let s = {| match None with Some (Some _) -> () | _ -> () |} in
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression (Env.initial_safe_string) pe in
  let ute = Untypeast.untype_expression te in
  Format.asprintf "%a" Pprintast.expression ute

[%%expect{|
val res : string = "match None with | Some (Some _) -> () | _ -> ()"
|}]
