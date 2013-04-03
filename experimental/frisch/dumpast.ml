(* Illustrate how to use AST lifting to create a pretty-printer *)

open Outcometree

class out_value_builder =
  object
    method record (_ty : string) x = Oval_record (List.map (fun (l, s) -> (Oide_ident l, s)) x)
    method constr (_ty : string) (c, args) = Oval_constr (Oide_ident c, args)
    method list x = Oval_list x
    method array x = Oval_list (Array.to_list x)
    method tuple x = Oval_tuple x
    method int x = Oval_int x
    method string x = Oval_string x
    method char x = Oval_char x
    method int32 x = Oval_int32 x
    method int64 x = Oval_int64 x
    method nativeint x = Oval_nativeint x
  end

let lift =
  object
    inherit [_] Ast_lifter.lifter
    inherit out_value_builder
    method! lift_Location_t _ = Oval_ellipsis
        (* Special mapping for the Location.t type *)
  end

let show lifter parse s =
  let v = lifter (parse (Lexing.from_string s)) in
  Format.printf "%s@.==>@.%a@.=========@." s !Oprint.out_value v

let show_expr = show (lift # lift_Parsetree_expression) Parse.expression
let show_pat = show (lift # lift_Parsetree_pattern) Parse.pattern

let args =
  let open Arg in
  [
   "-e", String show_expr,
   "<expr> Dump AST for expression <expr>.";

   "-p", String show_pat,
   "<pat> Dump AST for pattern <pat>."
  ]

let usage =
  Printf.sprintf "%s [options]\n" Sys.argv.(0)

let () =
  Arg.parse (Arg.align args) show_expr usage


