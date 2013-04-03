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
    inherit [_] Lifter.lifter
    inherit out_value_builder
    method! lift_Location_t _ = Oval_ellipsis
        (* Special mapping for the Location.t type *)
  end

let e =
  Parse.expression (Lexing.from_string "fun x -> 1 + 3 * x")

let () =
  Format.printf "%a@." !Oprint.out_value
    (lift # lift_Parsetree_expression e)


