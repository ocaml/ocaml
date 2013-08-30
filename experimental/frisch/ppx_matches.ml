(*
  Example : List.filter [%matches ? 'a' .. 'z' ] text
  Output : List.filter  (function 'a' .. 'z' -> true | _ -> false) text
*)

open Asttypes
open Parsetree
open Ast_helper

let mapper =
  object(this)
    inherit Ast_mapper.mapper as super

    method! expr e =
      match e.pexp_desc with
      | Pexp_extension({txt="matches";_}, PPat (p, guard)) ->
        let p = this # pat p in
        let guard = Ast_mapper.map_opt (this # expr) guard in
        Exp.function_ ~loc:e.pexp_loc
            [
              Exp.case p ?guard (Convenience.constr "true" []);
              Exp.case (Pat.any ()) (Convenience.constr "false" []);
            ]
      | _ -> super#expr e
  end

let () = Ast_mapper.main mapper
