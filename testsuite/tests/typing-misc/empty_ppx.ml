module H = Ast_helper
module M = Ast_mapper
open Parsetree
let empty_polyvar loc = H.Typ.variant ~loc [] Asttypes.Closed None

let super = M.default_mapper
let typ mapper e =
  match e.ptyp_desc with
  | Ptyp_extension ({txt="empty_polyvar";loc},_) -> empty_polyvar loc
  | _ -> super.M.typ mapper e

let () = M.register "empty ppx" (fun _ ->
    { super with typ }
  )
