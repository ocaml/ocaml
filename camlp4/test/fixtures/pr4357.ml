open Camlp4.PreCast

let sample_expr _loc _loc_name s =
  Printf.eprintf "file=%s line=%d offset=%d bol=%d\n%!"
    (Loc.file_name _loc)
    (Loc.start_line _loc)
    (Loc.start_off _loc)
    (Loc.start_bol _loc);
  <:expr< $lid:s$ >>
;;

Quotation.add
  "sample"
  Syntax.Quotation.DynAst.expr_tag
  sample_expr
;;
