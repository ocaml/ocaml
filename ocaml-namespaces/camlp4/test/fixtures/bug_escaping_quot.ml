open Camlp4.PreCast;;
Camlp4_config.antiquotations := true;;
let expand_my_quot_expr _loc _loc_name_opt quotation_contents =
  Printf.eprintf "%S\n%!" quotation_contents;
  <:expr< dummy >>
;;
Syntax.Quotation.add "my" Syntax.Quotation.DynAst.expr_tag expand_my_quot_expr;;
