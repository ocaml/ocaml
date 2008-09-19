#load "camlp4of.cma";;
open Camlp4.PreCast;;
module M = Ast.Meta.Make(Ast.Meta.MetaGhostLoc);;
let ghost = Loc.ghost;;
M.Expr.meta_ctyp ghost <:ctyp@ghost< int >>;;
