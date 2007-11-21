open Camlp4.PreCast;;

let gen patts exprs =
  let cases =
    List.fold_right2 begin fun patt expr acc ->
      let _loc = Loc.merge (Ast.loc_of_patt patt) (Ast.loc_of_expr expr) in
      <:match_case< $patt$ -> $expr$ | $acc$ >>
    end patts exprs <:match_case@here<>>
  in
  let _loc = Ast.loc_of_match_case cases in
  <:expr< function $cases$ >>
;;
