(* camlp4r *)


type spc =
  [ SPCterm of (MLast.patt * option MLast.expr)
  | SPCnterm of MLast.patt and MLast.expr
  | SPCsterm of MLast.patt ]
;

value parser_of_expr :
  MLast.expr ->
    list (list (spc * option MLast.expr) * option MLast.patt * MLast.expr);
