open Camlp4.PreCast;
module CamlSyntax = Camlp4OCamlParser.Make (Camlp4OCamlRevisedParser.Make Syntax);

value expr_of_string = CamlSyntax.Gram.parse_string CamlSyntax.expr_eoi;

module LambdaGram = MakeGram Lexer;

value term = LambdaGram.Entry.mk "term";

EXTEND LambdaGram
  GLOBAL: term;
  term:
    [[ "fun"; v = var; "->"; t = term -> <:expr< `Lam $v$ $t$ >>
     | t1 = term; t2 = term           -> <:expr< `App $t1$ $t2$ >>
     | v = var                        -> <:expr< `Var $v$ >>
     | "("; t = term; ")"             -> t
     | `ANTIQUOT "" a                 -> expr_of_string _loc a
    ]];
  var:
    [[ v = LIDENT     -> <:expr< $str:v$ >>
     | `ANTIQUOT "" a -> expr_of_string _loc a
    ]];
END;

value expand_lambda_quot_expr loc _loc_name_opt quotation_contents =
  LambdaGram.parse_string term loc quotation_contents;

value expand_lambda_quot_patt loc _loc_name_opt _quotation_contents =
  Loc.raise loc (Failure "Patterns not supported");

Syntax.Quotation.add "lam"
  (Syntax.Quotation.ExAst (expand_lambda_quot_expr, expand_lambda_quot_patt));

Syntax.Quotation.default.val := "lam";
