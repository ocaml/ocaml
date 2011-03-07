(* Please keep me in sync with brion.inria.fr/gallium/index.php/Lambda_calculus_quotations *)

open Camlp4.PreCast;;
module CamlSyntax = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax));;

let expr_of_string = CamlSyntax.Gram.parse_string CamlSyntax.expr_eoi;;

module LambdaGram = MakeGram(Lexer);;

let term = LambdaGram.Entry.mk "term";;
let term_eoi = LambdaGram.Entry.mk "lambda term quotation";;

Camlp4_config.antiquotations := true;;

EXTEND LambdaGram
  GLOBAL: term term_eoi;
  term:
    [ "top"
      [ "fun"; v = var; "->"; t = term -> <:expr< `Lam($v$, $t$) >> ]
    | "app"
      [ t1 = SELF; t2 = SELF           -> <:expr< `App($t1$, $t2$) >> ]
    | "simple"
      [ `ANTIQUOT((""|"term"), a)      -> expr_of_string _loc a
      | v = var                        -> <:expr< `Var($v$) >>
      | "("; t = term; ")"             -> t ]
    ];
  var:
    [[ v = LIDENT               -> <:expr< $str:v$ >>
     | `ANTIQUOT((""|"var"), a) -> expr_of_string _loc a
    ]];
  term_eoi:
    [[ t = term; `EOI -> t ]];
END;;

let expand_lambda_quot_expr loc _loc_name_opt quotation_contents =
  LambdaGram.parse_string term_eoi loc quotation_contents;;

(* to have this syntax <:lam< fun k -> k >> *)
Syntax.Quotation.add "lam" Syntax.Quotation.DynAst.expr_tag expand_lambda_quot_expr;;

Syntax.Quotation.default := "lam";;
