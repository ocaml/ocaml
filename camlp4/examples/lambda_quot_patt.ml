(* Please keep me in sync with brion.inria.fr/gallium/index.php/Lambda_calculus_quotations *)

open Camlp4.PreCast;;
module CamlSyntax = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax));;

let patt_of_string = CamlSyntax.Gram.parse_string CamlSyntax.patt_eoi;;

module LambdaGram = MakeGram(Lexer);;

let term = LambdaGram.Entry.mk "term";;
let term_eoi = LambdaGram.Entry.mk "lambda term quotation";;

Camlp4_config.antiquotations := true;;

EXTEND LambdaGram
  GLOBAL: term term_eoi;
  term:
    [ "top"
      [ "fun"; v = var; "->"; t = term -> <:patt< `Lam($v$, $t$) >> ]
    | "app"
      [ t1 = SELF; t2 = SELF           -> <:patt< `App($t1$, $t2$) >> ]
    | "simple"
      [ `ANTIQUOT((""|"term"), a)      -> patt_of_string _loc a
      | v = var                        -> <:patt< `Var($v$) >>
      | "("; t = term; ")"             -> t ]
    ];
  var:
    [[ v = LIDENT               -> <:patt< $str:v$ >>
     | `ANTIQUOT((""|"var"), a) -> patt_of_string _loc a
    ]];
  term_eoi:
    [[ t = term; `EOI -> t ]];
END;;

let expand_lambda_quot_patt loc _loc_name_opt quotation_contents =
  LambdaGram.parse_string term_eoi loc quotation_contents;;

(* function <:lam< fun x -> $(t|u)$ >> -> ... *)
Syntax.Quotation.add "lam" Syntax.Quotation.DynAst.patt_tag expand_lambda_quot_patt;;

Syntax.Quotation.default := "lam";;
