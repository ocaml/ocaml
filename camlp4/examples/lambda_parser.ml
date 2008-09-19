(* Please keep me in sync with brion.inria.fr/gallium/index.php/Lambda_calculus_quotations *)

type term =
  | Lam of var * term
  | App of term * term
  | Int of int
  | Var of var
and var = string

module LambdaGram = Camlp4.PreCast.MakeGram(Camlp4.PreCast.Lexer);;
module Loc = Camlp4.PreCast.Loc;; (* should not be necessary when camlp4 will be fixed *)
open Camlp4.Sig;; (* from tokens *)
let term = LambdaGram.Entry.mk "term";;
let term_eoi = LambdaGram.Entry.mk "lambda term quotation";;

EXTEND LambdaGram
  GLOBAL: term term_eoi;
  term:
    [ "top"
      [ "fun"; v = var; "->"; t = term -> Lam(v, t) ]
    | "app"
      [ t1 = SELF; t2 = SELF           -> App(t1, t2) ]
    | "simple"
      [ v = var                        -> Var(v)
      | `INT(i, _)                     -> Int(i)
      | "("; t = term; ")"             -> t ]
    ];
  var:
    [[ `LIDENT v -> v ]];
  term_eoi:
    [[ t = term; `EOI -> t ]];
END;;

let lambda_parser = LambdaGram.parse_string term_eoi;;
