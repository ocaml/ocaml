(* module LambdaSyntax = struct
  module Loc = Camlp4.PreCast.Loc
  type 'a antiquotable =
    | Val of Loc.t * 'a
    | Ant of Loc.t * string
  type term' =
    | Lam of var * term
    | App of term * term
    | Var of var
    | Int of int antiquotable
    |+ Why you don't want an antiquotation case here:
     *   Basically it seems natural that since an antiquotation of expression
     *   can be at any expression place. One can be a
     *   .... in fact not I not against that...
    | Anti of Loc.t * string
     +|
  and term = term' antiquotable
  and var = string antiquotable
end                                                                              *)
module Antiquotable = struct
  module Loc = Camlp4.PreCast.Loc
  type 'a t =
    | Val of Loc.t * 'a
    | Ant of Loc.t * string
end
module Identity_type_functor = struct
  type 'a t = 'a
end
module MakeLambdaSyntax(Node : sig type 'a t end) = struct
  type term' =
    | Lam of var * term
    | App of term * term
    | Var of var
    | Int of num
  and term = term'  Node.t
  and num  = int    Node.t
  and var  = string Node.t
end
module AntiquotableLambdaSyntax = MakeLambdaSyntax(Antiquotable);;
module LambdaSyntax = MakeLambdaSyntax(Identity_type_functor);;
module LambdaParser = struct
  open Antiquotable;;
  open AntiquotableLambdaSyntax;;
  open Camlp4.PreCast;;

  module LambdaGram = MakeGram(Lexer);;

  let term = LambdaGram.Entry.mk "term";;
  let term_eoi = LambdaGram.Entry.mk "lambda term quotation";;

  Camlp4_config.antiquotations := true;;

  let mkLam _loc v t = Val(_loc, Lam(v, t));;
  let mkApp _loc f x = Val(_loc, App(f, x));;
  let mkVar _loc x   = Val(_loc, Var(x));;
  let mkInt _loc v   = Val(_loc, Int(v));;

  EXTEND LambdaGram
    GLOBAL: term term_eoi;
    term:
      [ "top"
        [ "fun"; v = var; "->"; t = term -> mkLam _loc v t ]
      | "app"
        [ t1 = SELF; t2 = SELF           -> mkApp _loc t1 t2 ]
      | "simple"
        [ `ANTIQUOT((""|"term"), a)      -> Ant(_loc, a)
        | i = int                        -> mkInt _loc i
        | v = var                        -> mkVar _loc v
        | "("; t = term; ")"             -> t ]
      ];
    var:
      [[ v = LIDENT              -> Val(_loc, v)
      | `ANTIQUOT((""|"var"), a) -> Ant(_loc, a)
      ]];
    int:
      [[ `INT(i, _)              -> Val(_loc, i)
      | `ANTIQUOT((""|"int"), a) -> Ant(_loc, a)
      ]];
    term_eoi:
      [[ t = term; `EOI -> t ]];
  END;;

  let parse_string = LambdaGram.parse_string term_eoi
end
module LambdaLifter = struct
  open Antiquotable;;
  open AntiquotableLambdaSyntax;;
  module CamlSyntax =
    Camlp4OCamlParser.Make(
      Camlp4OCamlRevisedParser.Make(
        Camlp4.PreCast.Syntax
      )
    );;
  module Ast = Camlp4.PreCast.Ast
  let expr_of_string = CamlSyntax.Gram.parse_string CamlSyntax.expr_eoi;;
  let patt_of_string = CamlSyntax.Gram.parse_string CamlSyntax.patt_eoi;;

  (*
  << fun x -> $3$ >> -> Lam(VAtom"x", 3)

  (* compilo.ml -pp lam.cmo *)
  match t with
  | << (fun $x$ -> $e1$) $e2$ >> -> << $subst ...$ >>
  *)

  (* This part can be generated use SwitchValRepr *)
  let rec term_to_expr = function
    | Val(_loc, Lam(v, t))   -> <:expr< Lam($var_to_expr v$, $term_to_expr t$) >>
    | Val(_loc, App(t1, t2)) -> <:expr< App($term_to_expr t1$, $term_to_expr t2$) >>
    | Val(_loc, Var(v))      -> <:expr< Var($var_to_expr v$) >>
    | Val(_loc, Int(i))      -> <:expr< Int($int_to_expr i$) >>
    | Ant(_loc, a)           -> expr_of_string _loc a
  and var_to_expr = function
    | Val(_loc, v) -> <:expr< $str:v$ >>
    | Ant(_loc, s) -> expr_of_string _loc s
  and int_to_expr = function
    | Val(_loc, v) -> <:expr< $`int:v$ >>
    | Ant(_loc, s) -> expr_of_string _loc s
  ;;

  let rec term_to_patt = function
    | Val(_loc, Lam(v, t)) -> <:patt< Lam($var_to_patt v$, $term_to_patt t$) >>
    | Val(_loc, App(t1, t2)) -> <:patt< App($term_to_patt t1$, $term_to_patt t2$) >>
    | Val(_loc, Var(v)) -> <:patt< Var($var_to_patt v$) >>
    | Val(_loc, Int(i)) -> <:patt< Int($int_to_patt i$) >>
    | Ant(_loc, a) -> patt_of_string _loc a
  and var_to_patt = function
    | Val(_loc, v) -> <:patt< $str:v$ >>
    | Ant(_loc, s) -> patt_of_string _loc s
  and int_to_patt = function
    | Val(_loc, v) -> <:patt< $`int:v$ >>
    | Ant(_loc, s) -> patt_of_string _loc s
  ;;

    (*
Arrow(Var"a", Var"b")
<:typ< 'a -> 'b >>

  let a = ...
  let b = ...
  let ( ^-> ) t1 t2 = Arrow(t1, t2)
  a ^-> b
  *)
end
module LambadExpander = struct
  module Q = Camlp4.PreCast.Syntax.Quotation;;
  let expand_lambda_quot_expr loc _loc_name_opt quotation_contents =
    LambdaLifter.term_to_expr
      (LambdaParser.parse_string loc quotation_contents)
  ;;
  Q.add "lam" Q.DynAst.expr_tag expand_lambda_quot_expr;;
  let expand_lambda_quot_patt loc _loc_name_opt quotation_contents =
    LambdaLifter.term_to_patt
      (LambdaParser.parse_string loc quotation_contents)
  ;;
  Q.add "lam" Q.DynAst.patt_tag expand_lambda_quot_patt;;

  Q.default := "lam";;
end
