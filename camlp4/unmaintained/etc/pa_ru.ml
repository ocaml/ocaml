(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Pcaml;

value o2b =
  fun
  [ Some _ -> True
  | None -> False ]
;

EXTEND
  GLOBAL: expr;
  expr: LEVEL "top"
    [ [ "do"; "{"; seq = sequence; "}" ->
          match seq with
          [ [e] -> e
          | _ -> <:expr< do { $list:seq$ } >> ] ] ]
  ;
  sequence:
    [ [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; [ "in" | ";" ];
        el = SELF ->
          let e =
            match el with
            [ [e] -> e
            | _ -> <:expr< do { $list:el$ } >> ]
          in
          [ <:expr< let $opt:o2b o$ $list:l$ in $e$ >> ]
      | e = expr; ";"; el = SELF ->
          let e = let _loc = MLast.loc_of_expr e in <:expr< ($e$ : unit) >> in
          [e :: el]
      | e = expr; ";" -> [e]
      | e = expr -> [e] ] ]
  ;
END;
