(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id$ *)

open Pcaml;
open Pa_extend;

value sfold _loc n foldfun f e s =
  let styp = STquo _loc (new_type_var ()) in
  let e = <:expr< Extfold.$lid:foldfun$ $f$ $e$ >> in
  let t = STapp _loc (STapp _loc (STtyp <:ctyp< Extfold.t _ >>) s.styp) styp in
  {used = s.used; text = TXmeta _loc n [s.text] e t; styp = styp}
;

value sfoldsep _loc n foldfun f e s sep =
  let styp = STquo _loc (new_type_var ()) in
  let e = <:expr< Extfold.$lid:foldfun$ $f$ $e$ >> in
  let t =
    STapp _loc (STapp _loc (STtyp <:ctyp< Extfold.tsep _ >>) s.styp) styp
  in
  {used = s.used @ sep.used; text = TXmeta _loc n [s.text; sep.text] e t;
   styp = styp}
;

EXTEND
  GLOBAL: symbol;
  symbol: LEVEL "top"
    [ [ UIDENT "FOLD0"; f = simple_expr; e = simple_expr; s = SELF ->
          sfold _loc "FOLD0" "sfold0" f e s
      | UIDENT "FOLD1"; f = simple_expr; e = simple_expr; s = SELF ->
          sfold _loc "FOLD1" "sfold1" f e s
      | UIDENT "FOLD0"; f = simple_expr; e = simple_expr; s = SELF;
        UIDENT "SEP"; sep = symbol ->
          sfoldsep _loc "FOLD0 SEP" "sfold0sep" f e s sep
      | UIDENT "FOLD1"; f = simple_expr; e = simple_expr; s = SELF;
        UIDENT "SEP"; sep = symbol ->
          sfoldsep _loc "FOLD1 SEP" "sfold1sep" f e s sep ] ]
  ;
  simple_expr:
    [ [ i = LIDENT -> <:expr< $lid:i$ >>
      | "("; e = expr; ")" -> e ] ]
  ;
END;
