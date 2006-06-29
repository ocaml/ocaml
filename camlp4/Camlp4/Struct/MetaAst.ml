(* Generated file, do not edit by hand! *)
(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright   2006    Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)


(* Authors:
 * - Nicolas Pouillard: initial version
 *)


module type META_LOC = sig
  module Ast : Sig.Camlp4Ast.S;
  value meta_loc_patt : Ast.Loc.t -> Ast.patt;
  value meta_loc_expr : Ast.Loc.t -> Ast.expr;
end;


module MetaLoc (Ast : Sig.Camlp4Ast.S) = struct
  module Ast = Ast;
  value meta_loc_patt _loc =
    let (a, b, c, d, e, f, g, h) = Ast.Loc.to_tuple _loc in
    <:patt< Loc.of_tuple
              ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
               $`int:e$, $`int:f$, $`int:g$,
               $if h then <:patt< True >> else <:patt< False >> $) >>;
  value meta_loc_expr _loc =
    let (a, b, c, d, e, f, g, h) = Ast.Loc.to_tuple _loc in
    <:expr< Loc.of_tuple
              ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
               $`int:e$, $`int:f$, $`int:g$,
               $if h then <:expr< True >> else <:expr< False >> $) >>;
end;


module MetaGhostLoc (Ast : Sig.Camlp4Ast.S) = struct
  module Ast = Ast;
  value meta_loc_patt _loc = <:patt< Loc.ghost >>;
  value meta_loc_expr _loc = <:expr< Loc.ghost >>;
end;


module MetaLocVar (Ast : Sig.Camlp4Ast.S) = struct
  module Ast = Ast;
  value meta_loc_patt _loc = <:patt< $lid:Loc.name.val$ >>;
  value meta_loc_expr _loc = <:expr< $lid:Loc.name.val$ >>;
end;


module Make (MetaLoc : META_LOC) = struct
  open MetaLoc;
  open Ast;

  module Expr = struct

    value rec meta_list _loc mf =
      fun
      [ [] -> <:expr< [] >>
      | [x :: xs] -> <:expr< [ $mf x$ :: $meta_list _loc mf xs$ ] >> ];
  
    value meta_bool _loc =
      fun   
      [ Ast.BFalse -> <:expr< Ast.BFalse >>
      | Ast.BTrue -> <:expr< Ast.BTrue >>
      | Ast.BAnt s -> <:expr< $anti:s$ >> ]; 
  
    value meta_chr _ x = x;
    value meta_flo _ x = x;
    value meta_int _ x = x;
    value meta_lid _ x = x;
    value meta_uid _ x = x;
    value meta_str _loc s = <:expr< $`str:s$ >>;
    value meta_s _loc s = <:expr< $str:s$ >>;
    value meta_c x = x;
    value meta_b = meta_bool;
    value meta_to = meta_bool;
  
    value rec meta_e x = expr x
    and meta_bi x = binding x
    and meta_a x = assoc x
    and meta_mb x = module_binding x
    and meta_p x = patt x
    and meta_t x = ctyp x
    and meta_mt x = module_type x
    (* and meta_sl _loc x = meta_list _loc meta_s x *)
    and meta_tt _loc (t1, t2) = <:expr< ($ctyp t1$, $ctyp t2$) >>
    and meta_st x = str_item x
    and meta_sg x = sig_item x
    and meta_me x = module_expr x
    and meta_ce x = class_expr x
    and meta_ct x = class_type x
    and meta_csg x = class_sig_item x
    and meta_cst x = class_str_item x
    and meta_wc x = with_constr x
  
    and with_constr =
    fun
    [ <:with_constr@_loc<>>                                             -> <:expr< Ast.WcNil $meta_loc_expr _loc$ >>
    | <:with_constr@_loc< type $t1$ = $t2$ >>                           -> <:expr< Ast.WcTyp $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:with_constr@_loc< $wc1$ and $wc2$ >>                            -> <:expr< Ast.WcAnd $meta_loc_expr _loc$ $meta_wc wc1$ $meta_wc wc2$ >>
    | <:with_constr@_loc< module $i1$ = $i2$ >>                         -> <:expr< Ast.WcMod $meta_loc_expr _loc$ $meta_i i1$ $meta_i i2$ >>
    | <:with_constr@_loc< $anti:s$ >>                                   -> <:expr< $anti:s$ >> ]
  
    and ident = fun
    [ <:ident@_loc< $i1$.$i2$ >>                                        -> <:expr< Ast.IdAcc $meta_loc_expr _loc$ $meta_i i1$ $meta_i i2$ >>
    | <:ident@_loc< $i1$ $i2$ >>                                        -> <:expr< Ast.IdApp $meta_loc_expr _loc$ $meta_i i1$ $meta_i i2$ >>
    | <:ident@_loc< $anti:s$ >>                                         -> <:expr< $anti:s$ >>
    | <:ident@_loc< $lid:s$ >>                                          -> <:expr< Ast.IdLid $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:ident@_loc< $uid:s$ >>                                          -> <:expr< Ast.IdUid $meta_loc_expr _loc$ $meta_s _loc s$ >> ]
    and meta_i i = ident i
  
    and expr = fun
    [ <:expr@_loc<>>                                                    -> <:expr< Ast.ExNil $meta_loc_expr _loc$ >>
    | <:expr@_loc< $id:i$ >>                                            -> <:expr< Ast.ExId $meta_loc_expr _loc$ $meta_i i$ >>
    | <:expr@_loc< $anti:s$ >>                                          -> <:expr< $anti:s$ >>
    | <:expr@_loc< $e1$ $e2$ >>                                         -> <:expr< Ast.ExApp $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $e1$ .( $e2$ ) >>                                    -> <:expr< Ast.ExAre $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< [| $e$ |] >>                                         -> <:expr< Ast.ExArr $meta_loc_expr _loc$ $meta_e e$ >>
    | <:expr@_loc< $e1$ := $e2$ >>                                      -> <:expr< Ast.ExAss $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $chr:c$ >>                                           -> <:expr< Ast.ExChr $meta_loc_expr _loc$ $meta_s _loc c$ >>
    | <:expr@_loc< ($e$ :> $t$) >>                                      -> <:expr< Ast.ExCoe $meta_loc_expr _loc$ $meta_e e$ (Ast.TyNil $meta_loc_expr _loc$) $meta_t t$ >>
    | <:expr@_loc< ($e$ : $t1$ :> $t2$) >>                              -> <:expr< Ast.ExCoe $meta_loc_expr _loc$ $meta_e e$ $meta_t t1$ $meta_t t2$ >>
    | <:expr@_loc< $flo:s$ >>                                           -> <:expr< Ast.ExFlo $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< for $s$ = $e1$ $to:b$ $e2$ do { $e3$ } >>            -> <:expr< Ast.ExFor $meta_loc_expr _loc$ $meta_s _loc s$ $meta_e e1$ $meta_e e2$ $meta_to _loc b$ $meta_e e3$ >>
    | <:expr@_loc< fun [ $a$ ] >>                                       -> <:expr< Ast.ExFun $meta_loc_expr _loc$ $meta_a a$ >>
    | <:expr@_loc< if $e1$ then $e2$ else $e3$ >>                       -> <:expr< Ast.ExIfe $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ $meta_e e3$ >>
    | <:expr@_loc< $int:s$ >>                                           -> <:expr< Ast.ExInt $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< lazy $e$ >>                                          -> <:expr< Ast.ExLaz $meta_loc_expr _loc$ $meta_e e$ >>
    | <:expr@_loc< let $opt:r$ $bi$ in $e$ >>                           -> <:expr< Ast.ExLet $meta_loc_expr _loc$ $meta_bool _loc r$ $meta_bi bi$ $meta_e e$ >>
    | <:expr@_loc< match $e$ with [ $a$ ] >>                            -> <:expr< Ast.ExMat $meta_loc_expr _loc$ $meta_e e$ $meta_a a$ >>
    | <:expr@_loc< { $bi$ } >>                                          -> <:expr< Ast.ExRec $meta_loc_expr _loc$ $meta_bi bi$ (Ast.ExNil $meta_loc_expr _loc$) >>
    | <:expr@_loc< { ($e$) with $bi$ } >>                               -> <:expr< Ast.ExRec $meta_loc_expr _loc$ $meta_bi bi$ $meta_e e$ >>
    | <:expr@_loc< do { $e$ } >>                                        -> <:expr< Ast.ExSeq $meta_loc_expr _loc$ $meta_e e$ >>
    | <:expr@_loc< $e1$ .[ $e2$ ] >>                                    -> <:expr< Ast.ExSte $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $str:s$ >>                                           -> <:expr< Ast.ExStr $meta_loc_expr _loc$ $meta_str _loc s$ >>
    | <:expr@_loc< try $e$ with [ $a$ ] >>                              -> <:expr< Ast.ExTry $meta_loc_expr _loc$ $meta_e e$ $meta_a a$ >>
    | <:expr@_loc< ( $tup:e$ ) >>                                       -> <:expr< Ast.ExTup $meta_loc_expr _loc$ $meta_e e$ >>
    | <:expr@_loc< ( $e$ : $t$ ) >>                                     -> <:expr< Ast.ExTyc $meta_loc_expr _loc$ $meta_e e$ $meta_t t$ >>
    | <:expr@_loc< while $e1$ do { $e2$ } >>                            -> <:expr< Ast.ExWhi $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< `$s$ >>                                              -> <:expr< Ast.ExVrn $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< $e$ # $s$ >>                                         -> <:expr< Ast.ExSnd $meta_loc_expr _loc$ $meta_e e$ $meta_s _loc s$ >>
    | <:expr@_loc< ~ $s$ >>                                             -> <:expr< Ast.ExLab $meta_loc_expr _loc$ $meta_s _loc s$ (Ast.ExNil $meta_loc_expr _loc$) >>
    | <:expr@_loc< ~ $s$ : $e$ >>                                       -> <:expr< Ast.ExLab $meta_loc_expr _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:expr@_loc< ? $s$ >>                                             -> <:expr< Ast.ExOlb $meta_loc_expr _loc$ $meta_s _loc s$ (Ast.ExNil $meta_loc_expr _loc$) >>
    | <:expr@_loc< ? $s$ : $e$ >>                                       -> <:expr< Ast.ExOlb $meta_loc_expr _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:expr@_loc< assert $e$ >>                                        -> <:expr< Ast.ExAsr $meta_loc_expr _loc$ $meta_e e$ >>
    | <:expr@_loc< new $i$ >>                                           -> <:expr< Ast.ExNew $meta_loc_expr _loc$ $meta_i i$ >>
    | <:expr@_loc< {< $bi$ >} >>                                        -> <:expr< Ast.ExOvr $meta_loc_expr _loc$ $meta_bi bi$ >>
    | <:expr@_loc< let module $s$ = $me$ in $e$ >>                      -> <:expr< Ast.ExLmd $meta_loc_expr _loc$ $meta_s _loc s$ $meta_me me$ $meta_e e$ >>
    | <:expr@_loc< assert False >>                                      -> <:expr< Ast.ExAsf $meta_loc_expr _loc$ >>
    | <:expr@_loc< $int32:s$ >>                                         -> <:expr< Ast.ExInt32 $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< $int64:s$ >>                                         -> <:expr< Ast.ExInt64 $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< $nativeint:s$ >>                                     -> <:expr< Ast.ExNativeInt $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< object ($p$) $cst$ end >>                            -> <:expr< Ast.ExObj $meta_loc_expr _loc$ $meta_p p$ $meta_cst cst$ >>
    | <:expr@_loc< $e1$, $e2$ >>                                        -> <:expr< Ast.ExCom $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $e1$ . $e2$ >>                                       -> <:expr< Ast.ExAcc $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $e1$; $e2$ >>                                        -> <:expr< Ast.ExSem $meta_loc_expr _loc$ $meta_e e1$ $meta_e e2$ >> ]
  
    and patt = fun
    [ <:patt@_loc<>>                                                    -> <:expr< Ast.PaNil $meta_loc_expr _loc$ >>
    | <:patt@_loc< $id:i$ >>                                            -> <:expr< Ast.PaId $meta_loc_expr _loc$ $meta_i i$ >>
    | <:patt@_loc< $anti:s$ >>                                          -> <:expr< $anti:s$ >>
    | <:patt@_loc< ( $p1$ as $p2$ ) >>                                  -> <:expr< Ast.PaAli $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< _ >>                                                 -> <:expr< Ast.PaAny $meta_loc_expr _loc$ >>
    | <:patt@_loc< $p1$ $p2$ >>                                         -> <:expr< Ast.PaApp $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $chr:c$ >>                                           -> <:expr< Ast.PaChr $meta_loc_expr _loc$ $meta_s _loc c$ >>
    | <:patt@_loc< $int:s$ >>                                           -> <:expr< Ast.PaInt $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< $p1$ | $p2$ >>                                       -> <:expr< Ast.PaOrp $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $p1$ .. $p2$ >>                                      -> <:expr< Ast.PaRng $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< { $p$ } >>                                           -> <:expr< Ast.PaRec $meta_loc_expr _loc$ $meta_p p$ >>
    | <:patt@_loc< $str:s$ >>                                           -> <:expr< Ast.PaStr $meta_loc_expr _loc$ $meta_str _loc s$ >>
    | <:patt@_loc< ( $tup:p$ ) >>                                       -> <:expr< Ast.PaTup $meta_loc_expr _loc$ $meta_p p$ >>
    | <:patt@_loc< ( $p$ : $t$ ) >>                                     -> <:expr< Ast.PaTyc $meta_loc_expr _loc$ $meta_p p$ $meta_t t$ >>
    | <:patt@_loc< $int32:s$ >>                                         -> <:expr< Ast.PaInt32 $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< $int64:s$ >>                                         -> <:expr< Ast.PaInt64 $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< $nativeint:s$ >>                                     -> <:expr< Ast.PaNativeInt $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< `$s$ >>                                              -> <:expr< Ast.PaVrn $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< [| $p$ |] >>                                         -> <:expr< Ast.PaArr $meta_loc_expr _loc$ $meta_p p$ >>
    | <:patt@_loc< $flo:f$ >>                                           -> <:expr< Ast.PaFlo $meta_loc_expr _loc$ $meta_s _loc f$ >>
    | <:patt@_loc< ~ $s$ >>                                             -> <:expr< Ast.PaLab $meta_loc_expr _loc$ $meta_s _loc s$ (Ast.PaNil $meta_loc_expr _loc$) >>
    | <:patt@_loc< ? $s$ >>                                             -> <:expr< Ast.PaOlb $meta_loc_expr _loc$ $meta_s _loc s$ (Ast.PaNil $meta_loc_expr _loc$) >>
    | <:patt@_loc< ~ $s$ : ($p$) >>                                     -> <:expr< Ast.PaLab $meta_loc_expr _loc$ $meta_s _loc s$ $meta_p p$ >>
    | <:patt@_loc< ? $s$ : ($p$) >>                                     -> <:expr< Ast.PaOlb $meta_loc_expr _loc$ $meta_s _loc s$ $meta_p p$ >>
    | <:patt@_loc< ? $s$ : ($p$ = $e$) >>                               -> <:expr< Ast.PaOlbi $meta_loc_expr _loc$ $meta_s _loc s$ $meta_p p$ $meta_e e$ >>
    | <:patt@_loc< # $i$ >>                                             -> <:expr< Ast.PaTyp $meta_loc_expr _loc$ $meta_i i$ >>
    | <:patt@_loc< $p1$, $p2$ >>                                        -> <:expr< Ast.PaCom $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $p1$ = $p2$ >>                                       -> <:expr< Ast.PaEq $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $p1$; $p2$ >>                                        -> <:expr< Ast.PaSem $meta_loc_expr _loc$ $meta_p p1$ $meta_p p2$ >> ]
  
    and assoc = fun
    [ <:assoc@_loc<>>                                                   -> <:expr< Ast.AsNil $meta_loc_expr _loc$ >>
    | <:assoc@_loc< $a1$ | $a2$ >>                                      -> <:expr< Ast.AsOr $meta_loc_expr _loc$ $meta_a a1$ $meta_a a2$ >>
    | <:assoc@_loc< $p$ when $e1$ -> $e2$ >>                            -> <:expr< Ast.AsArr $meta_loc_expr _loc$ $meta_p p$ $meta_e e1$ $meta_e e2$ >>
    | <:assoc@_loc< $anti:s$ >>                                         -> <:expr< $anti:s$ >> ]
  
    and binding = fun
    [ <:binding@_loc<>>                                                 -> <:expr< Ast.BiNil $meta_loc_expr _loc$ >>
    | <:binding@_loc< $bi1$ and $bi2$ >>                                -> <:expr< Ast.BiAnd $meta_loc_expr _loc$ $meta_bi bi1$ $meta_bi bi2$ >>
    | <:binding@_loc< $bi1$ ; $bi2$ >>                                  -> <:expr< Ast.BiSem $meta_loc_expr _loc$ $meta_bi bi1$ $meta_bi bi2$ >>
    | <:binding@_loc< $p$ = $e$ >>                                      -> <:expr< Ast.BiEq $meta_loc_expr _loc$ $meta_p p$ $meta_e e$ >>
    | <:binding@_loc< $anti:s$ >>                                       -> <:expr< $anti:s$ >> ]
  
    and module_binding = fun
    [ <:module_binding@_loc<>>                                          -> <:expr< Ast.MbNil $meta_loc_expr _loc$ >>
    | <:module_binding@_loc< $mb1$ and $mb2$ >>                         -> <:expr< Ast.MbAnd $meta_loc_expr _loc$ $meta_mb mb1$ $meta_mb mb2$ >>
    | <:module_binding@_loc< $s$ : $mt$ = $me$ >>                       -> <:expr< Ast.MbColEq $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt$ $meta_me me$ >>
    | <:module_binding@_loc< $s$ : $mt$ >>                              -> <:expr< Ast.MbCol $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:module_binding@_loc< $anti:s$ >>                                -> <:expr< $anti:s$ >> ]
  
    and ctyp = fun
    [ <:ctyp@_loc<>>                                                    -> <:expr< Ast.TyNil $meta_loc_expr _loc$ >>
    | <:ctyp@_loc< $id:i$ >>                                            -> <:expr< Ast.TyId $meta_loc_expr _loc$ $meta_i i$ >>
    | <:ctyp@_loc< $t1$ as $t2$ >>                                      -> <:expr< Ast.TyAli $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< _ >>                                                 -> <:expr< Ast.TyAny $meta_loc_expr _loc$ >>
    | <:ctyp@_loc< $t1$ $t2$ >>                                         -> <:expr< Ast.TyApp $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ -> $t2$ >>                                      -> <:expr< Ast.TyArr $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ | $t2$ >>                                       -> <:expr< Ast.TyOr $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ of $t2$ >>                                      -> <:expr< Ast.TyOf $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ and $t2$ >>                                     -> <:expr< Ast.TyAnd $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$; $t2$ >>                                        -> <:expr< Ast.TySem $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$, $t2$ >>                                        -> <:expr< Ast.TyCom $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ : $t2$ >>                                       -> <:expr< Ast.TyCol $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< mutable $t$ >>                                       -> <:expr< Ast.TyMut $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< # $i$ >>                                             -> <:expr< Ast.TyCls $meta_loc_expr _loc$ $meta_i i$ >>
    | <:ctyp@_loc< ~ $s$ : $t$ >>                                       -> <:expr< Ast.TyLab $meta_loc_expr _loc$ $meta_s _loc s$ $meta_t t$ >>
    | <:ctyp@_loc< $t1$ == $t2$ >>                                      -> <:expr< Ast.TyMan $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    (* | <:me-ta< <:ctyp< < $t$ > >> >> *)
    (* | <:me-ta< <:ctyp< < $t$ .. > >> >> *)
    | <:ctyp@_loc< < $t$ $opt:b$ > >>                                   -> <:expr< Ast.TyObj $meta_loc_expr _loc$ $meta_t t$ $meta_bool _loc b$ >>
    | <:ctyp@_loc< ? $s$ : $t$ >>                                       -> <:expr< Ast.TyOlb $meta_loc_expr _loc$ $meta_s _loc s$ $meta_t t$ >>
    | <:ctyp@_loc< ! $t1$ . $t2$ >>                                     -> <:expr< Ast.TyPol $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< '$s$ >>                                              -> <:expr< Ast.TyQuo $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< { $t$ } >>                                           -> <:expr< Ast.TyRec $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ $t$ ] >>                                           -> <:expr< Ast.TySum $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< ( $tup:t$ ) >>                                       -> <:expr< Ast.TyTup $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ = $t$ ] >>                                         -> <:expr< Ast.TyVrnEq $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ > $t$ ] >>                                         -> <:expr< Ast.TyVrnSup $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ < $t$ ] >>                                         -> <:expr< Ast.TyVrnInf $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ < $t1$ > $t2$ ] >>                                 -> <:expr< Ast.TyVrnInfSup $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< private $t$ >>                                       -> <:expr< Ast.TyPrv $meta_loc_expr _loc$ $meta_t t$ >>
    | <:ctyp@_loc< `$s$ >>                                              -> <:expr< Ast.TyVrn $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< $t1$ of & $t2$ >>                                    -> <:expr< Ast.TyOfAmp $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ & $t2$ >>                                       -> <:expr< Ast.TyAmp $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ * $t2$ >>                                       -> <:expr< Ast.TySta $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< +'$s$ >>                                             -> <:expr< Ast.TyQuP $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< -'$s$ >>                                             -> <:expr< Ast.TyQuM $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< $anti:s$ >>                                          -> <:expr< $anti:s$ >>
    | Ast.TyDcl _loc s tl t ttl ->
        <:expr< Ast.TyDcl $meta_loc_expr _loc$ $str:s$
                               $meta_list _loc ctyp tl$ $ctyp t$
                               $meta_list _loc (meta_tt _loc) ttl$ >> ]
  
  
    and sig_item = fun
    [ <:sig_item@_loc<>>                                                -> <:expr< Ast.SgNil $meta_loc_expr _loc$ >>
    | <:sig_item@_loc< $sg1$; $sg2$ >>                                  -> <:expr< Ast.SgSem $meta_loc_expr _loc$ $meta_sg sg1$ $meta_sg sg2$ >>
    | <:sig_item@_loc< exception $t$ >>                                 -> <:expr< Ast.SgExc $meta_loc_expr _loc$ $meta_t t$ >>
    (* | <:me--ta< <:sig_item< external $s$ : $t$ = $list:sl$ >> >> *)
    | <:sig_item@_loc< external $s1$ : $t$ = $s2$ >>                    -> <:expr< Ast.SgExt $meta_loc_expr _loc$ $meta_s _loc s1$ $meta_t t$ $meta_s _loc s2$ >>
    | <:sig_item@_loc< module $s$ : $mt$ >>                             -> <:expr< Ast.SgMod $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:sig_item@_loc< module type $s$ = $mt$ >>                        -> <:expr< Ast.SgMty $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:sig_item@_loc< open $i$ >>                                      -> <:expr< Ast.SgOpn $meta_loc_expr _loc$ $meta_i i$ >>
    | <:sig_item@_loc< type $t$ >>                                      -> <:expr< Ast.SgTyp $meta_loc_expr _loc$ $meta_t t$ >>
    | <:sig_item@_loc< value $s$ : $t$ >>                               -> <:expr< Ast.SgVal $meta_loc_expr _loc$ $meta_s _loc s$ $meta_t t$ >>
    | <:sig_item@_loc< include $mt$ >>                                  -> <:expr< Ast.SgInc $meta_loc_expr _loc$ $meta_mt mt$ >>
    | <:sig_item@_loc< class $ct$ >>                                    -> <:expr< Ast.SgCls $meta_loc_expr _loc$ $meta_ct ct$ >>
    | <:sig_item@_loc< class type $ct$ >>                               -> <:expr< Ast.SgClt $meta_loc_expr _loc$ $meta_ct ct$ >>
    | <:sig_item@_loc< module rec $mb$ >>                               -> <:expr< Ast.SgRecMod $meta_loc_expr _loc$ $meta_mb mb$ >>
    | <:sig_item@_loc< # $s$ $e$ >>                                     -> <:expr< Ast.SgDir $meta_loc_expr _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:sig_item@_loc< $anti:s$ >>                                      -> <:expr< $anti:s$ >> ]
  
    and str_item = fun
    [ <:str_item@_loc<>>                                                -> <:expr< Ast.StNil $meta_loc_expr _loc$ >>
    | <:str_item@_loc< $st1$; $st2$ >>                                  -> <:expr< Ast.StSem $meta_loc_expr _loc$ $meta_st st1$ $meta_st st2$ >>
    | <:str_item@_loc< exception $t$ >>                                 -> <:expr< Ast.StExc $meta_loc_expr _loc$ $meta_t t$ Ast.ONone >>
    | <:str_item@_loc< exception $t$ = $i$ >>                           -> <:expr< Ast.StExc $meta_loc_expr _loc$ $meta_t t$ (Ast.OSome $meta_i i$) >>
    | <:str_item@_loc< $exp:e$ >>                                       -> <:expr< Ast.StExp $meta_loc_expr _loc$ $meta_e e$ >>
    (* | <:me--ta< <:str_item< external $s$ : $t$ = $list:sl$ >> >> *)
    | <:str_item@_loc< external $s1$ : $t$ = $s2$ >>                    -> <:expr< Ast.StExt $meta_loc_expr _loc$ $meta_s _loc s1$ $meta_t t$ $meta_s _loc s2$ >>
    | <:str_item@_loc< module $s$ = $me$ >>                             -> <:expr< Ast.StMod $meta_loc_expr _loc$ $meta_s _loc s$ $meta_me me$ >>
    | <:str_item@_loc< module type $s$ = $mt$ >>                        -> <:expr< Ast.StMty $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:str_item@_loc< open $i$ >>                                      -> <:expr< Ast.StOpn $meta_loc_expr _loc$ $meta_i i$ >>
    | <:str_item@_loc< type $t$ >>                                      -> <:expr< Ast.StTyp $meta_loc_expr _loc$ $meta_t t$ >>
    | <:str_item@_loc< value $opt:r$ $bi$ >>                            -> <:expr< Ast.StVal $meta_loc_expr _loc$ $meta_bool _loc r$ $meta_bi bi$ >>
    | <:str_item@_loc< include $me$ >>                                  -> <:expr< Ast.StInc $meta_loc_expr _loc$ $meta_me me$ >>
    | <:str_item@_loc< class $ce$ >>                                    -> <:expr< Ast.StCls $meta_loc_expr _loc$ $meta_ce ce$ >>
    | <:str_item@_loc< class type $ct$ >>                               -> <:expr< Ast.StClt $meta_loc_expr _loc$ $meta_ct ct$ >>
    | <:str_item@_loc< module rec $mb$ >>                               -> <:expr< Ast.StRecMod $meta_loc_expr _loc$ $meta_mb mb$ >>
    | <:str_item@_loc< # $s$ $e$ >>                                     -> <:expr< Ast.StDir $meta_loc_expr _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:str_item@_loc< $anti:s$ >>                                      -> <:expr< $anti:s$ >>
    | _ -> assert False
    ]
  
  
    and module_type = fun
    [ <:module_type@_loc< $id:i$ >>                                     -> <:expr< Ast.MtId $meta_loc_expr _loc$ $meta_i i$ >>
    | <:module_type@_loc< functor ( $s$ : $mt1$ ) -> $mt2$ >>           -> <:expr< Ast.MtFun $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt1$ $meta_mt mt2$ >>
    | <:module_type@_loc< '$s$ >>                                       -> <:expr< Ast.MtQuo $meta_loc_expr _loc$ $meta_s _loc s$ >>
    | <:module_type@_loc< $anti:s$ >>                                   -> <:expr< $anti:s$ >>
    | <:module_type@_loc< sig $sg$ end >>                               -> <:expr< Ast.MtSig $meta_loc_expr _loc$ $meta_sg sg$ >>
    | <:module_type@_loc< $mt$ with $wc$ >>                             -> <:expr< Ast.MtWit $meta_loc_expr _loc$ $meta_mt mt$ $meta_wc wc$ >> ]
  
    and module_expr = fun
    [ <:module_expr@_loc< $id:i$ >>                                     -> <:expr< Ast.MeId $meta_loc_expr _loc$ $meta_i i$ >>
    | <:module_expr@_loc< $me1$ $me2$ >>                                -> <:expr< Ast.MeApp $meta_loc_expr _loc$ $meta_me me1$ $meta_me me2$ >>
    | <:module_expr@_loc< functor ( $s$ : $mt$ ) -> $me$ >>             -> <:expr< Ast.MeFun $meta_loc_expr _loc$ $meta_s _loc s$ $meta_mt mt$ $meta_me me$ >>
    | <:module_expr@_loc< $anti:s$ >>                                   -> <:expr< $anti:s$ >>
    | <:module_expr@_loc< struct $st$ end >>                            -> <:expr< Ast.MeStr $meta_loc_expr _loc$ $meta_st st$ >>
    | <:module_expr@_loc< ( $me$ : $mt$ ) >>                            -> <:expr< Ast.MeTyc $meta_loc_expr _loc$ $meta_me me$ $meta_mt mt$ >> ]
  
    and class_expr = fun
    [ <:class_expr@_loc<>>                                              -> <:expr< Ast.CeNil $meta_loc_expr _loc$ >>
    | <:class_expr@_loc< $ce$ $e$ >>                                    -> <:expr< Ast.CeApp $meta_loc_expr _loc$ $meta_ce ce$ $meta_e e$ >>
    | <:class_expr@_loc< $opt:v$ $id:i$ >>                              -> <:expr< Ast.CeCon $meta_loc_expr _loc$ $meta_bool _loc v$ $meta_i i$ (Ast.TyNil $meta_loc_expr _loc$) >>
    | <:class_expr@_loc< $opt:v$ $id:i$ [ $t$ ] >>                      -> <:expr< Ast.CeCon $meta_loc_expr _loc$ $meta_bool _loc v$ $meta_i i$ $meta_t t$ >>
    | <:class_expr@_loc< fun $p$ -> $ce$ >>                             -> <:expr< Ast.CeFun $meta_loc_expr _loc$ $meta_p p$ $meta_ce ce$ >>
    | <:class_expr@_loc< let $opt:r$ $bi$ in $ce$ >>                    -> <:expr< Ast.CeLet $meta_loc_expr _loc$ $meta_bool _loc r$ $meta_bi bi$ $meta_ce ce$ >>
    | <:class_expr@_loc< object ($p$) $cst$ end >>                      -> <:expr< Ast.CeStr $meta_loc_expr _loc$ $meta_p p$ $meta_cst cst$ >>
    | <:class_expr@_loc< $anti:s$ >>                                    -> <:expr< $anti:s$ >>
    | <:class_expr@_loc< $ce1$ and $ce2$ >>                             -> <:expr< Ast.CeAnd $meta_loc_expr _loc$ $meta_ce ce1$ $meta_ce ce2$ >>
    | <:class_expr@_loc< ( $ce$ : $ct$ ) >>                             -> <:expr< Ast.CeTyc $meta_loc_expr _loc$ $meta_ce ce$ $meta_ct ct$ >>
    | <:class_expr@_loc< $ce1$ = $ce2$ >>                               -> <:expr< Ast.CeEq $meta_loc_expr _loc$ $meta_ce ce1$ $meta_ce ce2$ >> ]
  
  
    and class_type = fun
    [ <:class_type@_loc<>>                                              -> <:expr< Ast.CtNil $meta_loc_expr _loc$ >>
    | <:class_type@_loc< $opt:v$ $id:i$ >>                              -> <:expr< Ast.CtCon $meta_loc_expr _loc$ $meta_bool _loc v$ $meta_i i$ (Ast.TyNil $meta_loc_expr _loc$) >>
    | <:class_type@_loc< $opt:v$ $id:i$ [ $t$ ] >>                      -> <:expr< Ast.CtCon $meta_loc_expr _loc$ $meta_bool _loc v$ $meta_i i$ $meta_t t$ >>
    | <:class_type@_loc< [ $t$ ] -> $ct$ >>                             -> <:expr< Ast.CtFun $meta_loc_expr _loc$ $meta_t t$ $meta_ct ct$ >>
    | <:class_type@_loc< $anti:s$ >>                                    -> <:expr< $anti:s$ >>
    | <:class_type@_loc< object ($t$) $csg$ end >>                      -> <:expr< Ast.CtSig $meta_loc_expr _loc$ $meta_t t$ $meta_csg csg$ >>
    | <:class_type@_loc< $ct1$ and $ct2$ >>                             -> <:expr< Ast.CtAnd $meta_loc_expr _loc$ $meta_ct ct1$ $meta_ct ct2$ >>
    | <:class_type@_loc< $ct1$ : $ct2$ >>                               -> <:expr< Ast.CtCol $meta_loc_expr _loc$ $meta_ct ct1$ $meta_ct ct2$ >>
    | <:class_type@_loc< $ct1$ = $ct2$ >>                               -> <:expr< Ast.CtEq $meta_loc_expr _loc$ $meta_ct ct1$ $meta_ct ct2$ >> ]
  
    and class_sig_item = fun
    [ <:class_sig_item@_loc<>>                                          -> <:expr< Ast.CgNil $meta_loc_expr _loc$ >>
    | <:class_sig_item@_loc< type $t1$ = $t2$ >>                        -> <:expr< Ast.CgCtr $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:class_sig_item@_loc< $csg1$; $csg2$ >>                          -> <:expr< Ast.CgSem $meta_loc_expr _loc$ $meta_csg csg1$ $meta_csg csg2$ >>
    | <:class_sig_item@_loc< $anti:s$ >>                                -> <:expr< $anti:s$ >>
    | <:class_sig_item@_loc< inherit $ct$ >>                            -> <:expr< Ast.CgInh $meta_loc_expr _loc$ $meta_ct ct$ >>
    | <:class_sig_item@_loc< method $opt:pr$ $s$ : $t$ >>               -> <:expr< Ast.CgMth $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_t t$ >>
    | <:class_sig_item@_loc< method virtual $opt:pr$ $s$ : $t$ >>       -> <:expr< Ast.CgVir $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_t t$ >>
    | <:class_sig_item@_loc< value $opt:m$ $opt:v$ $s$ : $t$ >>         -> <:expr< Ast.CgVal $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc m$ $meta_bool _loc v$ $meta_t t$ >> ]
  
    and class_str_item = fun
    [ <:class_str_item@_loc<>>                                          -> <:expr< Ast.CrNil $meta_loc_expr _loc$ >>
    | <:class_str_item@_loc< $cst1$; $cst2$ >>                          -> <:expr< Ast.CrSem $meta_loc_expr _loc$ $meta_cst cst1$ $meta_cst cst2$ >>
    | <:class_str_item@_loc< $anti:s$ >>                                -> <:expr< $anti:s$ >>
    | <:class_str_item@_loc< type $t1$ = $t2$ >>                        -> <:expr< Ast.CrCtr $meta_loc_expr _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:class_str_item@_loc< inherit $ce$ as $s$ >>                     -> <:expr< Ast.CrInh $meta_loc_expr _loc$ $meta_ce ce$ $meta_s _loc s$ >>
    | <:class_str_item@_loc< initializer $e$ >>                         -> <:expr< Ast.CrIni $meta_loc_expr _loc$ $meta_e e$ >>
    | <:class_str_item@_loc< method $opt:pr$ $s$ : $t$ = $e$ >>         -> <:expr< Ast.CrMth $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_e e$ $meta_t t$ >>
    | <:class_str_item@_loc< method virtual $opt:pr$ $s$ : $t$ >>       -> <:expr< Ast.CrVir $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_t t$ >>
    | <:class_str_item@_loc< value $opt:m$ $s$ = $e$ >>                 -> <:expr< Ast.CrVal $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc m$ $meta_e e$ >>
    | <:class_str_item@_loc< value virtual $opt:m$ $s$ : $t$ >>         -> <:expr< Ast.CrVvr $meta_loc_expr _loc$ $meta_s _loc s$ $meta_bool _loc m$ $meta_t t$ >> ];
  
  end;

  module Patt = struct

    value rec meta_list _loc mf =
      fun
      [ [] -> <:patt< [] >>
      | [x :: xs] -> <:patt< [ $mf x$ :: $meta_list _loc mf xs$ ] >> ];
  
    value meta_bool _loc =
      fun   
      [ Ast.BFalse -> <:patt< Ast.BFalse >>
      | Ast.BTrue -> <:patt< Ast.BTrue >>
      | Ast.BAnt s -> <:patt< $anti:s$ >> ]; 
  
    value meta_chr _ x = x;
    value meta_flo _ x = x;
    value meta_int _ x = x;
    value meta_lid _ x = x;
    value meta_uid _ x = x;
    value meta_str _loc s = <:patt< $`str:s$ >>;
    value meta_s _loc s = <:patt< $str:s$ >>;
    value meta_c x = x;
    value meta_b = meta_bool;
    value meta_to = meta_bool;
  
    value rec meta_e x = expr x
    and meta_bi x = binding x
    and meta_a x = assoc x
    and meta_mb x = module_binding x
    and meta_p x = patt x
    and meta_t x = ctyp x
    and meta_mt x = module_type x
    (* and meta_sl _loc x = meta_list _loc meta_s x *)
    and meta_tt _loc (t1, t2) = <:patt< ($ctyp t1$, $ctyp t2$) >>
    and meta_st x = str_item x
    and meta_sg x = sig_item x
    and meta_me x = module_expr x
    and meta_ce x = class_expr x
    and meta_ct x = class_type x
    and meta_csg x = class_sig_item x
    and meta_cst x = class_str_item x
    and meta_wc x = with_constr x
  
    and with_constr =
    fun
    [ <:with_constr@_loc<>>                                             -> <:patt< Ast.WcNil $meta_loc_patt _loc$ >>
    | <:with_constr@_loc< type $t1$ = $t2$ >>                           -> <:patt< Ast.WcTyp $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:with_constr@_loc< $wc1$ and $wc2$ >>                            -> <:patt< Ast.WcAnd $meta_loc_patt _loc$ $meta_wc wc1$ $meta_wc wc2$ >>
    | <:with_constr@_loc< module $i1$ = $i2$ >>                         -> <:patt< Ast.WcMod $meta_loc_patt _loc$ $meta_i i1$ $meta_i i2$ >>
    | <:with_constr@_loc< $anti:s$ >>                                   -> <:patt< $anti:s$ >> ]
  
    and ident = fun
    [ <:ident@_loc< $i1$.$i2$ >>                                        -> <:patt< Ast.IdAcc $meta_loc_patt _loc$ $meta_i i1$ $meta_i i2$ >>
    | <:ident@_loc< $i1$ $i2$ >>                                        -> <:patt< Ast.IdApp $meta_loc_patt _loc$ $meta_i i1$ $meta_i i2$ >>
    | <:ident@_loc< $anti:s$ >>                                         -> <:patt< $anti:s$ >>
    | <:ident@_loc< $lid:s$ >>                                          -> <:patt< Ast.IdLid $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:ident@_loc< $uid:s$ >>                                          -> <:patt< Ast.IdUid $meta_loc_patt _loc$ $meta_s _loc s$ >> ]
    and meta_i i = ident i
  
    and expr = fun
    [ <:expr@_loc<>>                                                    -> <:patt< Ast.ExNil $meta_loc_patt _loc$ >>
    | <:expr@_loc< $id:i$ >>                                            -> <:patt< Ast.ExId $meta_loc_patt _loc$ $meta_i i$ >>
    | <:expr@_loc< $anti:s$ >>                                          -> <:patt< $anti:s$ >>
    | <:expr@_loc< $e1$ $e2$ >>                                         -> <:patt< Ast.ExApp $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $e1$ .( $e2$ ) >>                                    -> <:patt< Ast.ExAre $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< [| $e$ |] >>                                         -> <:patt< Ast.ExArr $meta_loc_patt _loc$ $meta_e e$ >>
    | <:expr@_loc< $e1$ := $e2$ >>                                      -> <:patt< Ast.ExAss $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $chr:c$ >>                                           -> <:patt< Ast.ExChr $meta_loc_patt _loc$ $meta_s _loc c$ >>
    | <:expr@_loc< ($e$ :> $t$) >>                                      -> <:patt< Ast.ExCoe $meta_loc_patt _loc$ $meta_e e$ (Ast.TyNil $meta_loc_patt _loc$) $meta_t t$ >>
    | <:expr@_loc< ($e$ : $t1$ :> $t2$) >>                              -> <:patt< Ast.ExCoe $meta_loc_patt _loc$ $meta_e e$ $meta_t t1$ $meta_t t2$ >>
    | <:expr@_loc< $flo:s$ >>                                           -> <:patt< Ast.ExFlo $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< for $s$ = $e1$ $to:b$ $e2$ do { $e3$ } >>            -> <:patt< Ast.ExFor $meta_loc_patt _loc$ $meta_s _loc s$ $meta_e e1$ $meta_e e2$ $meta_to _loc b$ $meta_e e3$ >>
    | <:expr@_loc< fun [ $a$ ] >>                                       -> <:patt< Ast.ExFun $meta_loc_patt _loc$ $meta_a a$ >>
    | <:expr@_loc< if $e1$ then $e2$ else $e3$ >>                       -> <:patt< Ast.ExIfe $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ $meta_e e3$ >>
    | <:expr@_loc< $int:s$ >>                                           -> <:patt< Ast.ExInt $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< lazy $e$ >>                                          -> <:patt< Ast.ExLaz $meta_loc_patt _loc$ $meta_e e$ >>
    | <:expr@_loc< let $opt:r$ $bi$ in $e$ >>                           -> <:patt< Ast.ExLet $meta_loc_patt _loc$ $meta_bool _loc r$ $meta_bi bi$ $meta_e e$ >>
    | <:expr@_loc< match $e$ with [ $a$ ] >>                            -> <:patt< Ast.ExMat $meta_loc_patt _loc$ $meta_e e$ $meta_a a$ >>
    | <:expr@_loc< { $bi$ } >>                                          -> <:patt< Ast.ExRec $meta_loc_patt _loc$ $meta_bi bi$ (Ast.ExNil $meta_loc_patt _loc$) >>
    | <:expr@_loc< { ($e$) with $bi$ } >>                               -> <:patt< Ast.ExRec $meta_loc_patt _loc$ $meta_bi bi$ $meta_e e$ >>
    | <:expr@_loc< do { $e$ } >>                                        -> <:patt< Ast.ExSeq $meta_loc_patt _loc$ $meta_e e$ >>
    | <:expr@_loc< $e1$ .[ $e2$ ] >>                                    -> <:patt< Ast.ExSte $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $str:s$ >>                                           -> <:patt< Ast.ExStr $meta_loc_patt _loc$ $meta_str _loc s$ >>
    | <:expr@_loc< try $e$ with [ $a$ ] >>                              -> <:patt< Ast.ExTry $meta_loc_patt _loc$ $meta_e e$ $meta_a a$ >>
    | <:expr@_loc< ( $tup:e$ ) >>                                       -> <:patt< Ast.ExTup $meta_loc_patt _loc$ $meta_e e$ >>
    | <:expr@_loc< ( $e$ : $t$ ) >>                                     -> <:patt< Ast.ExTyc $meta_loc_patt _loc$ $meta_e e$ $meta_t t$ >>
    | <:expr@_loc< while $e1$ do { $e2$ } >>                            -> <:patt< Ast.ExWhi $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< `$s$ >>                                              -> <:patt< Ast.ExVrn $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< $e$ # $s$ >>                                         -> <:patt< Ast.ExSnd $meta_loc_patt _loc$ $meta_e e$ $meta_s _loc s$ >>
    | <:expr@_loc< ~ $s$ >>                                             -> <:patt< Ast.ExLab $meta_loc_patt _loc$ $meta_s _loc s$ (Ast.ExNil $meta_loc_patt _loc$) >>
    | <:expr@_loc< ~ $s$ : $e$ >>                                       -> <:patt< Ast.ExLab $meta_loc_patt _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:expr@_loc< ? $s$ >>                                             -> <:patt< Ast.ExOlb $meta_loc_patt _loc$ $meta_s _loc s$ (Ast.ExNil $meta_loc_patt _loc$) >>
    | <:expr@_loc< ? $s$ : $e$ >>                                       -> <:patt< Ast.ExOlb $meta_loc_patt _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:expr@_loc< assert $e$ >>                                        -> <:patt< Ast.ExAsr $meta_loc_patt _loc$ $meta_e e$ >>
    | <:expr@_loc< new $i$ >>                                           -> <:patt< Ast.ExNew $meta_loc_patt _loc$ $meta_i i$ >>
    | <:expr@_loc< {< $bi$ >} >>                                        -> <:patt< Ast.ExOvr $meta_loc_patt _loc$ $meta_bi bi$ >>
    | <:expr@_loc< let module $s$ = $me$ in $e$ >>                      -> <:patt< Ast.ExLmd $meta_loc_patt _loc$ $meta_s _loc s$ $meta_me me$ $meta_e e$ >>
    | <:expr@_loc< assert False >>                                      -> <:patt< Ast.ExAsf $meta_loc_patt _loc$ >>
    | <:expr@_loc< $int32:s$ >>                                         -> <:patt< Ast.ExInt32 $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< $int64:s$ >>                                         -> <:patt< Ast.ExInt64 $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< $nativeint:s$ >>                                     -> <:patt< Ast.ExNativeInt $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:expr@_loc< object ($p$) $cst$ end >>                            -> <:patt< Ast.ExObj $meta_loc_patt _loc$ $meta_p p$ $meta_cst cst$ >>
    | <:expr@_loc< $e1$, $e2$ >>                                        -> <:patt< Ast.ExCom $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $e1$ . $e2$ >>                                       -> <:patt< Ast.ExAcc $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >>
    | <:expr@_loc< $e1$; $e2$ >>                                        -> <:patt< Ast.ExSem $meta_loc_patt _loc$ $meta_e e1$ $meta_e e2$ >> ]
  
    and patt = fun
    [ <:patt@_loc<>>                                                    -> <:patt< Ast.PaNil $meta_loc_patt _loc$ >>
    | <:patt@_loc< $id:i$ >>                                            -> <:patt< Ast.PaId $meta_loc_patt _loc$ $meta_i i$ >>
    | <:patt@_loc< $anti:s$ >>                                          -> <:patt< $anti:s$ >>
    | <:patt@_loc< ( $p1$ as $p2$ ) >>                                  -> <:patt< Ast.PaAli $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< _ >>                                                 -> <:patt< Ast.PaAny $meta_loc_patt _loc$ >>
    | <:patt@_loc< $p1$ $p2$ >>                                         -> <:patt< Ast.PaApp $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $chr:c$ >>                                           -> <:patt< Ast.PaChr $meta_loc_patt _loc$ $meta_s _loc c$ >>
    | <:patt@_loc< $int:s$ >>                                           -> <:patt< Ast.PaInt $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< $p1$ | $p2$ >>                                       -> <:patt< Ast.PaOrp $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $p1$ .. $p2$ >>                                      -> <:patt< Ast.PaRng $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< { $p$ } >>                                           -> <:patt< Ast.PaRec $meta_loc_patt _loc$ $meta_p p$ >>
    | <:patt@_loc< $str:s$ >>                                           -> <:patt< Ast.PaStr $meta_loc_patt _loc$ $meta_str _loc s$ >>
    | <:patt@_loc< ( $tup:p$ ) >>                                       -> <:patt< Ast.PaTup $meta_loc_patt _loc$ $meta_p p$ >>
    | <:patt@_loc< ( $p$ : $t$ ) >>                                     -> <:patt< Ast.PaTyc $meta_loc_patt _loc$ $meta_p p$ $meta_t t$ >>
    | <:patt@_loc< $int32:s$ >>                                         -> <:patt< Ast.PaInt32 $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< $int64:s$ >>                                         -> <:patt< Ast.PaInt64 $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< $nativeint:s$ >>                                     -> <:patt< Ast.PaNativeInt $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< `$s$ >>                                              -> <:patt< Ast.PaVrn $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:patt@_loc< [| $p$ |] >>                                         -> <:patt< Ast.PaArr $meta_loc_patt _loc$ $meta_p p$ >>
    | <:patt@_loc< $flo:f$ >>                                           -> <:patt< Ast.PaFlo $meta_loc_patt _loc$ $meta_s _loc f$ >>
    | <:patt@_loc< ~ $s$ >>                                             -> <:patt< Ast.PaLab $meta_loc_patt _loc$ $meta_s _loc s$ (Ast.PaNil $meta_loc_patt _loc$) >>
    | <:patt@_loc< ? $s$ >>                                             -> <:patt< Ast.PaOlb $meta_loc_patt _loc$ $meta_s _loc s$ (Ast.PaNil $meta_loc_patt _loc$) >>
    | <:patt@_loc< ~ $s$ : ($p$) >>                                     -> <:patt< Ast.PaLab $meta_loc_patt _loc$ $meta_s _loc s$ $meta_p p$ >>
    | <:patt@_loc< ? $s$ : ($p$) >>                                     -> <:patt< Ast.PaOlb $meta_loc_patt _loc$ $meta_s _loc s$ $meta_p p$ >>
    | <:patt@_loc< ? $s$ : ($p$ = $e$) >>                               -> <:patt< Ast.PaOlbi $meta_loc_patt _loc$ $meta_s _loc s$ $meta_p p$ $meta_e e$ >>
    | <:patt@_loc< # $i$ >>                                             -> <:patt< Ast.PaTyp $meta_loc_patt _loc$ $meta_i i$ >>
    | <:patt@_loc< $p1$, $p2$ >>                                        -> <:patt< Ast.PaCom $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $p1$ = $p2$ >>                                       -> <:patt< Ast.PaEq $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >>
    | <:patt@_loc< $p1$; $p2$ >>                                        -> <:patt< Ast.PaSem $meta_loc_patt _loc$ $meta_p p1$ $meta_p p2$ >> ]
  
    and assoc = fun
    [ <:assoc@_loc<>>                                                   -> <:patt< Ast.AsNil $meta_loc_patt _loc$ >>
    | <:assoc@_loc< $a1$ | $a2$ >>                                      -> <:patt< Ast.AsOr $meta_loc_patt _loc$ $meta_a a1$ $meta_a a2$ >>
    | <:assoc@_loc< $p$ when $e1$ -> $e2$ >>                            -> <:patt< Ast.AsArr $meta_loc_patt _loc$ $meta_p p$ $meta_e e1$ $meta_e e2$ >>
    | <:assoc@_loc< $anti:s$ >>                                         -> <:patt< $anti:s$ >> ]
  
    and binding = fun
    [ <:binding@_loc<>>                                                 -> <:patt< Ast.BiNil $meta_loc_patt _loc$ >>
    | <:binding@_loc< $bi1$ and $bi2$ >>                                -> <:patt< Ast.BiAnd $meta_loc_patt _loc$ $meta_bi bi1$ $meta_bi bi2$ >>
    | <:binding@_loc< $bi1$ ; $bi2$ >>                                  -> <:patt< Ast.BiSem $meta_loc_patt _loc$ $meta_bi bi1$ $meta_bi bi2$ >>
    | <:binding@_loc< $p$ = $e$ >>                                      -> <:patt< Ast.BiEq $meta_loc_patt _loc$ $meta_p p$ $meta_e e$ >>
    | <:binding@_loc< $anti:s$ >>                                       -> <:patt< $anti:s$ >> ]
  
    and module_binding = fun
    [ <:module_binding@_loc<>>                                          -> <:patt< Ast.MbNil $meta_loc_patt _loc$ >>
    | <:module_binding@_loc< $mb1$ and $mb2$ >>                         -> <:patt< Ast.MbAnd $meta_loc_patt _loc$ $meta_mb mb1$ $meta_mb mb2$ >>
    | <:module_binding@_loc< $s$ : $mt$ = $me$ >>                       -> <:patt< Ast.MbColEq $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt$ $meta_me me$ >>
    | <:module_binding@_loc< $s$ : $mt$ >>                              -> <:patt< Ast.MbCol $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:module_binding@_loc< $anti:s$ >>                                -> <:patt< $anti:s$ >> ]
  
    and ctyp = fun
    [ <:ctyp@_loc<>>                                                    -> <:patt< Ast.TyNil $meta_loc_patt _loc$ >>
    | <:ctyp@_loc< $id:i$ >>                                            -> <:patt< Ast.TyId $meta_loc_patt _loc$ $meta_i i$ >>
    | <:ctyp@_loc< $t1$ as $t2$ >>                                      -> <:patt< Ast.TyAli $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< _ >>                                                 -> <:patt< Ast.TyAny $meta_loc_patt _loc$ >>
    | <:ctyp@_loc< $t1$ $t2$ >>                                         -> <:patt< Ast.TyApp $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ -> $t2$ >>                                      -> <:patt< Ast.TyArr $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ | $t2$ >>                                       -> <:patt< Ast.TyOr $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ of $t2$ >>                                      -> <:patt< Ast.TyOf $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ and $t2$ >>                                     -> <:patt< Ast.TyAnd $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$; $t2$ >>                                        -> <:patt< Ast.TySem $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$, $t2$ >>                                        -> <:patt< Ast.TyCom $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ : $t2$ >>                                       -> <:patt< Ast.TyCol $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< mutable $t$ >>                                       -> <:patt< Ast.TyMut $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< # $i$ >>                                             -> <:patt< Ast.TyCls $meta_loc_patt _loc$ $meta_i i$ >>
    | <:ctyp@_loc< ~ $s$ : $t$ >>                                       -> <:patt< Ast.TyLab $meta_loc_patt _loc$ $meta_s _loc s$ $meta_t t$ >>
    | <:ctyp@_loc< $t1$ == $t2$ >>                                      -> <:patt< Ast.TyMan $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    (* | <:me-ta< <:ctyp< < $t$ > >> >> *)
    (* | <:me-ta< <:ctyp< < $t$ .. > >> >> *)
    | <:ctyp@_loc< < $t$ $opt:b$ > >>                                   -> <:patt< Ast.TyObj $meta_loc_patt _loc$ $meta_t t$ $meta_bool _loc b$ >>
    | <:ctyp@_loc< ? $s$ : $t$ >>                                       -> <:patt< Ast.TyOlb $meta_loc_patt _loc$ $meta_s _loc s$ $meta_t t$ >>
    | <:ctyp@_loc< ! $t1$ . $t2$ >>                                     -> <:patt< Ast.TyPol $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< '$s$ >>                                              -> <:patt< Ast.TyQuo $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< { $t$ } >>                                           -> <:patt< Ast.TyRec $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ $t$ ] >>                                           -> <:patt< Ast.TySum $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< ( $tup:t$ ) >>                                       -> <:patt< Ast.TyTup $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ = $t$ ] >>                                         -> <:patt< Ast.TyVrnEq $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ > $t$ ] >>                                         -> <:patt< Ast.TyVrnSup $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ < $t$ ] >>                                         -> <:patt< Ast.TyVrnInf $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< [ < $t1$ > $t2$ ] >>                                 -> <:patt< Ast.TyVrnInfSup $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< private $t$ >>                                       -> <:patt< Ast.TyPrv $meta_loc_patt _loc$ $meta_t t$ >>
    | <:ctyp@_loc< `$s$ >>                                              -> <:patt< Ast.TyVrn $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< $t1$ of & $t2$ >>                                    -> <:patt< Ast.TyOfAmp $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ & $t2$ >>                                       -> <:patt< Ast.TyAmp $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< $t1$ * $t2$ >>                                       -> <:patt< Ast.TySta $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:ctyp@_loc< +'$s$ >>                                             -> <:patt< Ast.TyQuP $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< -'$s$ >>                                             -> <:patt< Ast.TyQuM $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:ctyp@_loc< $anti:s$ >>                                          -> <:patt< $anti:s$ >>
    | Ast.TyDcl _loc s tl t ttl ->
        <:patt< Ast.TyDcl $meta_loc_patt _loc$ $str:s$
                               $meta_list _loc ctyp tl$ $ctyp t$
                               $meta_list _loc (meta_tt _loc) ttl$ >> ]
  
  
    and sig_item = fun
    [ <:sig_item@_loc<>>                                                -> <:patt< Ast.SgNil $meta_loc_patt _loc$ >>
    | <:sig_item@_loc< $sg1$; $sg2$ >>                                  -> <:patt< Ast.SgSem $meta_loc_patt _loc$ $meta_sg sg1$ $meta_sg sg2$ >>
    | <:sig_item@_loc< exception $t$ >>                                 -> <:patt< Ast.SgExc $meta_loc_patt _loc$ $meta_t t$ >>
    (* | <:me--ta< <:sig_item< external $s$ : $t$ = $list:sl$ >> >> *)
    | <:sig_item@_loc< external $s1$ : $t$ = $s2$ >>                    -> <:patt< Ast.SgExt $meta_loc_patt _loc$ $meta_s _loc s1$ $meta_t t$ $meta_s _loc s2$ >>
    | <:sig_item@_loc< module $s$ : $mt$ >>                             -> <:patt< Ast.SgMod $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:sig_item@_loc< module type $s$ = $mt$ >>                        -> <:patt< Ast.SgMty $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:sig_item@_loc< open $i$ >>                                      -> <:patt< Ast.SgOpn $meta_loc_patt _loc$ $meta_i i$ >>
    | <:sig_item@_loc< type $t$ >>                                      -> <:patt< Ast.SgTyp $meta_loc_patt _loc$ $meta_t t$ >>
    | <:sig_item@_loc< value $s$ : $t$ >>                               -> <:patt< Ast.SgVal $meta_loc_patt _loc$ $meta_s _loc s$ $meta_t t$ >>
    | <:sig_item@_loc< include $mt$ >>                                  -> <:patt< Ast.SgInc $meta_loc_patt _loc$ $meta_mt mt$ >>
    | <:sig_item@_loc< class $ct$ >>                                    -> <:patt< Ast.SgCls $meta_loc_patt _loc$ $meta_ct ct$ >>
    | <:sig_item@_loc< class type $ct$ >>                               -> <:patt< Ast.SgClt $meta_loc_patt _loc$ $meta_ct ct$ >>
    | <:sig_item@_loc< module rec $mb$ >>                               -> <:patt< Ast.SgRecMod $meta_loc_patt _loc$ $meta_mb mb$ >>
    | <:sig_item@_loc< # $s$ $e$ >>                                     -> <:patt< Ast.SgDir $meta_loc_patt _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:sig_item@_loc< $anti:s$ >>                                      -> <:patt< $anti:s$ >> ]
  
    and str_item = fun
    [ <:str_item@_loc<>>                                                -> <:patt< Ast.StNil $meta_loc_patt _loc$ >>
    | <:str_item@_loc< $st1$; $st2$ >>                                  -> <:patt< Ast.StSem $meta_loc_patt _loc$ $meta_st st1$ $meta_st st2$ >>
    | <:str_item@_loc< exception $t$ >>                                 -> <:patt< Ast.StExc $meta_loc_patt _loc$ $meta_t t$ Ast.ONone >>
    | <:str_item@_loc< exception $t$ = $i$ >>                           -> <:patt< Ast.StExc $meta_loc_patt _loc$ $meta_t t$ (Ast.OSome $meta_i i$) >>
    | <:str_item@_loc< $exp:e$ >>                                       -> <:patt< Ast.StExp $meta_loc_patt _loc$ $meta_e e$ >>
    (* | <:me--ta< <:str_item< external $s$ : $t$ = $list:sl$ >> >> *)
    | <:str_item@_loc< external $s1$ : $t$ = $s2$ >>                    -> <:patt< Ast.StExt $meta_loc_patt _loc$ $meta_s _loc s1$ $meta_t t$ $meta_s _loc s2$ >>
    | <:str_item@_loc< module $s$ = $me$ >>                             -> <:patt< Ast.StMod $meta_loc_patt _loc$ $meta_s _loc s$ $meta_me me$ >>
    | <:str_item@_loc< module type $s$ = $mt$ >>                        -> <:patt< Ast.StMty $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt$ >>
    | <:str_item@_loc< open $i$ >>                                      -> <:patt< Ast.StOpn $meta_loc_patt _loc$ $meta_i i$ >>
    | <:str_item@_loc< type $t$ >>                                      -> <:patt< Ast.StTyp $meta_loc_patt _loc$ $meta_t t$ >>
    | <:str_item@_loc< value $opt:r$ $bi$ >>                            -> <:patt< Ast.StVal $meta_loc_patt _loc$ $meta_bool _loc r$ $meta_bi bi$ >>
    | <:str_item@_loc< include $me$ >>                                  -> <:patt< Ast.StInc $meta_loc_patt _loc$ $meta_me me$ >>
    | <:str_item@_loc< class $ce$ >>                                    -> <:patt< Ast.StCls $meta_loc_patt _loc$ $meta_ce ce$ >>
    | <:str_item@_loc< class type $ct$ >>                               -> <:patt< Ast.StClt $meta_loc_patt _loc$ $meta_ct ct$ >>
    | <:str_item@_loc< module rec $mb$ >>                               -> <:patt< Ast.StRecMod $meta_loc_patt _loc$ $meta_mb mb$ >>
    | <:str_item@_loc< # $s$ $e$ >>                                     -> <:patt< Ast.StDir $meta_loc_patt _loc$ $meta_s _loc s$ $meta_e e$ >>
    | <:str_item@_loc< $anti:s$ >>                                      -> <:patt< $anti:s$ >>
    | _ -> assert False
    ]
  
  
    and module_type = fun
    [ <:module_type@_loc< $id:i$ >>                                     -> <:patt< Ast.MtId $meta_loc_patt _loc$ $meta_i i$ >>
    | <:module_type@_loc< functor ( $s$ : $mt1$ ) -> $mt2$ >>           -> <:patt< Ast.MtFun $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt1$ $meta_mt mt2$ >>
    | <:module_type@_loc< '$s$ >>                                       -> <:patt< Ast.MtQuo $meta_loc_patt _loc$ $meta_s _loc s$ >>
    | <:module_type@_loc< $anti:s$ >>                                   -> <:patt< $anti:s$ >>
    | <:module_type@_loc< sig $sg$ end >>                               -> <:patt< Ast.MtSig $meta_loc_patt _loc$ $meta_sg sg$ >>
    | <:module_type@_loc< $mt$ with $wc$ >>                             -> <:patt< Ast.MtWit $meta_loc_patt _loc$ $meta_mt mt$ $meta_wc wc$ >> ]
  
    and module_expr = fun
    [ <:module_expr@_loc< $id:i$ >>                                     -> <:patt< Ast.MeId $meta_loc_patt _loc$ $meta_i i$ >>
    | <:module_expr@_loc< $me1$ $me2$ >>                                -> <:patt< Ast.MeApp $meta_loc_patt _loc$ $meta_me me1$ $meta_me me2$ >>
    | <:module_expr@_loc< functor ( $s$ : $mt$ ) -> $me$ >>             -> <:patt< Ast.MeFun $meta_loc_patt _loc$ $meta_s _loc s$ $meta_mt mt$ $meta_me me$ >>
    | <:module_expr@_loc< $anti:s$ >>                                   -> <:patt< $anti:s$ >>
    | <:module_expr@_loc< struct $st$ end >>                            -> <:patt< Ast.MeStr $meta_loc_patt _loc$ $meta_st st$ >>
    | <:module_expr@_loc< ( $me$ : $mt$ ) >>                            -> <:patt< Ast.MeTyc $meta_loc_patt _loc$ $meta_me me$ $meta_mt mt$ >> ]
  
    and class_expr = fun
    [ <:class_expr@_loc<>>                                              -> <:patt< Ast.CeNil $meta_loc_patt _loc$ >>
    | <:class_expr@_loc< $ce$ $e$ >>                                    -> <:patt< Ast.CeApp $meta_loc_patt _loc$ $meta_ce ce$ $meta_e e$ >>
    | <:class_expr@_loc< $opt:v$ $id:i$ >>                              -> <:patt< Ast.CeCon $meta_loc_patt _loc$ $meta_bool _loc v$ $meta_i i$ (Ast.TyNil $meta_loc_patt _loc$) >>
    | <:class_expr@_loc< $opt:v$ $id:i$ [ $t$ ] >>                      -> <:patt< Ast.CeCon $meta_loc_patt _loc$ $meta_bool _loc v$ $meta_i i$ $meta_t t$ >>
    | <:class_expr@_loc< fun $p$ -> $ce$ >>                             -> <:patt< Ast.CeFun $meta_loc_patt _loc$ $meta_p p$ $meta_ce ce$ >>
    | <:class_expr@_loc< let $opt:r$ $bi$ in $ce$ >>                    -> <:patt< Ast.CeLet $meta_loc_patt _loc$ $meta_bool _loc r$ $meta_bi bi$ $meta_ce ce$ >>
    | <:class_expr@_loc< object ($p$) $cst$ end >>                      -> <:patt< Ast.CeStr $meta_loc_patt _loc$ $meta_p p$ $meta_cst cst$ >>
    | <:class_expr@_loc< $anti:s$ >>                                    -> <:patt< $anti:s$ >>
    | <:class_expr@_loc< $ce1$ and $ce2$ >>                             -> <:patt< Ast.CeAnd $meta_loc_patt _loc$ $meta_ce ce1$ $meta_ce ce2$ >>
    | <:class_expr@_loc< ( $ce$ : $ct$ ) >>                             -> <:patt< Ast.CeTyc $meta_loc_patt _loc$ $meta_ce ce$ $meta_ct ct$ >>
    | <:class_expr@_loc< $ce1$ = $ce2$ >>                               -> <:patt< Ast.CeEq $meta_loc_patt _loc$ $meta_ce ce1$ $meta_ce ce2$ >> ]
  
  
    and class_type = fun
    [ <:class_type@_loc<>>                                              -> <:patt< Ast.CtNil $meta_loc_patt _loc$ >>
    | <:class_type@_loc< $opt:v$ $id:i$ >>                              -> <:patt< Ast.CtCon $meta_loc_patt _loc$ $meta_bool _loc v$ $meta_i i$ (Ast.TyNil $meta_loc_patt _loc$) >>
    | <:class_type@_loc< $opt:v$ $id:i$ [ $t$ ] >>                      -> <:patt< Ast.CtCon $meta_loc_patt _loc$ $meta_bool _loc v$ $meta_i i$ $meta_t t$ >>
    | <:class_type@_loc< [ $t$ ] -> $ct$ >>                             -> <:patt< Ast.CtFun $meta_loc_patt _loc$ $meta_t t$ $meta_ct ct$ >>
    | <:class_type@_loc< $anti:s$ >>                                    -> <:patt< $anti:s$ >>
    | <:class_type@_loc< object ($t$) $csg$ end >>                      -> <:patt< Ast.CtSig $meta_loc_patt _loc$ $meta_t t$ $meta_csg csg$ >>
    | <:class_type@_loc< $ct1$ and $ct2$ >>                             -> <:patt< Ast.CtAnd $meta_loc_patt _loc$ $meta_ct ct1$ $meta_ct ct2$ >>
    | <:class_type@_loc< $ct1$ : $ct2$ >>                               -> <:patt< Ast.CtCol $meta_loc_patt _loc$ $meta_ct ct1$ $meta_ct ct2$ >>
    | <:class_type@_loc< $ct1$ = $ct2$ >>                               -> <:patt< Ast.CtEq $meta_loc_patt _loc$ $meta_ct ct1$ $meta_ct ct2$ >> ]
  
    and class_sig_item = fun
    [ <:class_sig_item@_loc<>>                                          -> <:patt< Ast.CgNil $meta_loc_patt _loc$ >>
    | <:class_sig_item@_loc< type $t1$ = $t2$ >>                        -> <:patt< Ast.CgCtr $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:class_sig_item@_loc< $csg1$; $csg2$ >>                          -> <:patt< Ast.CgSem $meta_loc_patt _loc$ $meta_csg csg1$ $meta_csg csg2$ >>
    | <:class_sig_item@_loc< $anti:s$ >>                                -> <:patt< $anti:s$ >>
    | <:class_sig_item@_loc< inherit $ct$ >>                            -> <:patt< Ast.CgInh $meta_loc_patt _loc$ $meta_ct ct$ >>
    | <:class_sig_item@_loc< method $opt:pr$ $s$ : $t$ >>               -> <:patt< Ast.CgMth $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_t t$ >>
    | <:class_sig_item@_loc< method virtual $opt:pr$ $s$ : $t$ >>       -> <:patt< Ast.CgVir $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_t t$ >>
    | <:class_sig_item@_loc< value $opt:m$ $opt:v$ $s$ : $t$ >>         -> <:patt< Ast.CgVal $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc m$ $meta_bool _loc v$ $meta_t t$ >> ]
  
    and class_str_item = fun
    [ <:class_str_item@_loc<>>                                          -> <:patt< Ast.CrNil $meta_loc_patt _loc$ >>
    | <:class_str_item@_loc< $cst1$; $cst2$ >>                          -> <:patt< Ast.CrSem $meta_loc_patt _loc$ $meta_cst cst1$ $meta_cst cst2$ >>
    | <:class_str_item@_loc< $anti:s$ >>                                -> <:patt< $anti:s$ >>
    | <:class_str_item@_loc< type $t1$ = $t2$ >>                        -> <:patt< Ast.CrCtr $meta_loc_patt _loc$ $meta_t t1$ $meta_t t2$ >>
    | <:class_str_item@_loc< inherit $ce$ as $s$ >>                     -> <:patt< Ast.CrInh $meta_loc_patt _loc$ $meta_ce ce$ $meta_s _loc s$ >>
    | <:class_str_item@_loc< initializer $e$ >>                         -> <:patt< Ast.CrIni $meta_loc_patt _loc$ $meta_e e$ >>
    | <:class_str_item@_loc< method $opt:pr$ $s$ : $t$ = $e$ >>         -> <:patt< Ast.CrMth $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_e e$ $meta_t t$ >>
    | <:class_str_item@_loc< method virtual $opt:pr$ $s$ : $t$ >>       -> <:patt< Ast.CrVir $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc pr$ $meta_t t$ >>
    | <:class_str_item@_loc< value $opt:m$ $s$ = $e$ >>                 -> <:patt< Ast.CrVal $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc m$ $meta_e e$ >>
    | <:class_str_item@_loc< value virtual $opt:m$ $s$ : $t$ >>         -> <:patt< Ast.CrVvr $meta_loc_patt _loc$ $meta_s _loc s$ $meta_bool _loc m$ $meta_t t$ >> ];
  
  end;
end;
