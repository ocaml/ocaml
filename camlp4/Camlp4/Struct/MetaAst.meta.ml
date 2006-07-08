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

  <:start_meta< expr, patt >>

  value rec meta_list _loc mf =
    fun
    [ [] -> <:meta_kind< [] >>
    | [x :: xs] -> <:meta_kind< [ $mf x$ :: $meta_list _loc mf xs$ ] >> ];

  value meta_bool _loc =
    fun   
    [ Ast.BFalse -> <:meta_kind< Ast.BFalse >>
    | Ast.BTrue -> <:meta_kind< Ast.BTrue >>
    | Ast.BAnt s -> <:meta_kind< $anti:s$ >> ]; 

  value meta_chr _ x = x;
  value meta_flo _ x = x;
  value meta_int _ x = x;
  value meta_lid _ x = x;
  value meta_uid _ x = x;
  value meta_str _loc s = <:meta_kind< $`str:s$ >>;
  value meta_s _loc s = <:meta_kind< $str:s$ >>;
  value meta_c x = x;
  value meta_b = meta_bool;
  value meta_to = meta_bool;

  value rec meta_e x = expr x
  and meta_bi x = binding x
  and meta_a x = match_case x
  and meta_mb x = module_binding x
  and meta_p x = patt x
  and meta_t x = ctyp x
  and meta_mt x = module_type x
  (* and meta_sl _loc x = meta_list _loc meta_s x *)
  and meta_tt _loc (t1, t2) = <:meta_kind< ($ctyp t1$, $ctyp t2$) >>
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
  [ <:meta< <:with_constr<>> >>
  | <:meta< <:with_constr< type $t1$ = $t2$ >> >>
  | <:meta< <:with_constr< $wc1$ and $wc2$ >> >>
  | <:meta< <:with_constr< module $i1$ = $i2$ >> >>
  | <:meta< <:with_constr< $anti:s$ >> >> ]

  and ident = fun
  [ <:meta< <:ident< $i1$.$i2$ >> >>
  | <:meta< <:ident< $i1$ $i2$ >> >>
  | <:meta< <:ident< $anti:s$ >> >>
  | <:meta< <:ident< $lid:s$ >> >>
  | <:meta< <:ident< $uid:s$ >> >> ]
  and meta_i i = ident i

  and expr = fun
  [ <:meta< <:expr<>> >>
  | <:meta< <:expr< $id:i$ >> >>
  | <:meta< <:expr< $anti:s$ >> >>
  | <:meta< <:expr< $e1$ $e2$ >> >>
  | <:meta< <:expr< $e1$ .( $e2$ ) >> >>
  | <:meta< <:expr< [| $e$ |] >> >>
  | <:meta< <:expr< $e1$ := $e2$ >> >>
  | <:meta< <:expr< $chr:c$ >> >>
  | <:meta< <:expr< ($e$ :> $t$) >> >>
  | <:meta< <:expr< ($e$ : $t1$ :> $t2$) >> >>
  | <:meta< <:expr< $flo:s$ >> >>
  | <:meta< <:expr< for $s$ = $e1$ $to:b$ $e2$ do { $e3$ } >> >>
  | <:meta< <:expr< fun [ $a$ ] >> >>
  | <:meta< <:expr< if $e1$ then $e2$ else $e3$ >> >>
  | <:meta< <:expr< $int:s$ >> >>
  | <:meta< <:expr< lazy $e$ >> >>
  | <:meta< <:expr< let $rec:r$ $bi$ in $e$ >> >>
  | <:meta< <:expr< match $e$ with [ $a$ ] >> >>
  | <:meta< <:expr< { $bi$ } >> >>
  | <:meta< <:expr< { ($e$) with $bi$ } >> >>
  | <:meta< <:expr< do { $e$ } >> >>
  | <:meta< <:expr< $e1$ .[ $e2$ ] >> >>
  | <:meta< <:expr< $str:s$ >> >>
  | <:meta< <:expr< try $e$ with [ $a$ ] >> >>
  | <:meta< <:expr< ( $tup:e$ ) >> >>
  | <:meta< <:expr< ( $e$ : $t$ ) >> >>
  | <:meta< <:expr< while $e1$ do { $e2$ } >> >>
  | <:meta< <:expr< `$s$ >> >>
  | <:meta< <:expr< $e$ # $s$ >> >>
  | <:meta< <:expr< ~ $s$ >> >>
  | <:meta< <:expr< ~ $s$ : $e$ >> >>
  | <:meta< <:expr< ? $s$ >> >>
  | <:meta< <:expr< ? $s$ : $e$ >> >>
  | <:meta< <:expr< assert $e$ >> >>
  | <:meta< <:expr< new $i$ >> >>
  | <:meta< <:expr< {< $bi$ >} >> >>
  | <:meta< <:expr< let module $s$ = $me$ in $e$ >> >>
  | <:meta< <:expr< assert False >> >>
  | <:meta< <:expr< $int32:s$ >> >>
  | <:meta< <:expr< $int64:s$ >> >>
  | <:meta< <:expr< $nativeint:s$ >> >>
  | <:meta< <:expr< object ($p$) $cst$ end >> >>
  | <:meta< <:expr< $e1$, $e2$ >> >>
  | <:meta< <:expr< $e1$ . $e2$ >> >>
  | <:meta< <:expr< $e1$; $e2$ >> >> ]

  and patt = fun
  [ <:meta< <:patt<>> >>
  | <:meta< <:patt< $id:i$ >> >>
  | <:meta< <:patt< $anti:s$ >> >>
  | <:meta< <:patt< ( $p1$ as $p2$ ) >> >>
  | <:meta< <:patt< _ >> >>
  | <:meta< <:patt< $p1$ $p2$ >> >>
  | <:meta< <:patt< $chr:c$ >> >>
  | <:meta< <:patt< $int:s$ >> >>
  | <:meta< <:patt< $p1$ | $p2$ >> >>
  | <:meta< <:patt< $p1$ .. $p2$ >> >>
  | <:meta< <:patt< { $p$ } >> >>
  | <:meta< <:patt< $str:s$ >> >>
  | <:meta< <:patt< ( $tup:p$ ) >> >>
  | <:meta< <:patt< ( $p$ : $t$ ) >> >>
  | <:meta< <:patt< $int32:s$ >> >>
  | <:meta< <:patt< $int64:s$ >> >>
  | <:meta< <:patt< $nativeint:s$ >> >>
  | <:meta< <:patt< `$s$ >> >>
  | <:meta< <:patt< [| $p$ |] >> >>
  | <:meta< <:patt< $flo:f$ >> >>
  | <:meta< <:patt< ~ $s$ >> >>
  | <:meta< <:patt< ? $s$ >> >>
  | <:meta< <:patt< ~ $s$ : ($p$) >> >>
  | <:meta< <:patt< ? $s$ : ($p$) >> >>
  | <:meta< <:patt< ? $s$ : ($p$ = $e$) >> >>
  | <:meta< <:patt< # $i$ >> >>
  | <:meta< <:patt< $p1$, $p2$ >> >>
  | <:meta< <:patt< $p1$ = $p2$ >> >>
  | <:meta< <:patt< $p1$; $p2$ >> >> ]

  and match_case = fun
  [ <:meta< <:match_case<>> >>
  | <:meta< <:match_case< $a1$ | $a2$ >> >>
  | <:meta< <:match_case< $p$ when $e1$ -> $e2$ >> >>
  | <:meta< <:match_case< $anti:s$ >> >> ]

  and binding = fun
  [ <:meta< <:binding<>> >>
  | <:meta< <:binding< $bi1$ and $bi2$ >> >>
  | <:meta< <:binding< $bi1$ ; $bi2$ >> >>
  | <:meta< <:binding< $p$ = $e$ >> >>
  | <:meta< <:binding< $anti:s$ >> >> ]

  and module_binding = fun
  [ <:meta< <:module_binding<>> >>
  | <:meta< <:module_binding< $mb1$ and $mb2$ >> >>
  | <:meta< <:module_binding< $s$ : $mt$ = $me$ >> >>
  | <:meta< <:module_binding< $s$ : $mt$ >> >>
  | <:meta< <:module_binding< $anti:s$ >> >> ]

  and ctyp = fun
  [ <:meta< <:ctyp<>> >>
  | <:meta< <:ctyp< $id:i$ >> >>
  | <:meta< <:ctyp< $t1$ as $t2$ >> >>
  | <:meta< <:ctyp< _ >> >>
  | <:meta< <:ctyp< $t1$ $t2$ >> >>
  | <:meta< <:ctyp< $t1$ -> $t2$ >> >>
  | <:meta< <:ctyp< $t1$ | $t2$ >> >>
  | <:meta< <:ctyp< $t1$ of $t2$ >> >>
  | <:meta< <:ctyp< $t1$ and $t2$ >> >>
  | <:meta< <:ctyp< $t1$; $t2$ >> >>
  | <:meta< <:ctyp< $t1$, $t2$ >> >>
  | <:meta< <:ctyp< $t1$ : $t2$ >> >>
  | <:meta< <:ctyp< mutable $t$ >> >>
  | <:meta< <:ctyp< # $i$ >> >>
  | <:meta< <:ctyp< ~ $s$ : $t$ >> >>
  | <:meta< <:ctyp< $t1$ == $t2$ >> >>
  | <:meta< <:ctyp< < $t$ $..:b$ > >> >>
  | <:meta< <:ctyp< ? $s$ : $t$ >> >>
  | <:meta< <:ctyp< ! $t1$ . $t2$ >> >>
  | <:meta< <:ctyp< '$s$ >> >>
  | <:meta< <:ctyp< { $t$ } >> >>
  | <:meta< <:ctyp< [ $t$ ] >> >>
  | <:meta< <:ctyp< ( $tup:t$ ) >> >>
  | <:meta< <:ctyp< [ = $t$ ] >> >>
  | <:meta< <:ctyp< [ > $t$ ] >> >>
  | <:meta< <:ctyp< [ < $t$ ] >> >>
  | <:meta< <:ctyp< [ < $t1$ > $t2$ ] >> >>
  | <:meta< <:ctyp< private $t$ >> >>
  | <:meta< <:ctyp< `$s$ >> >>
  | <:meta< <:ctyp< $t1$ of & $t2$ >> >>
  | <:meta< <:ctyp< $t1$ & $t2$ >> >>
  | <:meta< <:ctyp< $t1$ * $t2$ >> >>
  | <:meta< <:ctyp< +'$s$ >> >>
  | <:meta< <:ctyp< -'$s$ >> >>
  | <:meta< <:ctyp< $anti:s$ >> >>
  | Ast.TyDcl _loc s tl t ttl ->
      <:meta_kind< Ast.TyDcl $meta_loc_meta_kind _loc$ $str:s$
                             $meta_list _loc ctyp tl$ $ctyp t$
                             $meta_list _loc (meta_tt _loc) ttl$ >> ]


  and sig_item = fun
  [ <:meta< <:sig_item<>> >>
  | <:meta< <:sig_item< $sg1$; $sg2$ >> >>
  | <:meta< <:sig_item< exception $t$ >> >>
  (* | <:me--ta< <:sig_item< external $s$ : $t$ = $list:sl$ >> >> *)
  | <:meta< <:sig_item< external $s1$ : $t$ = $s2$ >> >>
  | <:meta< <:sig_item< module $s$ : $mt$ >> >>
  | <:meta< <:sig_item< module type $s$ = $mt$ >> >>
  | <:meta< <:sig_item< open $i$ >> >>
  | <:meta< <:sig_item< type $t$ >> >>
  | <:meta< <:sig_item< value $s$ : $t$ >> >>
  | <:meta< <:sig_item< include $mt$ >> >>
  | <:meta< <:sig_item< class $ct$ >> >>
  | <:meta< <:sig_item< class type $ct$ >> >>
  | <:meta< <:sig_item< module rec $mb$ >> >>
  | <:meta< <:sig_item< # $s$ $e$ >> >>
  | <:meta< <:sig_item< $anti:s$ >> >> ]

  and str_item = fun
  [ <:meta< <:str_item<>> >>
  | <:meta< <:str_item< $st1$; $st2$ >> >>
  | <:meta< <:str_item< exception $t$ >> >>
  | <:meta< <:str_item< exception $t$ = $i$ >> >>
  | <:meta< <:str_item< $exp:e$ >> >>
  (* | <:me--ta< <:str_item< external $s$ : $t$ = $list:sl$ >> >> *)
  | <:meta< <:str_item< external $s1$ : $t$ = $s2$ >> >>
  | <:meta< <:str_item< module $s$ = $me$ >> >>
  | <:meta< <:str_item< module type $s$ = $mt$ >> >>
  | <:meta< <:str_item< open $i$ >> >>
  | <:meta< <:str_item< type $t$ >> >>
  | <:meta< <:str_item< value $rec:r$ $bi$ >> >>
  | <:meta< <:str_item< include $me$ >> >>
  | <:meta< <:str_item< class $ce$ >> >>
  | <:meta< <:str_item< class type $ct$ >> >>
  | <:meta< <:str_item< module rec $mb$ >> >>
  | <:meta< <:str_item< # $s$ $e$ >> >>
  | <:meta< <:str_item< $anti:s$ >> >>
  | _ -> assert False
  ]


  and module_type = fun
  [ <:meta< <:module_type< $id:i$ >> >>
  | <:meta< <:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >> >>
  | <:meta< <:module_type< '$s$ >> >>
  | <:meta< <:module_type< $anti:s$ >> >>
  | <:meta< <:module_type< sig $sg$ end >> >>
  | <:meta< <:module_type< $mt$ with $wc$ >> >> ]

  and module_expr = fun
  [ <:meta< <:module_expr< $id:i$ >> >>
  | <:meta< <:module_expr< $me1$ $me2$ >> >>
  | <:meta< <:module_expr< functor ( $s$ : $mt$ ) -> $me$ >> >>
  | <:meta< <:module_expr< $anti:s$ >> >>
  | <:meta< <:module_expr< struct $st$ end >> >>
  | <:meta< <:module_expr< ( $me$ : $mt$ ) >> >> ]

  and class_expr = fun
  [ <:meta< <:class_expr<>> >>
  | <:meta< <:class_expr< $ce$ $e$ >> >>
  | <:meta< <:class_expr< $virtual:v$ $id:i$ >> >>
  | <:meta< <:class_expr< $virtual:v$ $id:i$ [ $t$ ] >> >>
  | <:meta< <:class_expr< fun $p$ -> $ce$ >> >>
  | <:meta< <:class_expr< let $rec:r$ $bi$ in $ce$ >> >>
  | <:meta< <:class_expr< object ($p$) $cst$ end >> >>
  | <:meta< <:class_expr< $anti:s$ >> >>
  | <:meta< <:class_expr< $ce1$ and $ce2$ >> >>
  | <:meta< <:class_expr< ( $ce$ : $ct$ ) >> >>
  | <:meta< <:class_expr< $ce1$ = $ce2$ >> >> ]


  and class_type = fun
  [ <:meta< <:class_type<>> >>
  | <:meta< <:class_type< $virtual:v$ $id:i$ >> >>
  | <:meta< <:class_type< $virtual:v$ $id:i$ [ $t$ ] >> >>
  | <:meta< <:class_type< [ $t$ ] -> $ct$ >> >>
  | <:meta< <:class_type< $anti:s$ >> >>
  | <:meta< <:class_type< object ($t$) $csg$ end >> >>
  | <:meta< <:class_type< $ct1$ and $ct2$ >> >>
  | <:meta< <:class_type< $ct1$ : $ct2$ >> >>
  | <:meta< <:class_type< $ct1$ = $ct2$ >> >> ]

  and class_sig_item = fun
  [ <:meta< <:class_sig_item<>> >>
  | <:meta< <:class_sig_item< type $t1$ = $t2$ >> >>
  | <:meta< <:class_sig_item< $csg1$; $csg2$ >> >>
  | <:meta< <:class_sig_item< $anti:s$ >> >>
  | <:meta< <:class_sig_item< inherit $ct$ >> >>
  | <:meta< <:class_sig_item< method $private:pr$ $s$ : $t$ >> >>
  | <:meta< <:class_sig_item< method virtual $private:pr$ $s$ : $t$ >> >>
  | <:meta< <:class_sig_item< value $mutable:m$ $virtual:v$ $s$ : $t$ >> >> ]

  and class_str_item = fun
  [ <:meta< <:class_str_item<>> >>
  | <:meta< <:class_str_item< $cst1$; $cst2$ >> >>
  | <:meta< <:class_str_item< $anti:s$ >> >>
  | <:meta< <:class_str_item< type $t1$ = $t2$ >> >>
  | <:meta< <:class_str_item< inherit $ce$ as $s$ >> >>
  | <:meta< <:class_str_item< initializer $e$ >> >>
  | <:meta< <:class_str_item< method $private:pr$ $s$ : $t$ = $e$ >> >>
  | <:meta< <:class_str_item< method virtual $private:pr$ $s$ : $t$ >> >>
  | <:meta< <:class_str_item< value $mutable:m$ $s$ = $e$ >> >>
  | <:meta< <:class_str_item< value virtual $mutable:m$ $s$ : $t$ >> >> ];

  <:stop_meta<>>
end;
