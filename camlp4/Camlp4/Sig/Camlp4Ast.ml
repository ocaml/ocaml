(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

(** Signature for OCaml syntax trees.
    This signature is an extension of {!Ast.S}
    It provides:
      - Types for all kinds of structure.
      - Map: A base class for map traversals.
      - Map classes and functions for common kinds. *)
module type S = sig

  module Loc : Loc.S;

  type meta_bool =
    [ BTrue
    | BFalse
    | BAnt of string ];
  type meta_option 'a =
    [ ONone
    | OSome of 'a
    | OAnt of string ];
  type ident =
    [ IdAcc of Loc.t and ident and ident (* i . i *)
    | IdApp of Loc.t and ident and ident (* i i *)
    | IdLid of Loc.t and string (* foo *)
    | IdUid of Loc.t and string (* Bar *)
    | IdAnt of Loc.t and string (* $s$ *) ];
  type ctyp =
    [ TyNil of Loc.t
    | TyAli of Loc.t and ctyp and ctyp (* t as t *) (* list 'a as 'a *)
    | TyAny of Loc.t (* _ *)
    | TyApp of Loc.t and ctyp and ctyp (* t t *) (* list 'a *)
    | TyArr of Loc.t and ctyp and ctyp (* t -> t *) (* int -> string *)
    | TyCls of Loc.t and ident (* #i *) (* #point *)
    | TyLab of Loc.t and string and ctyp (* ~s *)
    | TyId  of Loc.t and ident (* i *) (* Lazy.t *)
    | TyMan of Loc.t and ctyp and ctyp (* t == t *) (* type t = [ A | B ] == Foo.t *)
      (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
    | TyDcl of Loc.t and string and list ctyp and ctyp and list (ctyp * ctyp)
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
    | TyObj of Loc.t and ctyp and meta_bool
    | TyOlb of Loc.t and string and ctyp (* ?s *)
    | TyPol of Loc.t and ctyp and ctyp (* ! t . t *) (* ! 'a . list 'a -> 'a *)
    | TyQuo of Loc.t and string (* 's *)
    | TyQuP of Loc.t and string (* +'s *)
    | TyQuM of Loc.t and string (* -'s *)
    | TyVrn of Loc.t and string (* `s *)
    | TyRec of Loc.t and ctyp (* { t } *) (* { foo : int ; bar : mutable string } *)
    | TyCol of Loc.t and ctyp and ctyp (* t : t *)
    | TySem of Loc.t and ctyp and ctyp (* t; t *)
    | TyCom of Loc.t and ctyp and ctyp (* t, t *)
    | TySum of Loc.t and ctyp (* [ t ] *) (* [ A of int and string | B ] *)
    | TyOf  of Loc.t and ctyp and ctyp (* t of t *) (* A of int *)
    | TyAnd of Loc.t and ctyp and ctyp (* t and t *)
    | TyOr  of Loc.t and ctyp and ctyp (* t | t *)
    | TyPrv of Loc.t and ctyp (* private t *)
    | TyMut of Loc.t and ctyp (* mutable t *)
    | TyTup of Loc.t and ctyp (* ( t ) *) (* (int * string) *)
    | TySta of Loc.t and ctyp and ctyp (* t * t *)
    | TyVrnEq of Loc.t and ctyp (* [ = t ] *)
    | TyVrnSup of Loc.t and ctyp (* [ > t ] *)
    | TyVrnInf of Loc.t and ctyp (* [ < t ] *)
    | TyVrnInfSup of Loc.t and ctyp and ctyp (* [ < t > t ] *)
    | TyAmp of Loc.t and ctyp and ctyp (* t & t *)
    | TyOfAmp of Loc.t and ctyp and ctyp (* t of & t *)
    | TyAnt of Loc.t and string (* $s$ *)
    ]
  ;
  type patt =
    [ PaNil of Loc.t
    | PaId  of Loc.t and ident (* i *)
    | PaAli of Loc.t and patt and patt (* p as p *) (* (Node x y as n) *)
    | PaAnt of Loc.t and string (* $s$ *)
    | PaAny of Loc.t (* _ *)
    | PaApp of Loc.t and patt and patt (* p p *) (* fun x y -> *)
    | PaArr of Loc.t and patt (* [| p |] *)
    | PaCom of Loc.t and patt and patt (* p, p *)
    | PaSem of Loc.t and patt and patt (* p; p *)
    | PaChr of Loc.t and string (* c *) (* 'x' *)
    | PaInt of Loc.t and string
    | PaInt32 of Loc.t and string
    | PaInt64 of Loc.t and string
    | PaNativeInt of Loc.t and string
    | PaFlo of Loc.t and string
    | PaLab of Loc.t and string and patt (* ~s or ~s:(p) *)
    (* ?s or ?s:(p = e) or ?(p = e) *)
    (* | PaOlb of Loc.t and string and meta_option(*FIXME*) (patt * meta_option(*FIXME*) expr) *)
    (* ?s or ?s:(p) *)
    | PaOlb of Loc.t and string and patt
    (* ?s:(p = e) or ?(p = e) *)
    | PaOlbi of Loc.t and string and patt and expr
    | PaOrp of Loc.t and patt and patt (* p | p *)
    | PaRng of Loc.t and patt and patt (* p .. p *)
    | PaRec of Loc.t and patt (* { p } *)
    | PaEq  of Loc.t and patt and patt (* p = p *)
    | PaStr of Loc.t and string (* s *)
    | PaTup of Loc.t and patt (* ( p ) *)
    | PaTyc of Loc.t and patt and ctyp (* (p : t) *)
    | PaTyp of Loc.t and ident (* #i *)
    | PaVrn of Loc.t and string (* `s *) ]
  and expr =
    [ ExNil of Loc.t
    | ExId  of Loc.t and ident (* i *)
    | ExAcc of Loc.t and expr and expr (* e.e *)
    | ExAnt of Loc.t and string (* $s$ *)
    | ExApp of Loc.t and expr and expr (* e e *)
    | ExAre of Loc.t and expr and expr (* e.(e) *)
    | ExArr of Loc.t and expr (* [| e |] *)
    | ExSem of Loc.t and expr and expr (* e; e *)
    | ExAsf of Loc.t (* assert False *)
    | ExAsr of Loc.t and expr (* assert e *)
    | ExAss of Loc.t and expr and expr (* e := e *)
    | ExChr of Loc.t and string (* 'c' *)
    | ExCoe of Loc.t and expr and ctyp and ctyp (* (e : t) or (e : t :> t) *)
    | ExFlo of Loc.t and string (* 3.14 *)
      (* for s = e to/downto e do { e } *)
    | ExFor of Loc.t and string and expr and expr and meta_bool and expr
    | ExFun of Loc.t and match_case (* fun [ a ] *)
    | ExIfe of Loc.t and expr and expr and expr (* if e then e else e *)
    | ExInt of Loc.t and string (* 42 *)
    | ExInt32 of Loc.t and string
    | ExInt64 of Loc.t and string
    | ExNativeInt of Loc.t and string
    | ExLab of Loc.t and string and expr (* ~s or ~s:e *)
    | ExLaz of Loc.t and expr (* lazy e *)
      (* let b in e or let rec b in e *)
    | ExLet of Loc.t and meta_bool and binding and expr
      (* let module s = me in e *)
    | ExLmd of Loc.t and string and module_expr and expr
      (* match e with [ a ] *)
    | ExMat of Loc.t and expr and match_case
      (* new i *)
    | ExNew of Loc.t and ident
      (* object ((p))? (cst)? end *)
    | ExObj of Loc.t and patt and class_str_item
      (* ?s or ?s:e *)
    | ExOlb of Loc.t and string and expr
      (* {< b >} *)
    | ExOvr of Loc.t and binding
      (* { b } or { (e) with b } *)
    | ExRec of Loc.t and binding and expr
      (* do { e } *)
    | ExSeq of Loc.t and expr
      (* e#s *)
    | ExSnd of Loc.t and expr and string
      (* e.[e] *)
    | ExSte of Loc.t and expr and expr
      (* s *) (* "foo" *)
    | ExStr of Loc.t and string
      (* try e with [ a ] *)
    | ExTry of Loc.t and expr and match_case
      (* (e) *)
    | ExTup of Loc.t and expr
      (* e, e *)
    | ExCom of Loc.t and expr and expr
      (* (e : t) *)
    | ExTyc of Loc.t and expr and ctyp
      (* `s *)
    | ExVrn of Loc.t and string
      (* while e do { e } *)
    | ExWhi of Loc.t and expr and expr ]
  and module_type =
      (* i *) (* A.B.C *)
    [ MtId  of Loc.t and ident
      (* functor (s : mt) -> mt *)
    | MtFun of Loc.t and string and module_type and module_type
      (* 's *)
    | MtQuo of Loc.t and string
      (* sig (sg)? end *)
    | MtSig of Loc.t and sig_item
      (* mt with wc *)
    | MtWit of Loc.t and module_type and with_constr 
    | MtAnt of Loc.t and string (* $s$ *) ]
  and sig_item =
    [ SgNil of Loc.t
      (* class cict *)
    | SgCls of Loc.t and class_type
      (* class type cict *)
    | SgClt of Loc.t and class_type
      (* sg ; sg *)
    | SgSem of Loc.t and sig_item and sig_item
      (* # s or # s e *)
    | SgDir of Loc.t and string and expr
      (* exception t *)
    | SgExc of Loc.t and ctyp
      (* |+ external s : t = s ... s +|
    | SgExt of Loc.t and string and ctyp and list string    *)
      (* external s : t = s *)
    | SgExt of Loc.t and string and ctyp and string
      (* include mt *)
    | SgInc of Loc.t and module_type
      (* module s : mt *)
    | SgMod of Loc.t and string and module_type
      (* module rec mb *)
    | SgRecMod of Loc.t and module_binding
      (* module type s = mt *)
    | SgMty of Loc.t and string and module_type
      (* open i *)
    | SgOpn of Loc.t and ident
      (* type t *)
    | SgTyp of Loc.t and ctyp
      (* value s : t *)
    | SgVal of Loc.t and string and ctyp
    | SgAnt of Loc.t and string (* $s$ *) ]
  and with_constr =
    [ WcNil of Loc.t
      (* type t = t *)
    | WcTyp of Loc.t and ctyp and ctyp
      (* module i = i *)
    | WcMod of Loc.t and ident and ident
      (* wc and wc *)
    | WcAnd of Loc.t and with_constr and with_constr
    | WcAnt of Loc.t and string (* $s$ *) ]
  and binding =
    [ BiNil of Loc.t
      (* b and b *) (* let a = 42 and c = 43 *)
    | BiAnd of Loc.t and binding and binding
      (* b ; b *)
    | BiSem of Loc.t and binding and binding
      (* p = e *) (* let patt = expr *)
    | BiEq  of Loc.t and patt and expr
    | BiAnt of Loc.t and string (* $s$ *) ]
  and module_binding =
    [ MbNil of Loc.t
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | MbAnd of Loc.t and module_binding and module_binding
      (* s : mt = me *)
    | MbColEq  of Loc.t and string and module_type and module_expr
      (* s : mt *)
    | MbCol  of Loc.t and string and module_type
    | MbAnt of Loc.t and string (* $s$ *) ]
  and match_case =
    [ McNil of Loc.t
      (* a | a *)
    | McOr of Loc.t and match_case and match_case
      (* p (when e)? -> e *)
    | McArr of Loc.t and patt and expr and expr
    | McAnt of Loc.t and string (* $s$ *) ]
  and module_expr =
      (* i *)
    [ MeId  of Loc.t and ident
      (* me me *)
    | MeApp of Loc.t and module_expr and module_expr
      (* functor (s : mt) -> me *)
    | MeFun of Loc.t and string and module_type and module_expr
      (* struct (st)? end *)
    | MeStr of Loc.t and str_item
      (* (me : mt) *)
    | MeTyc of Loc.t and module_expr and module_type
    | MeAnt of Loc.t and string (* $s$ *) ]
  and str_item =
    [ StNil of Loc.t
      (* class cice *)
    | StCls of Loc.t and class_expr
      (* class type cict *)
    | StClt of Loc.t and class_type
      (* st ; st *)
    | StSem of Loc.t and str_item and str_item
      (* # s or # s e *)
    | StDir of Loc.t and string and expr
      (* exception t or exception t = i *)
    | StExc of Loc.t and ctyp and meta_option(*FIXME*) ident
      (* e *)
    | StExp of Loc.t and expr
      (* |+ external s : t = s ... s +|
    | StExt of Loc.t and string and ctyp and list string    *)
      (* external s : t = s *)
    | StExt of Loc.t and string and ctyp and string
      (* include me *)
    | StInc of Loc.t and module_expr
      (* module s = me *)
    | StMod of Loc.t and string and module_expr
      (* module rec mb *)
    | StRecMod of Loc.t and module_binding
      (* module type s = mt *)
    | StMty of Loc.t and string and module_type
      (* open i *)
    | StOpn of Loc.t and ident
      (* type t *)
    | StTyp of Loc.t and ctyp
      (* value b or value rec b *)
    | StVal of Loc.t and meta_bool and binding
    | StAnt of Loc.t and string (* $s$ *) ]
  and class_type =
    [ CtNil of Loc.t
      (* (virtual)? i ([ t ])? *)
    | CtCon of Loc.t and meta_bool and ident and ctyp
      (* [t] -> ct *)
    | CtFun of Loc.t and ctyp and class_type
      (* object ((t))? (csg)? end *)
    | CtSig of Loc.t and ctyp and class_sig_item
      (* ct and ct *)
    | CtAnd of Loc.t and class_type and class_type
      (* ct : ct *)
    | CtCol of Loc.t and class_type and class_type
      (* ct = ct *)
    | CtEq  of Loc.t and class_type and class_type
      (* $s$ *)
    | CtAnt of Loc.t and string ]
  and class_sig_item =
    [ CgNil of Loc.t
      (* type t = t *)
    | CgCtr of Loc.t and ctyp and ctyp
      (* csg ; csg *)
    | CgSem of Loc.t and class_sig_item and class_sig_item
      (* inherit ct *)
    | CgInh of Loc.t and class_type
      (* method s : t or method private s : t *)
    | CgMth of Loc.t and string and meta_bool and ctyp
      (* value (virtual)? (mutable)? s : t *)
    | CgVal of Loc.t and string and meta_bool and meta_bool and ctyp
      (* method virtual (mutable)? s : t *)
    | CgVir of Loc.t and string and meta_bool and ctyp
    | CgAnt of Loc.t and string (* $s$ *) ]
  and class_expr =
    [ CeNil of Loc.t
      (* ce e *)
    | CeApp of Loc.t and class_expr and expr
      (* (virtual)? i ([ t ])? *)
    | CeCon of Loc.t and meta_bool and ident and ctyp
      (* fun p -> ce *)
    | CeFun of Loc.t and patt and class_expr
      (* let (rec)? b in ce *)
    | CeLet of Loc.t and meta_bool and binding and class_expr
      (* object ((p))? (cst)? end *)
    | CeStr of Loc.t and patt and class_str_item
      (* ce : ct *)
    | CeTyc of Loc.t and class_expr and class_type
      (* ce and ce *)
    | CeAnd of Loc.t and class_expr and class_expr
      (* ce = ce *)
    | CeEq  of Loc.t and class_expr and class_expr
      (* $s$ *)
    | CeAnt of Loc.t and string ]
  and class_str_item =
    [ CrNil of Loc.t
      (* cst ; cst *)
    | CrSem of Loc.t and class_str_item and class_str_item
      (* type t = t *)
    | CrCtr of Loc.t and ctyp and ctyp
      (* inherit ce or inherit ce as s *)
    | CrInh of Loc.t and class_expr and string
      (* initializer e *)
    | CrIni of Loc.t and expr
      (* method (private)? s : t = e or method (private)? s = e *)
    | CrMth of Loc.t and string and meta_bool and expr and ctyp
      (* value (mutable)? s = e *)
    | CrVal of Loc.t and string and meta_bool and expr
      (* method virtual (private)? s : t *)
    | CrVir of Loc.t and string and meta_bool and ctyp
      (* value virtual (private)? s : t *)
    | CrVvr of Loc.t and string and meta_bool and ctyp
    | CrAnt of Loc.t and string (* $s$ *) ];

  value loc_of_ctyp : ctyp -> Loc.t;
  value loc_of_patt : patt -> Loc.t;
  value loc_of_expr : expr -> Loc.t;
  value loc_of_module_type : module_type -> Loc.t;
  value loc_of_module_expr : module_expr -> Loc.t;
  value loc_of_sig_item : sig_item -> Loc.t;
  value loc_of_str_item : str_item -> Loc.t;
  value loc_of_class_type : class_type -> Loc.t;
  value loc_of_class_sig_item : class_sig_item -> Loc.t;
  value loc_of_class_expr : class_expr -> Loc.t;
  value loc_of_class_str_item : class_str_item -> Loc.t;
  value loc_of_with_constr : with_constr -> Loc.t;
  value loc_of_binding : binding -> Loc.t;
  value loc_of_module_binding : module_binding -> Loc.t;
  value loc_of_match_case : match_case -> Loc.t;
  value loc_of_ident : ident -> Loc.t;

  (** See {!Ast.S.map}. *)
  class map : object
    inherit Mapper.c;
    method meta_bool : meta_bool -> meta_bool;
    method meta_option : ! 'a 'b . ('a -> 'b) -> meta_option 'a -> meta_option 'b;
    method _Loc_t : Loc.t -> Loc.t;
    method expr : expr -> expr;
    method patt : patt -> patt;
    method ctyp : ctyp -> ctyp;
    method str_item : str_item -> str_item;
    method sig_item : sig_item -> sig_item;

    method module_expr : module_expr -> module_expr;
    method module_type : module_type -> module_type;
    method class_expr : class_expr -> class_expr;
    method class_type : class_type -> class_type;
    method class_sig_item : class_sig_item -> class_sig_item;
    method class_str_item : class_str_item -> class_str_item;
    method with_constr : with_constr -> with_constr;
    method binding : binding -> binding;
    method module_binding : module_binding -> module_binding;
    method match_case : match_case -> match_case;
    method ident : ident -> ident;
  end;

  (** See {!Ast.S.remove_antiquots}. *)
  (* class remove_antiquots : object inherit map; end; *)

  class c_expr : [expr -> expr] -> object inherit map; end;
  class c_patt : [patt -> patt] -> object inherit map; end;
  class c_ctyp : [ctyp -> ctyp] -> object inherit map; end;
  class c_str_item : [str_item -> str_item] -> object inherit map; end;
  class c_sig_item : [sig_item -> sig_item] -> object inherit map; end;
  class c_loc : [Loc.t -> Loc.t] -> object inherit map; end;
  
  value map_expr : (expr -> expr) -> expr -> expr;
  value map_patt : (patt -> patt) -> patt -> patt;
  value map_ctyp : (ctyp -> ctyp) -> ctyp -> ctyp;
  value map_str_item : (str_item -> str_item) -> str_item -> str_item;
  value map_sig_item : (sig_item -> sig_item) -> sig_item -> sig_item;
  value map_loc : (Loc.t -> Loc.t) -> Loc.t -> Loc.t;

  value ident_of_expr : expr -> ident;
  value ident_of_ctyp : ctyp -> ident;

  value biAnd_of_list : list binding -> binding;
  value biSem_of_list : list binding -> binding;
  value paSem_of_list : list patt -> patt;
  value paCom_of_list : list patt -> patt;
  value tyOr_of_list : list ctyp -> ctyp;
  value tyAnd_of_list : list ctyp -> ctyp;
  value tySem_of_list : list ctyp -> ctyp;
  value stSem_of_list : list str_item -> str_item;
  value sgSem_of_list : list sig_item -> sig_item;
  value crSem_of_list : list class_str_item -> class_str_item;
  value cgSem_of_list : list class_sig_item -> class_sig_item;
  value ctAnd_of_list : list class_type -> class_type;
  value ceAnd_of_list : list class_expr -> class_expr;
  value wcAnd_of_list : list with_constr -> with_constr;
  value meApp_of_list : list module_expr -> module_expr;
  value mbAnd_of_list : list module_binding -> module_binding;
  value asOr_of_list : list match_case -> match_case;
  value idAcc_of_list : list ident -> ident;
  value idApp_of_list : list ident -> ident;
  value exSem_of_list : list expr -> expr;
  value exCom_of_list : list expr -> expr;

  value list_of_ctyp : ctyp -> list ctyp -> list ctyp;
  value list_of_binding : binding -> list binding -> list binding;
  value list_of_with_constr : with_constr -> list with_constr -> list with_constr;
  value list_of_patt : patt -> list patt -> list patt;
  value list_of_expr : expr -> list expr -> list expr;
  value list_of_str_item : str_item -> list str_item -> list str_item;
  value list_of_sig_item : sig_item -> list sig_item -> list sig_item;
  value list_of_class_sig_item : class_sig_item -> list class_sig_item -> list class_sig_item;
  value list_of_class_str_item : class_str_item -> list class_str_item -> list class_str_item;
  value list_of_class_type : class_type -> list class_type -> list class_type;
  value list_of_class_expr : class_expr -> list class_expr -> list class_expr;
  value list_of_module_expr : module_expr -> list module_expr -> list module_expr;
  value list_of_module_binding : module_binding -> list module_binding -> list module_binding;
  value list_of_match_case : match_case -> list match_case -> list match_case;
  value list_of_ident : ident -> list ident -> list ident;

  (** Like [String.escape] but takes care to not
      escape antiquotations strings. *)
  value safe_string_escaped : string -> string;

  value ty_of_stl : (Loc.t * string * list ctyp) -> ctyp;
  value ty_of_sbt : (Loc.t * string * bool * ctyp) -> ctyp;
  value bi_of_pe : (patt * expr) -> binding;
  value pel_of_binding : binding -> list (patt * expr);
  value binding_of_pel : list (patt * expr) -> binding;
  value sum_type_of_list : list (Loc.t * string * list ctyp) -> ctyp;
  value record_type_of_list : list (Loc.t * string * bool * ctyp) -> ctyp;
end;

(** This functor is a restriction functor.
    It takes a Camlp4Ast module and gives the Ast one.
    Typical use is for [with] constraints.
    Example: ... with module Ast = Camlp4.Sig.Camlp4Ast.ToAst Camlp4Ast *)
module ToAst (M : S) : Ast.S
  with module Loc = M.Loc
   and type meta_bool = M.meta_bool
   and type meta_option 'a = M.meta_option 'a
   and type ctyp = M.ctyp
   and type patt = M.patt
   and type expr = M.expr
   and type module_type = M.module_type
   and type sig_item = M.sig_item
   and type with_constr = M.with_constr
   and type module_expr = M.module_expr
   and type str_item = M.str_item
   and type class_type = M.class_type
   and type class_sig_item = M.class_sig_item
   and type class_expr = M.class_expr
   and type class_str_item = M.class_str_item
   and type binding = M.binding
   and type module_binding = M.module_binding
   and type match_case = M.match_case
   and type ident = M.ident
= M;

(** Since the Ast contains locations. This functor produces Ast types
    for a given location type. *)
module Make (Loc : Type.S) = struct

  type meta_bool =
    [ BTrue
    | BFalse
    | BAnt of string ];
  type meta_option 'a =
    [ ONone
    | OSome of 'a
    | OAnt of string ];
  type ident =
    [ IdAcc of Loc.t and ident and ident (* i . i *)
    | IdApp of Loc.t and ident and ident (* i i *)
    | IdLid of Loc.t and string (* foo *)
    | IdUid of Loc.t and string (* Bar *)
    | IdAnt of Loc.t and string (* $s$ *) ];
  type ctyp =
    [ TyNil of Loc.t
    | TyAli of Loc.t and ctyp and ctyp (* t as t *) (* list 'a as 'a *)
    | TyAny of Loc.t (* _ *)
    | TyApp of Loc.t and ctyp and ctyp (* t t *) (* list 'a *)
    | TyArr of Loc.t and ctyp and ctyp (* t -> t *) (* int -> string *)
    | TyCls of Loc.t and ident (* #i *) (* #point *)
    | TyLab of Loc.t and string and ctyp (* ~s *)
    | TyId  of Loc.t and ident (* i *) (* Lazy.t *)
    | TyMan of Loc.t and ctyp and ctyp (* t == t *) (* type t = [ A | B ] == Foo.t *)
      (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
    | TyDcl of Loc.t and string and list ctyp and ctyp and list (ctyp * ctyp)
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
    | TyObj of Loc.t and ctyp and meta_bool
    | TyOlb of Loc.t and string and ctyp (* ?s *)
    | TyPol of Loc.t and ctyp and ctyp (* ! t . t *) (* ! 'a . list 'a -> 'a *)
    | TyQuo of Loc.t and string (* 's *)
    | TyQuP of Loc.t and string (* +'s *)
    | TyQuM of Loc.t and string (* -'s *)
    | TyVrn of Loc.t and string (* `s *)
    | TyRec of Loc.t and ctyp (* { t } *) (* { foo : int ; bar : mutable string } *)
    | TyCol of Loc.t and ctyp and ctyp (* t : t *)
    | TySem of Loc.t and ctyp and ctyp (* t; t *)
    | TyCom of Loc.t and ctyp and ctyp (* t, t *)
    | TySum of Loc.t and ctyp (* [ t ] *) (* [ A of int and string | B ] *)
    | TyOf  of Loc.t and ctyp and ctyp (* t of t *) (* A of int *)
    | TyAnd of Loc.t and ctyp and ctyp (* t and t *)
    | TyOr  of Loc.t and ctyp and ctyp (* t | t *)
    | TyPrv of Loc.t and ctyp (* private t *)
    | TyMut of Loc.t and ctyp (* mutable t *)
    | TyTup of Loc.t and ctyp (* ( t ) *) (* (int * string) *)
    | TySta of Loc.t and ctyp and ctyp (* t * t *)
    | TyVrnEq of Loc.t and ctyp (* [ = t ] *)
    | TyVrnSup of Loc.t and ctyp (* [ > t ] *)
    | TyVrnInf of Loc.t and ctyp (* [ < t ] *)
    | TyVrnInfSup of Loc.t and ctyp and ctyp (* [ < t > t ] *)
    | TyAmp of Loc.t and ctyp and ctyp (* t & t *)
    | TyOfAmp of Loc.t and ctyp and ctyp (* t of & t *)
    | TyAnt of Loc.t and string (* $s$ *)
    ]
  ;
  type patt =
    [ PaNil of Loc.t
    | PaId  of Loc.t and ident (* i *)
    | PaAli of Loc.t and patt and patt (* p as p *) (* (Node x y as n) *)
    | PaAnt of Loc.t and string (* $s$ *)
    | PaAny of Loc.t (* _ *)
    | PaApp of Loc.t and patt and patt (* p p *) (* fun x y -> *)
    | PaArr of Loc.t and patt (* [| p |] *)
    | PaCom of Loc.t and patt and patt (* p, p *)
    | PaSem of Loc.t and patt and patt (* p; p *)
    | PaChr of Loc.t and string (* c *) (* 'x' *)
    | PaInt of Loc.t and string
    | PaInt32 of Loc.t and string
    | PaInt64 of Loc.t and string
    | PaNativeInt of Loc.t and string
    | PaFlo of Loc.t and string
    | PaLab of Loc.t and string and patt (* ~s or ~s:(p) *)
    (* ?s or ?s:(p = e) or ?(p = e) *)
    (* | PaOlb of Loc.t and string and (patt * meta_option(*FIXME*) expr) *)
    (* ?s or ?s:(p) *)
    | PaOlb of Loc.t and string and patt
    (* ?s:(p = e) or ?(p = e) *)
    | PaOlbi of Loc.t and string and patt and expr
    | PaOrp of Loc.t and patt and patt (* p | p *)
    | PaRng of Loc.t and patt and patt (* p .. p *)
    | PaRec of Loc.t and patt (* { p } *)
    | PaEq  of Loc.t and patt and patt (* p = p *)
    | PaStr of Loc.t and string (* s *)
    | PaTup of Loc.t and patt (* ( p ) *)
    | PaTyc of Loc.t and patt and ctyp (* (p : t) *)
    | PaTyp of Loc.t and ident (* #i *)
    | PaVrn of Loc.t and string (* `s *) ]
  and expr =
    [ ExNil of Loc.t
    | ExId  of Loc.t and ident (* i *)
    | ExAcc of Loc.t and expr and expr (* e.e *)
    | ExAnt of Loc.t and string (* $s$ *)
    | ExApp of Loc.t and expr and expr (* e e *)
    | ExAre of Loc.t and expr and expr (* e.(e) *)
    | ExArr of Loc.t and expr (* [| e |] *)
    | ExSem of Loc.t and expr and expr (* e; e *)
    | ExAsf of Loc.t (* assert False *)
    | ExAsr of Loc.t and expr (* assert e *)
    | ExAss of Loc.t and expr and expr (* e := e *)
    | ExChr of Loc.t and string (* 'c' *)
    | ExCoe of Loc.t and expr and ctyp and ctyp (* (e : t) or (e : t :> t) *)
    | ExFlo of Loc.t and string (* 3.14 *)
      (* for s = e to/downto e do { e } *)
    | ExFor of Loc.t and string and expr and expr and meta_bool and expr
    | ExFun of Loc.t and match_case (* fun [ a ] *)
    | ExIfe of Loc.t and expr and expr and expr (* if e then e else e *)
    | ExInt of Loc.t and string (* 42 *)
    | ExInt32 of Loc.t and string
    | ExInt64 of Loc.t and string
    | ExNativeInt of Loc.t and string
    | ExLab of Loc.t and string and expr (* ~s or ~s:e *)
    | ExLaz of Loc.t and expr (* lazy e *)
      (* let b in e or let rec b in e *)
    | ExLet of Loc.t and meta_bool and binding and expr
      (* let module s = me in e *)
    | ExLmd of Loc.t and string and module_expr and expr
      (* match e with [ a ] *)
    | ExMat of Loc.t and expr and match_case
      (* new i *)
    | ExNew of Loc.t and ident
      (* object ((p))? (cst)? end *)
    | ExObj of Loc.t and patt and class_str_item
      (* ?s or ?s:e *)
    | ExOlb of Loc.t and string and expr
      (* {< b >} *)
    | ExOvr of Loc.t and binding
      (* { b } or { (e) with b } *)
    | ExRec of Loc.t and binding and expr
      (* do { e } *)
    | ExSeq of Loc.t and expr
      (* e#s *)
    | ExSnd of Loc.t and expr and string
      (* e.[e] *)
    | ExSte of Loc.t and expr and expr
      (* s *) (* "foo" *)
    | ExStr of Loc.t and string
      (* try e with [ a ] *)
    | ExTry of Loc.t and expr and match_case
      (* (e) *)
    | ExTup of Loc.t and expr
      (* e, e *)
    | ExCom of Loc.t and expr and expr
      (* (e : t) *)
    | ExTyc of Loc.t and expr and ctyp
      (* `s *)
    | ExVrn of Loc.t and string
      (* while e do { e } *)
    | ExWhi of Loc.t and expr and expr ]
  and module_type =
      (* i *) (* A.B.C *)
    [ MtId  of Loc.t and ident
      (* functor (s : mt) -> mt *)
    | MtFun of Loc.t and string and module_type and module_type
      (* 's *)
    | MtQuo of Loc.t and string
      (* sig (sg)? end *)
    | MtSig of Loc.t and sig_item
      (* mt with wc *)
    | MtWit of Loc.t and module_type and with_constr 
    | MtAnt of Loc.t and string (* $s$ *) ]
  and sig_item =
    [ SgNil of Loc.t
      (* class cict *)
    | SgCls of Loc.t and class_type
      (* class type cict *)
    | SgClt of Loc.t and class_type
      (* sg ; sg *)
    | SgSem of Loc.t and sig_item and sig_item
      (* # s or # s e *)
    | SgDir of Loc.t and string and expr
      (* exception t *)
    | SgExc of Loc.t and ctyp
      (* |+ external s : t = s ... s +|
    | SgExt of Loc.t and string and ctyp and list string    *)
      (* external s : t = s *)
    | SgExt of Loc.t and string and ctyp and string
      (* include mt *)
    | SgInc of Loc.t and module_type
      (* module s : mt *)
    | SgMod of Loc.t and string and module_type
      (* module rec mb *)
    | SgRecMod of Loc.t and module_binding
      (* module type s = mt *)
    | SgMty of Loc.t and string and module_type
      (* open i *)
    | SgOpn of Loc.t and ident
      (* type t *)
    | SgTyp of Loc.t and ctyp
      (* value s : t *)
    | SgVal of Loc.t and string and ctyp
    | SgAnt of Loc.t and string (* $s$ *) ]
  and with_constr =
    [ WcNil of Loc.t
      (* type t = t *)
    | WcTyp of Loc.t and ctyp and ctyp
      (* module i = i *)
    | WcMod of Loc.t and ident and ident
      (* wc and wc *)
    | WcAnd of Loc.t and with_constr and with_constr
    | WcAnt of Loc.t and string (* $s$ *) ]
  and binding =
    [ BiNil of Loc.t
      (* b and b *) (* let a = 42 and c = 43 *)
    | BiAnd of Loc.t and binding and binding
      (* b ; b *)
    | BiSem of Loc.t and binding and binding
      (* p = e *) (* let patt = expr *)
    | BiEq  of Loc.t and patt and expr
    | BiAnt of Loc.t and string (* $s$ *) ]
  and module_binding =
    [ MbNil of Loc.t
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | MbAnd of Loc.t and module_binding and module_binding
      (* s : mt = me *)
    | MbColEq  of Loc.t and string and module_type and module_expr
      (* s : mt *)
    | MbCol  of Loc.t and string and module_type
    | MbAnt of Loc.t and string (* $s$ *) ]
  and match_case =
    [ McNil of Loc.t
      (* a | a *)
    | McOr of Loc.t and match_case and match_case
      (* p (when e)? -> e *)
    | McArr of Loc.t and patt and expr and expr
    | McAnt of Loc.t and string (* $s$ *) ]
  and module_expr =
      (* i *)
    [ MeId  of Loc.t and ident
      (* me me *)
    | MeApp of Loc.t and module_expr and module_expr
      (* functor (s : mt) -> me *)
    | MeFun of Loc.t and string and module_type and module_expr
      (* struct (st)? end *)
    | MeStr of Loc.t and str_item
      (* (me : mt) *)
    | MeTyc of Loc.t and module_expr and module_type
    | MeAnt of Loc.t and string (* $s$ *) ]
  and str_item =
    [ StNil of Loc.t
      (* class cice *)
    | StCls of Loc.t and class_expr
      (* class type cict *)
    | StClt of Loc.t and class_type
      (* st ; st *)
    | StSem of Loc.t and str_item and str_item
      (* # s or # s e *)
    | StDir of Loc.t and string and expr
      (* exception t or exception t = i *)
    | StExc of Loc.t and ctyp and meta_option(*FIXME*) ident
      (* e *)
    | StExp of Loc.t and expr
      (* |+ external s : t = s ... s +|
    | StExt of Loc.t and string and ctyp and list string    *)
      (* external s : t = s *)
    | StExt of Loc.t and string and ctyp and string
      (* include me *)
    | StInc of Loc.t and module_expr
      (* module s = me *)
    | StMod of Loc.t and string and module_expr
      (* module rec mb *)
    | StRecMod of Loc.t and module_binding
      (* module type s = mt *)
    | StMty of Loc.t and string and module_type
      (* open i *)
    | StOpn of Loc.t and ident
      (* type t *)
    | StTyp of Loc.t and ctyp
      (* value b or value rec b *)
    | StVal of Loc.t and meta_bool and binding
    | StAnt of Loc.t and string (* $s$ *) ]
  and class_type =
    [ CtNil of Loc.t
      (* (virtual)? i ([ t ])? *)
    | CtCon of Loc.t and meta_bool and ident and ctyp
      (* [t] -> ct *)
    | CtFun of Loc.t and ctyp and class_type
      (* object ((t))? (csg)? end *)
    | CtSig of Loc.t and ctyp and class_sig_item
      (* ct and ct *)
    | CtAnd of Loc.t and class_type and class_type
      (* ct : ct *)
    | CtCol of Loc.t and class_type and class_type
      (* ct = ct *)
    | CtEq  of Loc.t and class_type and class_type
      (* $s$ *)
    | CtAnt of Loc.t and string ]
  and class_sig_item =
    [ CgNil of Loc.t
      (* type t = t *)
    | CgCtr of Loc.t and ctyp and ctyp
      (* csg ; csg *)
    | CgSem of Loc.t and class_sig_item and class_sig_item
      (* inherit ct *)
    | CgInh of Loc.t and class_type
      (* method s : t or method private s : t *)
    | CgMth of Loc.t and string and meta_bool and ctyp
      (* value (virtual)? (mutable)? s : t *)
    | CgVal of Loc.t and string and meta_bool and meta_bool and ctyp
      (* method virtual (mutable)? s : t *)
    | CgVir of Loc.t and string and meta_bool and ctyp
    | CgAnt of Loc.t and string (* $s$ *) ]
  and class_expr =
    [ CeNil of Loc.t
      (* ce e *)
    | CeApp of Loc.t and class_expr and expr
      (* (virtual)? i ([ t ])? *)
    | CeCon of Loc.t and meta_bool and ident and ctyp
      (* fun p -> ce *)
    | CeFun of Loc.t and patt and class_expr
      (* let (rec)? b in ce *)
    | CeLet of Loc.t and meta_bool and binding and class_expr
      (* object ((p))? (cst)? end *)
    | CeStr of Loc.t and patt and class_str_item
      (* ce : ct *)
    | CeTyc of Loc.t and class_expr and class_type
      (* ce and ce *)
    | CeAnd of Loc.t and class_expr and class_expr
      (* ce = ce *)
    | CeEq  of Loc.t and class_expr and class_expr
      (* $s$ *)
    | CeAnt of Loc.t and string ]
  and class_str_item =
    [ CrNil of Loc.t
      (* cst ; cst *)
    | CrSem of Loc.t and class_str_item and class_str_item
      (* type t = t *)
    | CrCtr of Loc.t and ctyp and ctyp
      (* inherit ce or inherit ce as s *)
    | CrInh of Loc.t and class_expr and string
      (* initializer e *)
    | CrIni of Loc.t and expr
      (* method (private)? s : t = e or method (private)? s = e *)
    | CrMth of Loc.t and string and meta_bool and expr and ctyp
      (* value (mutable)? s = e *)
    | CrVal of Loc.t and string and meta_bool and expr
      (* method virtual (private)? s : t *)
    | CrVir of Loc.t and string and meta_bool and ctyp
      (* value virtual (private)? s : t *)
    | CrVvr of Loc.t and string and meta_bool and ctyp
    | CrAnt of Loc.t and string (* $s$ *) ];
end;

