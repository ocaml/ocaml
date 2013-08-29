(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Note: when you modify these types you must increment
   ast magic numbers defined in Camlp4_config.ml. *)

  type loc = Loc.t
   and meta_bool =
    [ BTrue
    | BFalse
    | BAnt of string ]
   and rec_flag =
    [ ReRecursive
    | ReNil
    | ReAnt of string ]
   and direction_flag =
    [ DiTo
    | DiDownto
    | DiAnt of string ]
   and mutable_flag =
    [ MuMutable
    | MuNil
    | MuAnt of string ]
   and private_flag =
    [ PrPrivate
    | PrNil
    | PrAnt of string ]
   and virtual_flag =
    [ ViVirtual
    | ViNil
    | ViAnt of string ]
   and override_flag =
    [ OvOverride
    | OvNil
    | OvAnt of string ]
   and row_var_flag =
    [ RvRowVar
    | RvNil
    | RvAnt of string ]
   and meta_option 'a =
    [ ONone
    | OSome of 'a
    | OAnt of string ]
   and meta_list 'a =
    [ LNil
    | LCons of 'a and meta_list 'a
    | LAnt of string ]
   and ident =
    [ IdAcc of loc and ident and ident (* i . i *)
    | IdApp of loc and ident and ident (* i i *)
    | IdLid of loc and string (* foo *)
    | IdUid of loc and string (* Bar *)
    | IdAnt of loc and string (* $s$ *) ]
   and ctyp =
    [ TyNil of loc
    | TyAli of loc and ctyp and ctyp (* t as t *) (* list 'a as 'a *)
    | TyAny of loc (* _ *)
    | TyApp of loc and ctyp and ctyp (* t t *) (* list 'a *)
    | TyArr of loc and ctyp and ctyp (* t -> t *) (* int -> string *)
    | TyCls of loc and ident (* #i *) (* #point *)
    | TyLab of loc and string and ctyp (* ~s:t *)
    | TyId  of loc and ident (* i *) (* Lazy.t *)
    | TyMan of loc and ctyp and ctyp (* t == t *) (* type t = [ A | B ] == Foo.t *)
      (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
    | TyDcl of loc and string and list ctyp and ctyp and list (ctyp * ctyp)
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
    | TyObj of loc and ctyp and row_var_flag
    | TyOlb of loc and string and ctyp (* ?s:t *)
    | TyPol of loc and ctyp and ctyp (* ! t . t *) (* ! 'a . list 'a -> 'a *)
    | TyTypePol of loc and ctyp and ctyp (* type t . t *) (* type a . list a -> a *)
    | TyQuo of loc and string (* 's *)
    | TyQuP of loc and string (* +'s *)
    | TyQuM of loc and string (* -'s *)
    | TyAnP of loc (* +_ *)
    | TyAnM of loc (* -_ *)
    | TyVrn of loc and string (* `s *)
    | TyRec of loc and ctyp (* { t } *) (* { foo : int ; bar : mutable string } *)
    | TyCol of loc and ctyp and ctyp (* t : t *)
    | TySem of loc and ctyp and ctyp (* t; t *)
    | TyCom of loc and ctyp and ctyp (* t, t *)
    | TySum of loc and ctyp (* [ t ] *) (* [ A of int and string | B ] *)
    | TyOf  of loc and ctyp and ctyp (* t of t *) (* A of int *)
    | TyAnd of loc and ctyp and ctyp (* t and t *)
    | TyOr  of loc and ctyp and ctyp (* t | t *)
    | TyPrv of loc and ctyp (* private t *)
    | TyMut of loc and ctyp (* mutable t *)
    | TyTup of loc and ctyp (* ( t ) *) (* (int * string) *)
    | TySta of loc and ctyp and ctyp (* t * t *)
    | TyVrnEq of loc and ctyp (* [ = t ] *)
    | TyVrnSup of loc and ctyp (* [ > t ] *)
    | TyVrnInf of loc and ctyp (* [ < t ] *)
    | TyVrnInfSup of loc and ctyp and ctyp (* [ < t > t ] *)
    | TyAmp of loc and ctyp and ctyp (* t & t *)
    | TyOfAmp of loc and ctyp and ctyp (* t of & t *)
    | TyPkg of loc and module_type (* (module S) *)
    | TyAtt of loc and string and str_item and ctyp  (* .. [@attr] *)
    | TyAnt of loc and string (* $s$ *)
    ]
   and patt =
    [ PaNil of loc
    | PaId  of loc and ident (* i *)
    | PaAli of loc and patt and patt (* p as p *) (* (Node x y as n) *)
    | PaAnt of loc and string (* $s$ *)
    | PaAny of loc (* _ *)
    | PaApp of loc and patt and patt (* p p *) (* fun x y -> *)
    | PaArr of loc and patt (* [| p |] *)
    | PaCom of loc and patt and patt (* p, p *)
    | PaSem of loc and patt and patt (* p; p *)
    | PaChr of loc and string (* c *) (* 'x' *)
    | PaInt of loc and string
    | PaInt32 of loc and string
    | PaInt64 of loc and string
    | PaNativeInt of loc and string
    | PaFlo of loc and string
    | PaLab of loc and string and patt (* ~s or ~s:(p) *)
    (* ?s or ?s:(p) *)
    | PaOlb of loc and string and patt
    (* ?s:(p = e) or ?(p = e) *)
    | PaOlbi of loc and string and patt and expr
    | PaOrp of loc and patt and patt (* p | p *)
    | PaRng of loc and patt and patt (* p .. p *)
    | PaRec of loc and patt (* { p } *)
    | PaEq  of loc and ident and patt (* i = p *)
    | PaStr of loc and string (* s *)
    | PaTup of loc and patt (* ( p ) *)
    | PaTyc of loc and patt and ctyp (* (p : t) *)
    | PaTyp of loc and ident (* #i *)
    | PaVrn of loc and string (* `s *)
    | PaLaz of loc and patt (* lazy p *)
    | PaAtt of loc and string and str_item and patt  (* .. [@attr] *)
    | PaMod of loc and string (* (module M) *) ]
  and expr =
    [ ExNil of loc
    | ExId  of loc and ident (* i *)
    | ExAcc of loc and expr and expr (* e.e *)
    | ExAnt of loc and string (* $s$ *)
    | ExApp of loc and expr and expr (* e e *)
    | ExAre of loc and expr and expr (* e.(e) *)
    | ExArr of loc and expr (* [| e |] *)
    | ExSem of loc and expr and expr (* e; e *)
    | ExAsf of loc (* assert False *)
    | ExAsr of loc and expr (* assert e *)
    | ExAss of loc and expr and expr (* e := e *)
    | ExChr of loc and string (* 'c' *)
    | ExCoe of loc and expr and ctyp and ctyp (* (e : t) or (e : t :> t) *)
    | ExFlo of loc and string (* 3.14 *)
      (* for s = e to/downto e do { e } *)
    | ExFor of loc and string and expr and expr and direction_flag and expr
    | ExFun of loc and match_case (* fun [ mc ] *)
    | ExIfe of loc and expr and expr and expr (* if e then e else e *)
    | ExInt of loc and string (* 42 *)
    | ExInt32 of loc and string
    | ExInt64 of loc and string
    | ExNativeInt of loc and string
    | ExLab of loc and string and expr (* ~s or ~s:e *)
    | ExLaz of loc and expr (* lazy e *)
      (* let b in e or let rec b in e *)
    | ExLet of loc and rec_flag and binding and expr
      (* let module s = me in e *)
    | ExLmd of loc and string and module_expr and expr
      (* match e with [ mc ] *)
    | ExMat of loc and expr and match_case
      (* new i *)
    | ExNew of loc and ident
      (* object ((p))? (cst)? end *)
    | ExObj of loc and patt and class_str_item
      (* ?s or ?s:e *)
    | ExOlb of loc and string and expr
      (* {< rb >} *)
    | ExOvr of loc and rec_binding
      (* { rb } or { (e) with rb } *)
    | ExRec of loc and rec_binding and expr
      (* do { e } *)
    | ExSeq of loc and expr
      (* e#s *)
    | ExSnd of loc and expr and string
      (* e.[e] *)
    | ExSte of loc and expr and expr
      (* s *) (* "foo" *)
    | ExStr of loc and string
      (* try e with [ mc ] *)
    | ExTry of loc and expr and match_case
      (* (e) *)
    | ExTup of loc and expr
      (* e, e *)
    | ExCom of loc and expr and expr
      (* (e : t) *)
    | ExTyc of loc and expr and ctyp
      (* `s *)
    | ExVrn of loc and string
      (* while e do { e } *)
    | ExWhi of loc and expr and expr
      (* let open i in e *)
    | ExOpI of loc and ident and override_flag and expr
      (* fun (type t) -> e *)
      (* let f x (type t) y z = e *)
    | ExFUN of loc and string and expr
      (* (module ME : S) which is represented as (module (ME : S)) *)
    | ExPkg of loc and module_expr
      (* e [@attr] *)
    | ExAtt of loc and string and str_item and expr
  ]
  and module_type =
    [ MtNil of loc
      (* i *) (* A.B.C *)
    | MtId  of loc and ident
      (* functor (s : mt) -> mt *)
    | MtFun of loc and string and module_type and module_type
      (* 's *)
    | MtQuo of loc and string
      (* sig sg end *)
    | MtSig of loc and sig_item
      (* mt with wc *)
    | MtWit of loc and module_type and with_constr
      (* module type of m *)
    | MtOf of loc and module_expr
    | MtAtt of loc and string and str_item and module_type  (* .. [@attr] *)
    | MtAnt of loc and string (* $s$ *) ]
  and sig_item =
    [ SgNil of loc
      (* class cict *)
    | SgCls of loc and class_type
      (* class type cict *)
    | SgClt of loc and class_type
      (* sg ; sg *)
    | SgSem of loc and sig_item and sig_item
      (* # s or # s e *)
    | SgDir of loc and string and expr
      (* exception t *)
    | SgExc of loc and ctyp
      (* external s : t = s ... s *)
    | SgExt of loc and string and ctyp and meta_list string
      (* include mt *)
    | SgInc of loc and module_type
      (* module s : mt *)
    | SgMod of loc and string and module_type
      (* module rec mb *)
    | SgRecMod of loc and module_binding
      (* module type s = mt *)
    | SgMty of loc and string and module_type
      (* open i *)
    | SgOpn of loc and ident
      (* type t *)
    | SgTyp of loc and ctyp
      (* value s : t *)
    | SgVal of loc and string and ctyp
    | SgAnt of loc and string (* $s$ *) ]
  and with_constr =
    [ WcNil of loc
      (* type t = t *)
    | WcTyp of loc and ctyp and ctyp
      (* module i = i *)
    | WcMod of loc and ident and ident
      (* type t := t *)
    | WcTyS of loc and ctyp and ctyp
      (* module i := i *)
    | WcMoS of loc and ident and ident
      (* wc and wc *)
    | WcAnd of loc and with_constr and with_constr
    | WcAnt of loc and string (* $s$ *) ]
  and binding =
    [ BiNil of loc
      (* bi and bi *) (* let a = 42 and c = 43 *)
    | BiAnd of loc and binding and binding
      (* p = e *) (* let patt = expr *)
    | BiEq  of loc and patt and expr
    | BiAnt of loc and string (* $s$ *) ]
  and rec_binding =
    [ RbNil of loc
      (* rb ; rb *)
    | RbSem of loc and rec_binding and rec_binding
      (* i = e *)
    | RbEq  of loc and ident and expr
    | RbAnt of loc and string (* $s$ *) ]
  and module_binding =
    [ MbNil of loc
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | MbAnd of loc and module_binding and module_binding
      (* s : mt = me *)
    | MbColEq  of loc and string and module_type and module_expr
      (* s : mt *)
    | MbCol  of loc and string and module_type
    | MbAnt of loc and string (* $s$ *) ]
  and match_case =
    [ McNil of loc
      (* a | a *)
    | McOr of loc and match_case and match_case
      (* p (when e)? -> e *)
    | McArr of loc and patt and expr and expr
    | McAnt of loc and string (* $s$ *) ]
  and module_expr =
    [ MeNil of loc
      (* i *)
    | MeId  of loc and ident
      (* me me *)
    | MeApp of loc and module_expr and module_expr
      (* functor (s : mt) -> me *)
    | MeFun of loc and string and module_type and module_expr
      (* struct st end *)
    | MeStr of loc and str_item
      (* (me : mt) *)
    | MeTyc of loc and module_expr and module_type
      (* (value e) *)
      (* (value e : S) which is represented as (value (e : S)) *)
    | MePkg of loc and expr
    | MeAtt of loc and string and str_item and module_expr  (* .. [@attr] *)
    | MeAnt of loc and string (* $s$ *) ]
  and str_item =
    [ StNil of loc
      (* class cice *)
    | StCls of loc and class_expr
      (* class type cict *)
    | StClt of loc and class_type
      (* st ; st *)
    | StSem of loc and str_item and str_item
      (* # s or # s e *)
    | StDir of loc and string and expr
      (* exception t or exception t = i *)
    | StExc of loc and ctyp and meta_option(*FIXME*) ident
      (* e *)
    | StExp of loc and expr
      (* external s : t = s ... s *)
    | StExt of loc and string and ctyp and meta_list string
      (* include me *)
    | StInc of loc and module_expr
      (* module s = me *)
    | StMod of loc and string and module_expr
      (* module rec mb *)
    | StRecMod of loc and module_binding
      (* module type s = mt *)
    | StMty of loc and string and module_type
      (* open i *)
    | StOpn of loc and override_flag and ident
      (* type t *)
    | StTyp of loc and ctyp
      (* value (rec)? bi *)
    | StVal of loc and rec_flag and binding
    | StAnt of loc and string (* $s$ *) ]
  and class_type =
    [ CtNil of loc
      (* (virtual)? i ([ t ])? *)
    | CtCon of loc and virtual_flag and ident and ctyp
      (* [t] -> ct *)
    | CtFun of loc and ctyp and class_type
      (* object ((t))? (csg)? end *)
    | CtSig of loc and ctyp and class_sig_item
      (* ct and ct *)
    | CtAnd of loc and class_type and class_type
      (* ct : ct *)
    | CtCol of loc and class_type and class_type
      (* ct = ct *)
    | CtEq  of loc and class_type and class_type
      (* $s$ *)
    | CtAtt of loc and string and str_item and class_type  (* .. [@attr] *)
    | CtAnt of loc and string ]
  and class_sig_item =
    [ CgNil of loc
      (* type t = t *)
    | CgCtr of loc and ctyp and ctyp
      (* csg ; csg *)
    | CgSem of loc and class_sig_item and class_sig_item
      (* inherit ct *)
    | CgInh of loc and class_type
      (* method s : t or method private s : t *)
    | CgMth of loc and string and private_flag and ctyp
      (* value (virtual)? (mutable)? s : t *)
    | CgVal of loc and string and mutable_flag and virtual_flag and ctyp
      (* method virtual (private)? s : t *)
    | CgVir of loc and string and private_flag and ctyp
    | CgAnt of loc and string (* $s$ *) ]
  and class_expr =
    [ CeNil of loc
      (* ce e *)
    | CeApp of loc and class_expr and expr
      (* (virtual)? i ([ t ])? *)
    | CeCon of loc and virtual_flag and ident and ctyp
      (* fun p -> ce *)
    | CeFun of loc and patt and class_expr
      (* let (rec)? bi in ce *)
    | CeLet of loc and rec_flag and binding and class_expr
      (* object ((p))? (cst)? end *)
    | CeStr of loc and patt and class_str_item
      (* ce : ct *)
    | CeTyc of loc and class_expr and class_type
      (* ce and ce *)
    | CeAnd of loc and class_expr and class_expr
      (* ce = ce *)
    | CeEq  of loc and class_expr and class_expr
      (* $s$ *)
    | CeAtt of loc and string and str_item and class_expr  (* .. [@attr] *)
    | CeAnt of loc and string ]
  and class_str_item =
    [ CrNil of loc
      (* cst ; cst *)
    | CrSem of loc and class_str_item and class_str_item
      (* type t = t *)
    | CrCtr of loc and ctyp and ctyp
      (* inherit(!)? ce (as s)? *)
    | CrInh of loc and override_flag and class_expr and string
      (* initializer e *)
    | CrIni of loc and expr
      (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
    | CrMth of loc and string and override_flag and private_flag and expr and ctyp
      (* value(!)? (mutable)? s = e *)
    | CrVal of loc and string and override_flag and mutable_flag and expr
      (* method virtual (private)? s : t *)
    | CrVir of loc and string and private_flag and ctyp
      (* value virtual (mutable)? s : t *)
    | CrVvr of loc and string and mutable_flag and ctyp
    | CrAnt of loc and string (* $s$ *) ];
