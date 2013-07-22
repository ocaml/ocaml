(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)



module Make (Ast : Sig.Camlp4Ast) = struct
  open Format;
  open Camlp4_import.Parsetree;
  open Camlp4_import.Longident;
  open Camlp4_import.Asttypes;
  open Ast;

  value error loc str = Loc.raise loc (Failure str);

  value char_of_char_token loc s =
    try Token.Eval.char s with [ Failure _ as exn -> Loc.raise loc exn ]
  ;

  value string_of_string_token loc s =
    try Token.Eval.string s
    with [ Failure _ as exn -> Loc.raise loc exn ]
  ;

  value remove_underscores s =
    let l = String.length s in
    let rec remove src dst =
      if src >= l then
        if dst >= l then s else String.sub s 0 dst
      else
        match s.[src] with
        [ '_' -> remove (src + 1) dst
        |  c  -> do { s.[dst] := c; remove (src + 1) (dst + 1) } ]
    in remove 0 0
  ;

  value mkloc = Loc.to_ocaml_location;
  value mkghloc loc = Loc.to_ocaml_location (Loc.ghostify loc);

  value with_loc txt loc = Camlp4_import.Location.mkloc txt (mkloc loc);

  value mktyp loc d = {ptyp_desc = d; ptyp_loc = mkloc loc; ptyp_attributes = []};
  value mkpat loc d = {ppat_desc = d; ppat_loc = mkloc loc; ppat_attributes = []};
  value mkghpat loc d = {ppat_desc = d; ppat_loc = mkghloc loc; ppat_attributes = []};
  value mkexp loc d = {pexp_desc = d; pexp_loc = mkloc loc; pexp_attributes = []};
  value mkmty loc d = {pmty_desc = d; pmty_loc = mkloc loc; pmty_attributes = []};
  value mksig loc d = {psig_desc = d; psig_loc = mkloc loc};
  value mkmod loc d = {pmod_desc = d; pmod_loc = mkloc loc; pmod_attributes = []};
  value mkstr loc d = {pstr_desc = d; pstr_loc = mkloc loc};
  value mkcty loc d = {pcty_desc = d; pcty_loc = mkloc loc; pcty_attributes = []};
  value mkcl loc d = {pcl_desc = d; pcl_loc = mkloc loc; pcl_attributes = []};
  value mkcf loc d = { pcf_desc = d; pcf_loc = mkloc loc; pcf_attributes = []};
  value mkctf loc d = { pctf_desc = d; pctf_loc = mkloc loc; pctf_attributes = []};

  value mkpolytype t =
    match t.ptyp_desc with
    [ Ptyp_poly _ _ -> t
    | _ -> { (t) with ptyp_desc = Ptyp_poly [] t } ]
  ;

  value mkvirtual = fun
    [ <:virtual_flag< virtual >> -> Virtual
    | <:virtual_flag<>> -> Concrete
    | _ -> assert False ];

  value mkdirection = fun
    [ <:direction_flag< to >> -> Upto
    | <:direction_flag< downto >> -> Downto
    | _ -> assert False ];

  value lident s = Lident s;
  value lident_with_loc s loc = with_loc (Lident s) loc;


  value ldot l s = Ldot l s;
  value lapply l s = Lapply l s;

  value conv_con =
    let t = Hashtbl.create 73 in
    do {
      List.iter (fun (s, s') -> Hashtbl.add t s s')
        [("True", "true"); ("False", "false"); (" True", "True");
        (" False", "False")];
      fun s -> try Hashtbl.find t s with [ Not_found -> s ]
    }
  ;

  value conv_lab =
    let t = Hashtbl.create 73 in
    do {
      List.iter (fun (s, s') -> Hashtbl.add t s s') [("val", "contents")];
      fun s -> try Hashtbl.find t s with [ Not_found -> s ]
    }
  ;

  value array_function_no_loc str name =
    ldot (lident str) (if Camlp4_config.unsafe.val then "unsafe_" ^ name else name)
  ;
  value array_function loc str name = with_loc (array_function_no_loc str name) loc;
  value mkrf =
    fun
    [ <:rec_flag< rec >> -> Recursive
    | <:rec_flag<>> -> Nonrecursive
    | _ -> assert False ];

  value mkli sloc s list = with_loc (loop lident list) sloc
    where rec loop f =
      fun
      [ [i :: il] -> loop (ldot (f i)) il
      | [] -> f s ]
  ;

  value rec ctyp_fa al =
    fun
    [ TyApp _ f a -> ctyp_fa [a :: al] f
    | f -> (f, al) ]
  ;

  value ident_tag ?(conv_lid = fun x -> x) i =

    let rec self i acc =
      match i with
      [ <:ident< $lid:"*predef*"$.$lid:"option"$ >> ->
          (ldot (lident "*predef*") "option", `lident)
      | <:ident< $i1$.$i2$ >> ->
          self i2 (Some (self i1 acc))
      | <:ident< $i1$ $i2$ >> ->
          let i' = Lapply (fst (self i1 None)) (fst (self i2 None)) in
          let x =
            match acc with
            [ None -> i'
            | _ -> error (loc_of_ident i) "invalid long identifier" ]
          in (x, `app)
      | <:ident< $uid:s$ >> ->
          let x =
            match acc with
            [ None -> lident s
            | Some (acc, `uident | `app) -> ldot acc s
            | _ -> error (loc_of_ident i) "invalid long identifier" ]
          in (x, `uident)
      | <:ident< $lid:s$ >> ->
          let x =
            match acc with
            [ None -> lident (conv_lid s)
            | Some (acc, `uident | `app) -> ldot acc (conv_lid s)
            | _ -> error (loc_of_ident i) "invalid long identifier" ]
          in (x, `lident)
      | _ -> error (loc_of_ident i) "invalid long identifier" ]
    in self i None;

  value ident_noloc ?conv_lid i = fst (ident_tag ?conv_lid i);
  value ident ?conv_lid  i =
        with_loc (ident_noloc ?conv_lid i) (loc_of_ident i);

  value long_lident msg id =
    match ident_tag id with
    [ (i, `lident) -> with_loc i (loc_of_ident id)
    | _ -> error (loc_of_ident id) msg ]
  ;

  value long_type_ident = long_lident "invalid long identifier type";
  value long_class_ident = long_lident "invalid class name";

  value long_uident_noloc ?(conv_con = fun x -> x) i =
    match ident_tag i with
    [ (Ldot i s, `uident) -> ldot i (conv_con s)
    | (Lident s, `uident) -> lident (conv_con s)
    | (i, `app) -> i
    | _ -> error (loc_of_ident i) "uppercase identifier expected" ]
  ;

  value long_uident ?conv_con i =
     with_loc (long_uident_noloc ?conv_con i) (loc_of_ident i);

  value rec ctyp_long_id_prefix t =
    match t with
    [ <:ctyp< $id:i$ >> -> ident_noloc i
    | <:ctyp< $m1$ $m2$ >> ->
        let li1 = ctyp_long_id_prefix m1 in
        let li2 = ctyp_long_id_prefix m2 in
        Lapply li1 li2
    | t -> error (loc_of_ctyp t) "invalid module expression" ]
  ;

  value ctyp_long_id t =
    match t with
    [ <:ctyp< $id:i$ >> ->
        (False, long_type_ident i)
    | TyApp loc _ _ ->
        error loc "invalid type name"
    | TyCls _ i -> (True, ident i)
    | t -> error (loc_of_ctyp t) "invalid type" ]
  ;

  value rec ty_var_list_of_ctyp =
    fun
    [ <:ctyp< $t1$ $t2$ >> -> ty_var_list_of_ctyp t1 @ ty_var_list_of_ctyp t2
    | <:ctyp< '$s$ >> -> [s]
    | _ -> assert False ];

  value predef_option loc =
    TyId (loc, IdAcc (loc, IdLid (loc, "*predef*"), IdLid (loc, "option")));

  value attribute_fwd = ref (fun _ _ _ -> assert False);

  value attribute loc s str =
    !attribute_fwd loc s str;

  value rec ctyp =
    fun
    [ TyId loc i ->
        let li = long_type_ident i in
        mktyp loc (Ptyp_constr li [])
    | TyAli loc t1 t2 ->
        let (t, i) =
          match (t1, t2) with
          [ (t, TyQuo _ s) -> (t, s)
          | (TyQuo _ s, t) -> (t, s)
          | _ -> error loc "invalid alias type" ]
        in
        mktyp loc (Ptyp_alias (ctyp t) i)
    | TyAny loc -> mktyp loc Ptyp_any
    | TyApp loc _ _ as f ->
        let (f, al) = ctyp_fa [] f in
        let (is_cls, li) = ctyp_long_id f in
        if is_cls then mktyp loc (Ptyp_class li (List.map ctyp al))
        else mktyp loc (Ptyp_constr li (List.map ctyp al))
    | TyArr loc (TyLab _ lab t1) t2 ->
        mktyp loc (Ptyp_arrow lab (ctyp t1) (ctyp t2))
    | TyArr loc (TyOlb loc1 lab t1) t2 ->
        let t1 = TyApp loc1 (predef_option loc1) t1 in
        mktyp loc (Ptyp_arrow ("?" ^ lab) (ctyp t1) (ctyp t2))
    | TyArr loc t1 t2 -> mktyp loc (Ptyp_arrow "" (ctyp t1) (ctyp t2))
    | <:ctyp@loc< < $fl$ > >> -> mktyp loc (Ptyp_object (meth_list fl []) Closed)
    | <:ctyp@loc< < $fl$ .. > >> ->
        mktyp loc (Ptyp_object (meth_list fl []) Open)
    | TyCls loc id ->
        mktyp loc (Ptyp_class (ident id) [])
    | <:ctyp@loc< (module $pt$) >> ->
        let (i, cs) = package_type pt in
        mktyp loc (Ptyp_package i cs)
    | TyAtt loc s str e ->
        let e = ctyp e in
        {(e) with ptyp_attributes = e.ptyp_attributes @ [attribute loc s str]}
    | TyLab loc _ _ -> error loc "labelled type not allowed here"
    | TyMan loc _ _ -> error loc "manifest type not allowed here"
    | TyOlb loc _ _ -> error loc "labelled type not allowed here"
    | TyPol loc t1 t2 -> mktyp loc (Ptyp_poly (ty_var_list_of_ctyp t1) (ctyp t2))
    | TyQuo loc s -> mktyp loc (Ptyp_var s)
    | TyRec loc _ -> error loc "record type not allowed here"
    | TySum loc _ -> error loc "sum type not allowed here"
    | TyPrv loc _ -> error loc "private type not allowed here"
    | TyMut loc _ -> error loc "mutable type not allowed here"
    | TyOr loc _ _ -> error loc "type1 | type2 not allowed here"
    | TyAnd loc _ _ -> error loc "type1 and type2 not allowed here"
    | TyOf loc _ _ -> error loc "type1 of type2 not allowed here"
    | TyCol loc _ _ -> error loc "type1 : type2 not allowed here"
    | TySem loc _ _ -> error loc "type1 ; type2 not allowed here"
    | <:ctyp@loc< ($t1$ * $t2$) >> ->
         mktyp loc (Ptyp_tuple (List.map ctyp (list_of_ctyp t1 (list_of_ctyp t2 []))))
    | <:ctyp@loc< [ = $t$ ] >> -> mktyp loc (Ptyp_variant (row_field t) Closed None)
    | <:ctyp@loc< [ > $t$ ] >> -> mktyp loc (Ptyp_variant (row_field t) Open None)
    | <:ctyp@loc< [ < $t$ ] >> -> mktyp loc (Ptyp_variant (row_field t) Closed (Some []))
    | <:ctyp@loc< [ < $t$ > $t'$ ] >> ->
        mktyp loc (Ptyp_variant (row_field t) Closed (Some (name_tags t')))
    | TyAnt loc _ -> error loc "antiquotation not allowed here"
    | TyOfAmp _ _ _ |TyAmp _ _ _ |TySta _ _ _ |
      TyCom _ _ _ |TyVrn _ _ |TyQuM _ _ |TyQuP _ _ |TyDcl _ _ _ _ _ |
          TyAnP _ | TyAnM _ | TyTypePol _ _ _ |
      TyObj _ _ (RvAnt _) | TyNil _ | TyTup _ _ ->
        assert False ]
  and row_field = fun
    [ <:ctyp<>> -> []
    | <:ctyp< `$i$ >> -> [Rtag i True []]
    | <:ctyp< `$i$ of & $t$ >> -> [Rtag i True (List.map ctyp (list_of_ctyp t []))]
    | <:ctyp< `$i$ of $t$ >> -> [Rtag i False (List.map ctyp (list_of_ctyp t []))]
    | <:ctyp< $t1$ | $t2$ >> -> row_field t1 @ row_field t2
    | t -> [Rinherit (ctyp t)] ]
  and name_tags = fun
    [ <:ctyp< $t1$ $t2$ >> -> name_tags t1 @ name_tags t2
    | <:ctyp< `$s$ >> -> [s]
    | _ -> assert False ]
  and meth_list fl acc =
    match fl with
    [ <:ctyp<>> -> acc
    | <:ctyp< $t1$; $t2$ >> -> meth_list t1 (meth_list t2 acc)
    | <:ctyp< $lid:lab$ : $t$ >> -> [(lab, mkpolytype (ctyp t)) :: acc]
    | _ -> assert False ]

  and package_type_constraints wc acc =
    match wc with
    [ <:with_constr<>> -> acc
    | <:with_constr< type $id:id$ = $ct$ >> ->
        [(ident id, ctyp ct) :: acc]
    | <:with_constr< $wc1$ and $wc2$ >> ->
        package_type_constraints wc1 (package_type_constraints wc2 acc)
    | _ -> error (loc_of_with_constr wc) "unexpected `with constraint' for a package type" ]

  and package_type : module_type -> package_type =
    fun
    [ <:module_type< $id:i$ with $wc$ >> ->
      (long_uident i, package_type_constraints wc [])
    | <:module_type< $id:i$ >> -> (long_uident i, [])
    | mt -> error (loc_of_module_type mt) "unexpected package type" ]
  ;

  value mktype loc name tl cl tk tp tm =
    {ptype_name = name;
     ptype_params = tl; ptype_cstrs = cl; ptype_kind = tk;
     ptype_private = tp; ptype_manifest = tm; ptype_loc = mkloc loc;
     ptype_attributes = []}
  ;
  value mkprivate' m = if m then Private else Public;
  value mkprivate = fun
    [ <:private_flag< private >> -> Private
    | <:private_flag<>> -> Public
    | _ -> assert False ];
  value mktrecord =
    fun
    [ <:ctyp@loc< $id:(<:ident@sloc< $lid:s$ >>)$ : mutable $t$ >> ->
      {pld_name=with_loc s sloc;
       pld_mutable=Mutable;
       pld_type=mkpolytype (ctyp t);
       pld_loc=mkloc loc;
       pld_attributes=[];
      }
    | <:ctyp@loc< $id:(<:ident@sloc< $lid:s$ >>)$ : $t$ >> ->
      {pld_name=with_loc s sloc;
       pld_mutable=Immutable;
       pld_type=mkpolytype (ctyp t);
       pld_loc=mkloc loc;
       pld_attributes=[];
      }
    | _ -> assert False (*FIXME*) ];
  value mkvariant =
    fun
    [ <:ctyp@loc< $id:(<:ident@sloc< $uid:s$ >>)$ >> ->
      {pcd_name = with_loc (conv_con s) sloc; pcd_args = []; pcd_res = None; pcd_loc = mkloc loc; pcd_attributes = []}
    | <:ctyp@loc< $id:(<:ident@sloc< $uid:s$ >>)$ of $t$ >> ->
        {pcd_name = with_loc (conv_con s) sloc; pcd_args = List.map ctyp (list_of_ctyp t []); pcd_res = None; pcd_loc = mkloc loc; pcd_attributes = []}
    | <:ctyp@loc< $id:(<:ident@sloc< $uid:s$ >>)$ : ($t$ -> $u$) >> ->
        {pcd_name = with_loc (conv_con s) sloc; pcd_args = List.map ctyp (list_of_ctyp t []); pcd_res = Some (ctyp u); pcd_loc = mkloc loc; pcd_attributes = []}
    | <:ctyp@loc< $id:(<:ident@sloc< $uid:s$ >>)$ : $t$ >> ->
        {pcd_name = with_loc (conv_con s) sloc; pcd_args = []; pcd_res = Some (ctyp t); pcd_loc = mkloc loc; pcd_attributes = []}
    | _ -> assert False (*FIXME*) ];
  value rec type_decl name tl cl loc m pflag =
    fun
    [ <:ctyp< $t1$ == $t2$ >> ->
        type_decl name tl cl loc (Some (ctyp t1)) pflag t2
    | <:ctyp@_loc< private $t$ >> ->
        if pflag then
          error _loc "multiple private keyword used, use only one instead"
        else
          type_decl name tl cl loc m True t
    | <:ctyp< { $t$ } >> ->
        mktype loc name tl cl
          (Ptype_record (List.map mktrecord (list_of_ctyp t []))) (mkprivate' pflag) m
    | <:ctyp< [ $t$ ] >> ->
        mktype loc name tl cl
          (Ptype_variant (List.map mkvariant (list_of_ctyp t []))) (mkprivate' pflag) m
    | t ->
        if m <> None then
          error loc "only one manifest type allowed by definition" else
        let m =
          match t with
          [ <:ctyp<>> -> None
          | _ -> Some (ctyp t) ]
        in
        mktype loc name tl cl Ptype_abstract (mkprivate' pflag) m ]
  ;

  value type_decl name tl cl t loc = type_decl name tl cl loc None False t;

  value mkvalue_desc loc name t p = {pval_name = name; pval_type = ctyp t; pval_prim = p; pval_loc = mkloc loc; pval_attributes = []};

  value rec list_of_meta_list =
    fun
    [ Ast.LNil -> []
    | Ast.LCons x xs -> [x :: list_of_meta_list xs]
    | Ast.LAnt _ -> assert False ];

  value mkmutable = fun
    [ <:mutable_flag< mutable >> -> Mutable
    | <:mutable_flag<>> -> Immutable
    | _ -> assert False ];

  value paolab lab p =
    match (lab, p) with
    [ ("", <:patt< $lid:i$ >> | <:patt< ($lid:i$ : $_$) >>) -> i
    | ("", p) -> error (loc_of_patt p) "bad ast in label"
    | _ -> lab ]
  ;

  value opt_private_ctyp =
    fun
    [ <:ctyp< private $t$ >> -> (Ptype_abstract, Private, ctyp t)
    | t -> (Ptype_abstract, Public, ctyp t) ];

  value rec type_parameters t acc =
    match t with
    [ <:ctyp< $t1$ $t2$ >> -> type_parameters t1 (type_parameters t2 acc)
    | <:ctyp< +'$s$ >> -> [(s, Covariant) :: acc]
    | <:ctyp< -'$s$ >> -> [(s, Contravariant) :: acc]
    | <:ctyp< '$s$ >> -> [(s, Invariant) :: acc]
    | _ -> assert False ];

  value rec optional_type_parameters t acc =
    match t with
    [ <:ctyp< $t1$ $t2$ >> -> optional_type_parameters t1 (optional_type_parameters t2 acc)
    | <:ctyp@loc< +'$s$ >> -> [(Some (with_loc s loc), Covariant) :: acc]
    | Ast.TyAnP _loc  -> [(None, Covariant) :: acc]
    | <:ctyp@loc< -'$s$ >> -> [(Some (with_loc s loc), Contravariant) :: acc]
    | Ast.TyAnM _loc -> [(None, Contravariant) :: acc]
    | <:ctyp@loc< '$s$ >> -> [(Some (with_loc s loc), Invariant) :: acc]
    | Ast.TyAny _loc -> [(None, Invariant) :: acc]
    | _ -> assert False ];

  value rec class_parameters t acc =
    match t with
    [ <:ctyp< $t1$, $t2$ >> -> class_parameters t1 (class_parameters t2 acc)
    | <:ctyp@loc< +'$s$ >> -> [(with_loc s loc, Covariant) :: acc]
    | <:ctyp@loc< -'$s$ >> -> [(with_loc s loc, Contravariant) :: acc]
    | <:ctyp@loc< '$s$ >> -> [(with_loc s loc, Invariant) :: acc]
    | _ -> assert False ];

  value rec type_parameters_and_type_name t acc =
    match t with
    [ <:ctyp< $t1$ $t2$ >> ->
        type_parameters_and_type_name t1
          (optional_type_parameters t2 acc)
    | <:ctyp< $id:i$ >> -> (ident i, acc)
    | _ -> assert False ];

  value mkwithtyp pwith_type loc id_tpl ct =
    let (id, tpl) = type_parameters_and_type_name id_tpl [] in
    let (kind, priv, ct) = opt_private_ctyp ct in
    pwith_type id
      { ptype_name = Camlp4_import.Location.mkloc (Camlp4_import.Longident.last id.txt) id.loc;
        ptype_params = tpl; ptype_cstrs = [];
        ptype_kind = kind;
        ptype_private = priv;
        ptype_manifest = Some ct;
        ptype_loc = mkloc loc;
        ptype_attributes = [];
      };

  value rec mkwithc wc acc =
    match wc with
    [ <:with_constr<>> -> acc
    | <:with_constr@loc< type $id_tpl$ = $ct$ >> ->
        [mkwithtyp (fun lid x -> Pwith_type lid x) loc id_tpl ct :: acc]
    | <:with_constr< module $i1$ = $i2$ >> ->
        [(Pwith_module (long_uident i1) (long_uident i2)) :: acc]
    | <:with_constr@loc< type $id_tpl$ := $ct$ >> ->
        [mkwithtyp (fun _ x -> Pwith_typesubst x) loc id_tpl ct :: acc]
    | <:with_constr@loc< module $i1$ := $i2$ >> (*WcMoS _ i1 i2*) ->
        match long_uident i1 with
        [ {txt=Lident s; loc} ->
          [(Pwith_modsubst {txt=s;loc} (long_uident i2)) ::
           acc]
        | _ -> error loc "bad 'with module :=' constraint"
        ]
    | <:with_constr< $wc1$ and $wc2$ >> -> mkwithc wc1 (mkwithc wc2 acc)
    | <:with_constr@loc< $anti:_$ >> ->
         error loc "bad with constraint (antiquotation)" ];

  value rec patt_fa al =
    fun
    [ PaApp _ f a -> patt_fa [a :: al] f
    | f -> (f, al) ]
  ;

  value rec deep_mkrangepat loc c1 c2 =
    if c1 = c2 then mkghpat loc (Ppat_constant (Const_char c1))
    else
      mkghpat loc
        (Ppat_or (mkghpat loc (Ppat_constant (Const_char c1)))
          (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2))
  ;

  value rec mkrangepat loc c1 c2 =
    if c1 > c2 then mkrangepat loc c2 c1
    else if c1 = c2 then mkpat loc (Ppat_constant (Const_char c1))
    else
      mkpat loc
        (Ppat_or (mkghpat loc (Ppat_constant (Const_char c1)))
          (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2))
  ;

  value rec patt =
    fun
    [ <:patt@loc< $id:(<:ident@sloc< $lid:s$ >>)$ >> ->
      mkpat loc (Ppat_var (with_loc s sloc))
    | <:patt@loc< $id:i$ >> ->
        let p = Ppat_construct (long_uident ~conv_con i) None
        in mkpat loc p
    | PaAli loc p1 p2 ->
        let (p, i) =
          match (p1, p2) with
          [ (p, <:patt< $id:(<:ident@sloc< $lid:s$ >>)$ >>) -> (p, with_loc s sloc)
          | (<:patt< $id:(<:ident@sloc< $lid:s$ >>)$ >>, p) -> (p, with_loc s sloc)
          | _ -> error loc "invalid alias pattern" ]
        in
        mkpat loc (Ppat_alias (patt p) i)
    | PaAnt loc _ -> error loc "antiquotation not allowed here"
    | PaAny loc -> mkpat loc Ppat_any
    | <:patt@loc< $id:(<:ident@sloc< $uid:s$ >>)$ ($tup:<:patt@loc_any< _ >>$) >> ->
        mkpat loc (Ppat_construct (lident_with_loc (conv_con s) sloc)
              (Some (mkpat loc_any Ppat_any)))
    | PaApp loc _ _ as f ->
        let (f, al) = patt_fa [] f in
        let al = List.map patt al in
        match (patt f).ppat_desc with
        [ Ppat_construct li None ->
              let a =
                match al with
                [ [a] -> a
                | _ -> mkpat loc (Ppat_tuple al) ]
              in
              mkpat loc (Ppat_construct li (Some a))
        | Ppat_variant s None ->
            let a =
                match al with
                [ [a] -> a
                | _ -> mkpat loc (Ppat_tuple al) ]
            in mkpat loc (Ppat_variant s (Some a))
        | _ ->
            error (loc_of_patt f)
              "this is not a constructor, it cannot be applied in a pattern" ]
    | PaArr loc p -> mkpat loc (Ppat_array (List.map patt (list_of_patt p [])))
    | PaChr loc s ->
        mkpat loc (Ppat_constant (Const_char (char_of_char_token loc s)))
    | PaInt loc s ->
        let i = try int_of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int"
        ] in mkpat loc (Ppat_constant (Const_int i))
    | PaInt32 loc s ->
        let i32 = try Int32.of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int32"
        ] in mkpat loc (Ppat_constant (Const_int32 i32))
    | PaInt64 loc s ->
        let i64 = try Int64.of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int64"
        ] in mkpat loc (Ppat_constant (Const_int64 i64))
    | PaNativeInt loc s ->
        let nati = try Nativeint.of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type nativeint"
        ] in mkpat loc (Ppat_constant (Const_nativeint nati))
    | PaFlo loc s -> mkpat loc (Ppat_constant (Const_float (remove_underscores s)))
    | PaLab loc _ _ -> error loc "labeled pattern not allowed here"
    | PaOlb loc _ _ | PaOlbi loc _ _ _ -> error loc "labeled pattern not allowed here"
    | PaOrp loc p1 p2 -> mkpat loc (Ppat_or (patt p1) (patt p2))
    | PaRng loc p1 p2 ->
        match (p1, p2) with
        [ (PaChr loc1 c1, PaChr loc2 c2) ->
            let c1 = char_of_char_token loc1 c1 in
            let c2 = char_of_char_token loc2 c2 in
            mkrangepat loc c1 c2
        | _ -> error loc "range pattern allowed only for characters" ]
    | PaRec loc p ->
        let ps = list_of_patt p [] in
        let is_wildcard = fun [ <:patt< _ >> -> True | _ -> False ] in
        let (wildcards,ps) = List.partition is_wildcard ps in
        let is_closed = if wildcards = [] then Closed else Open in
        mkpat loc (Ppat_record (List.map mklabpat ps, is_closed))
    | PaStr loc s ->
        mkpat loc (Ppat_constant (Const_string (string_of_string_token loc s) None))
    | <:patt@loc< ($p1$, $p2$) >> ->
         mkpat loc (Ppat_tuple
           (List.map patt (list_of_patt p1 (list_of_patt p2 []))))
    | <:patt@loc< ($tup:_$) >> -> error loc "singleton tuple pattern"
    | PaTyc loc p t -> mkpat loc (Ppat_constraint (patt p) (ctyp t))
    | PaTyp loc i -> mkpat loc (Ppat_type (long_type_ident i))
    | PaVrn loc s -> mkpat loc (Ppat_variant (conv_con s) None)
    | PaLaz loc p -> mkpat loc (Ppat_lazy (patt p))
    | PaMod loc m -> mkpat loc (Ppat_unpack (with_loc m loc))
    | PaAtt loc s str e ->
        let e = patt e in
        {(e) with ppat_attributes = e.ppat_attributes @ [attribute loc s str]}
    | PaEq _ _ _ | PaSem _ _ _ | PaCom _ _ _ | PaNil _ as p ->
        error (loc_of_patt p) "invalid pattern" ]
  and mklabpat =
    fun
    [ <:patt< $i$ = $p$ >> -> (ident ~conv_lid:conv_lab i, patt p)
    | p -> error (loc_of_patt p) "invalid pattern" ];

  value rec expr_fa al =
    fun
    [ ExApp _ f a -> expr_fa [a :: al] f
    | f -> (f, al) ]
  ;

  value rec class_expr_fa al =
    fun
    [ CeApp _ ce a -> class_expr_fa [a :: al] ce
    | ce -> (ce, al) ]
  ;


  value rec sep_expr_acc l =
    fun
    [ ExAcc _ e1 e2 -> sep_expr_acc (sep_expr_acc l e2) e1
    | <:expr@loc< $uid:s$ >> as e ->
        match l with
        [ [] -> [(loc, [], e)]
        | [(loc', sl, e) :: l] -> [(Loc.merge loc loc', [s :: sl], e) :: l] ]
    | <:expr< $id:(<:ident< $_$.$_$ >> as i)$ >> ->
        let rec normalize_acc =
          fun
          [ <:ident@_loc< $i1$.$i2$ >> ->
            <:expr< $normalize_acc i1$.$normalize_acc i2$ >>
          | <:ident@_loc< $i1$ $i2$ >> ->
            <:expr< $normalize_acc i1$ $normalize_acc i2$ >>
          | <:ident@_loc< $anti:_$ >> | <:ident@_loc< $uid:_$ >> |
            <:ident@_loc< $lid:_$ >> as i -> <:expr< $id:i$ >> ]
        in sep_expr_acc l (normalize_acc i)
    | e -> [(loc_of_expr e, [], e) :: l] ]
  ;

  value override_flag loc =
    fun [ <:override_flag< ! >> -> Override
        | <:override_flag<>> -> Fresh
        |  _ -> error loc "antiquotation not allowed here"
        ];

  value list_of_opt_ctyp ot acc =
    match ot with
    [ <:ctyp<>> -> acc
    | t -> list_of_ctyp t acc ];

value varify_constructors var_names =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
          [
       Ptyp_any -> Ptyp_any
      | Ptyp_var x -> Ptyp_var x
      | Ptyp_arrow label core_type core_type' ->
          Ptyp_arrow label (loop core_type) (loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr ({ txt = Lident s }) [] when List.mem s var_names ->
          Ptyp_var ("&" ^ s)
      | Ptyp_constr longident lst ->
          Ptyp_constr longident (List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object (List.map (fun (s, t) -> (s, loop t)) lst, o)
      | Ptyp_class longident lst ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias core_type string ->
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant row_field_list flag lbl_lst_option ->
          Ptyp_variant(List.map loop_row_field row_field_list, flag, lbl_lst_option)
      | Ptyp_poly string_lst core_type ->
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package longident lst ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension x ->
          Ptyp_extension x
]
    in
    {(t) with ptyp_desc = desc}
  and loop_row_field x  =
    match x with
      [ Rtag(label,flag,lst) ->
          Rtag(label,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t) ]
  in
  loop;



  value rec expr =
    fun
    [ <:expr@loc< $x$.val >> ->
        mkexp loc
          (Pexp_apply (mkexp loc (Pexp_ident (lident_with_loc "!" loc))) [("", expr x)])
    | ExAcc loc _ _ | <:expr@loc< $id:<:ident< $_$ . $_$ >>$ >> as e ->
        let (e, l) =
          match sep_expr_acc [] e with
          [ [(loc, ml, <:expr@sloc< $uid:s$ >>) :: l] ->
              (mkexp loc (Pexp_construct (mkli sloc (conv_con s) ml) None), l)
          | [(loc, ml, <:expr@sloc< $lid:s$ >>) :: l] ->
              (mkexp loc (Pexp_ident (mkli sloc s ml)), l)
          | [(_, [], e) :: l] -> (expr e, l)
          | _ -> error loc "bad ast in expression" ]
        in
        let (_, e) =
          List.fold_left
            (fun (loc_bp, e1) (loc_ep, ml, e2) ->
              match e2 with
              [ <:expr@sloc< $lid:s$ >> ->
                  let loc = Loc.merge loc_bp loc_ep
                  in  (loc, mkexp loc (Pexp_field e1 (mkli sloc (conv_lab s) ml)))
              | _ -> error (loc_of_expr e2) "lowercase identifier expected" ])
            (loc, e) l
        in
        e
    | ExAnt loc _ -> error loc "antiquotation not allowed here"
    | ExApp loc _ _ as f ->
        let (f, al) = expr_fa [] f in
        let al = List.map label_expr al in
        match (expr f).pexp_desc with
        [ Pexp_construct li None ->
            let al = List.map snd al in
              let a =
                match al with
                [ [a] -> a
                | _ -> mkexp loc (Pexp_tuple al) ]
              in
              mkexp loc (Pexp_construct li (Some a))
        | Pexp_variant s None ->
            let al = List.map snd al in
            let a =
                match al with
                [ [a] -> a
                | _ -> mkexp loc (Pexp_tuple al) ]
            in mkexp loc (Pexp_variant s (Some a))
        | _ -> mkexp loc (Pexp_apply (expr f) al) ]
    | ExAre loc e1 e2 ->
        mkexp loc
          (Pexp_apply (mkexp loc (Pexp_ident (array_function loc "Array" "get")))
            [("", expr e1); ("", expr e2)])
    | ExArr loc e -> mkexp loc (Pexp_array (List.map expr (list_of_expr e [])))
    | ExAsf loc -> 
        mkexp loc (Pexp_assert (mkexp loc (Pexp_construct {txt=Lident "false"; loc=mkloc loc} None)))
    | ExAss loc e v ->
        let e =
          match e with
          [ <:expr@loc< $x$.val >> ->
              Pexp_apply (mkexp loc (Pexp_ident (lident_with_loc ":=" loc)))
                [("", expr x); ("", expr v)]
          | ExAcc loc _ _ ->
              match (expr e).pexp_desc with
              [ Pexp_field e lab -> Pexp_setfield e lab (expr v)
              | _ -> error loc "bad record access" ]
          | ExAre loc e1 e2 ->
              Pexp_apply (mkexp loc (Pexp_ident (array_function loc "Array" "set")))
                [("", expr e1); ("", expr e2); ("", expr v)]
          | <:expr< $id:(<:ident@lloc< $lid:lab$ >>)$ >> -> Pexp_setinstvar (with_loc lab lloc) (expr v)
          | ExSte loc e1 e2 ->
              Pexp_apply
                (mkexp loc (Pexp_ident (array_function loc "String" "set")))
                [("", expr e1); ("", expr e2); ("", expr v)]
          | _ -> error loc "bad left part of assignment" ]
        in
        mkexp loc e
    | ExAsr loc e -> mkexp loc (Pexp_assert (expr e))
    | ExChr loc s ->
        mkexp loc (Pexp_constant (Const_char (char_of_char_token loc s)))
    | ExCoe loc e t1 t2 ->
        let t1 =
          match t1 with
          [ <:ctyp<>> -> None
          | t -> Some (ctyp t) ] in
        mkexp loc (Pexp_coerce (expr e) t1 (ctyp t2))
    | ExFlo loc s -> mkexp loc (Pexp_constant (Const_float (remove_underscores s)))
    | ExFor loc i e1 e2 df el ->
        let e3 = ExSeq loc el in
        mkexp loc (Pexp_for (with_loc i loc) (expr e1) (expr e2) (mkdirection df) (expr e3))
    | <:expr@loc< fun [ $PaLab _ lab po$ when $w$ -> $e$ ] >> ->
        mkfun loc lab None (patt_of_lab loc lab po) e w
    | <:expr@loc< fun [ $PaOlbi _ lab p e1$ when $w$ -> $e2$ ] >> ->
        let lab = paolab lab p in
        mkfun loc ("?" ^ lab) (Some (expr e1)) (patt p) e2 w
    | <:expr@loc< fun [ $PaOlb _ lab p$ when $w$ -> $e$ ] >> ->
        let lab = paolab lab p in
        mkfun loc ("?" ^ lab) None (patt_of_lab loc lab p) e w
    | ExFun loc a -> mkexp loc (Pexp_function (match_case a []))
    | ExIfe loc e1 e2 e3 ->
        mkexp loc (Pexp_ifthenelse (expr e1) (expr e2) (Some (expr e3)))
    | ExInt loc s ->
        let i = try int_of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int"
        ] in mkexp loc (Pexp_constant (Const_int i))
    | ExInt32 loc s ->
        let i32 = try Int32.of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int32"
        ] in mkexp loc (Pexp_constant (Const_int32 i32))
    | ExInt64 loc s ->
        let i64 = try Int64.of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int64"
        ] in mkexp loc (Pexp_constant (Const_int64 i64))
    | ExNativeInt loc s ->
        let nati = try Nativeint.of_string s with [
          Failure _ -> error loc "Integer literal exceeds the range of representable integers of type nativeint"
        ] in mkexp loc (Pexp_constant (Const_nativeint nati))
    | ExLab loc _ _ -> error loc "labeled expression not allowed here"
    | ExLaz loc e -> mkexp loc (Pexp_lazy (expr e))
    | ExLet loc rf bi e ->
        mkexp loc (Pexp_let (mkrf rf) (binding bi []) (expr e))
    | ExLmd loc i me e -> mkexp loc (Pexp_letmodule (with_loc i loc) (module_expr me) (expr e))
    | ExMat loc e a -> mkexp loc (Pexp_match (expr e) (match_case a []))
    | ExNew loc id -> mkexp loc (Pexp_new (long_type_ident id))
    | ExObj loc po cfl ->
        let p =
          match po with
          [ <:patt<>> -> <:patt@loc< _ >>
          | p -> p ]
        in
        let cil = class_str_item cfl [] in
        mkexp loc (Pexp_object { pcstr_self = patt p; pcstr_fields = cil })
    | ExOlb loc _ _ -> error loc "labeled expression not allowed here"
    | ExOvr loc iel -> mkexp loc (Pexp_override (mkideexp iel []))
    | ExRec loc lel eo ->
        match lel with
        [ <:rec_binding<>> -> error loc "empty record"
        | _ ->
          let eo =
            match eo with
            [ <:expr<>> -> None
            | e -> Some (expr e) ] in
          mkexp loc (Pexp_record (mklabexp lel []) eo) ]
    | ExSeq _loc e ->
        let rec loop =
          fun
          [ [] -> expr <:expr< () >>
          | [e] -> expr e
          | [e :: el] ->
              let _loc = Loc.merge (loc_of_expr e) _loc in
              mkexp _loc (Pexp_sequence (expr e) (loop el)) ]
        in
        loop (list_of_expr e [])
    | ExSnd loc e s -> mkexp loc (Pexp_send (expr e) s)
    | ExSte loc e1 e2 ->
        mkexp loc
          (Pexp_apply (mkexp loc (Pexp_ident (array_function loc "String" "get")))
            [("", expr e1); ("", expr e2)])
    | ExStr loc s ->
        mkexp loc (Pexp_constant (Const_string (string_of_string_token loc s) None))
    | ExTry loc e a -> mkexp loc (Pexp_try (expr e) (match_case a []))
    | <:expr@loc< ($e1$, $e2$) >> ->
         mkexp loc (Pexp_tuple (List.map expr (list_of_expr e1 (list_of_expr e2 []))))
    | <:expr@loc< ($tup:_$) >> -> error loc "singleton tuple"
    | ExTyc loc e t -> mkexp loc (Pexp_constraint (expr e) (ctyp t))
    | <:expr@loc< () >> ->
        mkexp loc (Pexp_construct (lident_with_loc "()" loc) None)
    | <:expr@loc< $lid:s$ >> ->
        mkexp loc (Pexp_ident (lident_with_loc s loc))
    | <:expr@loc< $uid:s$ >> ->
        mkexp loc (Pexp_construct (lident_with_loc (conv_con s) loc) None)
    | ExVrn loc s -> mkexp loc (Pexp_variant (conv_con s) None)
    | ExWhi loc e1 el ->
        let e2 = ExSeq loc el in
        mkexp loc (Pexp_while (expr e1) (expr e2))
    | ExOpI loc i ov e ->
        let fresh = override_flag loc ov in 
        mkexp loc (Pexp_open fresh (long_uident i) (expr e))
    | <:expr@loc< (module $me$ : $pt$) >> ->
        mkexp loc (Pexp_constraint (mkexp loc (Pexp_pack (module_expr me)),
                    mktyp loc (Ptyp_package (package_type pt))))
    | <:expr@loc< (module $me$) >> ->
        mkexp loc (Pexp_pack (module_expr me))
    | ExFUN loc i e ->
        mkexp loc (Pexp_newtype i (expr e))
    | <:expr@loc< $_$,$_$ >> -> error loc "expr, expr: not allowed here"
    | <:expr@loc< $_$;$_$ >> ->
        error loc "expr; expr: not allowed here, use do {...} or [|...|] to surround them"
    | ExAtt loc s str e ->
        let e = expr e in
        {(e) with pexp_attributes = e.pexp_attributes @ [attribute loc s str]}
    | ExId _ _ | ExNil _ as e -> error (loc_of_expr e) "invalid expr" ]
  and patt_of_lab _loc lab =
    fun
    [ <:patt<>> -> patt <:patt< $lid:lab$ >>
    | p -> patt p ]
  and expr_of_lab _loc lab =
    fun
    [ <:expr<>> -> expr <:expr< $lid:lab$ >>
    | e -> expr e ]
  and label_expr =
    fun
    [ ExLab loc lab eo -> (lab, expr_of_lab loc lab eo)
    | ExOlb loc lab eo -> ("?" ^ lab, expr_of_lab loc lab eo)
    | e -> ("", expr e) ]
  and binding x acc =
    match x with
    [ <:binding< $x$ and $y$ >> ->
         binding x (binding y acc)
    | <:binding@_loc< $pat:( <:patt@sloc< $lid:bind_name$ >> )$ = ($e$ : $TyTypePol _ vs ty$) >> ->
      (* this code is not pretty because it is temporary *)
      let rec id_to_string x =
        match x with
            [ <:ctyp< $lid:x$ >> -> [x]
            | <:ctyp< $x$ $y$ >> -> (id_to_string x) @ (id_to_string y)
            | _ -> assert False]
      in
      let vars = id_to_string vs in
      let ampersand_vars = List.map (fun x -> "&" ^ x) vars in
      let ty' = varify_constructors vars (ctyp ty) in
      let mkexp = mkexp _loc in
      let mkpat = mkpat _loc in
      let e = mkexp (Pexp_constraint (expr e) (ctyp ty)) in
      let rec mk_newtypes x =
        match x with
          [ [newtype :: []] -> mkexp (Pexp_newtype(newtype, e))
          | [newtype :: newtypes] ->
            mkexp(Pexp_newtype (newtype,mk_newtypes newtypes))
          | [] -> assert False]
      in
      let pat =
        mkpat (Ppat_constraint (mkpat (Ppat_var (with_loc bind_name sloc)),
                                mktyp _loc (Ptyp_poly ampersand_vars ty')))
      in
      let e = mk_newtypes vars in
      [{pvb_pat=pat; pvb_expr=e; pvb_attributes=[]} :: acc]
    | <:binding@_loc< $p$ = ($e$ : ! $vs$ . $ty$) >> ->
        [{pvb_pat=patt <:patt< ($p$ : ! $vs$ . $ty$ ) >>;
          pvb_expr=expr e;
          pvb_attributes=[]} :: acc]
    | <:binding< $p$ = $e$ >> -> [{pvb_pat=patt p; pvb_expr=expr e; pvb_attributes=[]} :: acc]
    | <:binding<>> -> acc
    | _ -> assert False ]
  and match_case x acc =
    match x with
    [ <:match_case< $x$ | $y$ >> -> match_case x (match_case y acc)
    | <:match_case< $pat:p$ when $w$ -> $e$ >> ->
        [when_expr (patt p) e w :: acc]
    | <:match_case<>> -> acc
    | _ -> assert False ]
  and when_expr p e w =
    let g = match w with
    [ <:expr<>> -> None
    | g -> Some (expr g) ]
    in
    {pc_lhs = p; pc_guard = g; pc_rhs = expr e}
  and mkfun loc lab def p e w =
     let () =
       match w with
         [ <:expr<>> -> ()
       | _ -> assert False ]
     in
     mkexp loc (Pexp_fun lab def p (expr e))
  and mklabexp x acc =
    match x with
    [ <:rec_binding< $x$; $y$ >> ->
         mklabexp x (mklabexp y acc)
    | <:rec_binding< $i$ = $e$ >> -> [(ident ~conv_lid:conv_lab i, expr e) :: acc]
    | _ -> assert False ]
  and mkideexp x acc =
    match x with
    [ <:rec_binding<>> -> acc
    | <:rec_binding< $x$; $y$ >> ->
         mkideexp x (mkideexp y acc)
    | <:rec_binding< $id:( <:ident@sloc< $lid:s$ >>)$ = $e$ >> -> [(with_loc s sloc, expr e) :: acc]
    | _ -> assert False ]
  and mktype_decl x acc =
    match x with
    [ <:ctyp< $x$ and $y$ >> ->
         mktype_decl x (mktype_decl y acc)
    | Ast.TyDcl cloc c tl td cl ->
        let cl =
          List.map
            (fun (t1, t2) ->
              let loc = Loc.merge (loc_of_ctyp t1) (loc_of_ctyp t2) in
              (ctyp t1, ctyp t2, mkloc loc))
            cl
        in
        [type_decl (with_loc c cloc) (List.fold_right optional_type_parameters tl []) cl td cloc :: acc]
    | _ -> assert False ]
  and module_type =
    fun
    [ <:module_type@loc<>> -> error loc "abstract/nil module type not allowed here"
    | <:module_type@loc< $id:i$ >> -> mkmty loc (Pmty_ident (long_uident i))
    | <:module_type@loc< functor ($n$ : $nt$) -> $mt$ >> ->
        mkmty loc (Pmty_functor (with_loc n loc) (module_type nt) (module_type mt))
    | <:module_type@loc< '$_$ >> -> error loc "module type variable not allowed here"
    | <:module_type@loc< sig $sl$ end >> ->
        mkmty loc (Pmty_signature (sig_item sl []))
    | <:module_type@loc< $mt$ with $wc$ >> ->
        mkmty loc (Pmty_with (module_type mt) (mkwithc wc []))
    | <:module_type@loc< module type of $me$ >> ->
        mkmty loc (Pmty_typeof (module_expr me))
    | MtAtt loc s str e ->
        let e = module_type e in
        {(e) with pmty_attributes = e.pmty_attributes @ [attribute loc s str]}
    | <:module_type< $anti:_$ >> -> assert False ]
  and sig_item s l =
    match s with
    [ <:sig_item<>> -> l
    | SgCls loc cd ->
        [mksig loc (Psig_class
           (List.map class_info_class_type (list_of_class_type cd []))) :: l]
    | SgClt loc ctd ->
        [mksig loc (Psig_class_type
           (List.map class_info_class_type (list_of_class_type ctd []))) :: l]
    | <:sig_item< $sg1$; $sg2$ >> -> sig_item sg1 (sig_item sg2 l)
    | SgDir _ _ _ -> l
    | <:sig_item@loc< exception $uid:s$ >> ->
        [mksig loc (Psig_exception {pcd_name=with_loc (conv_con s) loc; pcd_args=[]; pcd_attributes=[]; pcd_res=None; pcd_loc=mkloc loc}) :: l]
    | <:sig_item@loc< exception $uid:s$ of $t$ >> ->
        [mksig loc (Psig_exception {pcd_name=with_loc (conv_con s) loc; pcd_args=List.map ctyp (list_of_ctyp t []); pcd_attributes=[]; pcd_res=None; pcd_loc=mkloc loc}) :: l]
    | SgExc _ _ -> assert False (*FIXME*)
    | SgExt loc n t sl -> [mksig loc (Psig_value (mkvalue_desc loc (with_loc n loc) t (list_of_meta_list sl))) :: l]
    | SgInc loc mt -> [mksig loc (Psig_include (module_type mt) []) :: l]
    | SgMod loc n mt -> [mksig loc (Psig_module {pmd_name=with_loc n loc; pmd_type=module_type mt; pmd_attributes=[]}) :: l]
    | SgRecMod loc mb ->
        [mksig loc (Psig_recmodule (module_sig_binding mb [])) :: l]
    | SgMty loc n mt ->
        let si =
          match mt with
          [ MtQuo _ _ -> None
          | _ -> Some (module_type mt) ]
        in
        [mksig loc (Psig_modtype {pmtd_name=with_loc n loc; pmtd_type=si; pmtd_attributes=[]}) :: l]
    | SgOpn loc id ->
        [mksig loc (Psig_open Fresh (long_uident id) []) :: l]
    | SgTyp loc tdl -> [mksig loc (Psig_type (mktype_decl tdl [])) :: l]
    | SgVal loc n t -> [mksig loc (Psig_value (mkvalue_desc loc (with_loc n loc) t [])) :: l]
    | <:sig_item@loc< $anti:_$ >> -> error loc "antiquotation in sig_item" ]
  and module_sig_binding x acc =
    match x with
    [ <:module_binding< $x$ and $y$ >> ->
        module_sig_binding x (module_sig_binding y acc)
    | <:module_binding@loc< $s$ : $mt$ >> ->
        [{pmd_name=with_loc s loc; pmd_type=module_type mt; pmd_attributes=[]} :: acc]
    | _ -> assert False ]
  and module_str_binding x acc =
    match x with
    [ <:module_binding< $x$ and $y$ >> ->
        module_str_binding x (module_str_binding y acc)
    | <:module_binding@loc< $s$ : $mt$ = $me$ >> ->
        [{pmb_name=with_loc s loc;
          pmb_expr=
          {pmod_loc=Camlp4_import.Location.none;
           pmod_desc=Pmod_constraint(module_expr me,module_type mt);
           pmod_attributes=[];
          };
          pmb_attributes=[]} :: acc]
    | _ -> assert False ]
  and module_expr =
    fun
    [ <:module_expr@loc<>> -> error loc "nil module expression"
    | <:module_expr@loc< $id:i$ >> -> mkmod loc (Pmod_ident (long_uident i))
    | <:module_expr@loc< $me1$ $me2$ >> ->
        mkmod loc (Pmod_apply (module_expr me1) (module_expr me2))
    | <:module_expr@loc< functor ($n$ : $mt$) -> $me$ >> ->
        mkmod loc (Pmod_functor (with_loc n loc) (module_type mt) (module_expr me))
    | <:module_expr@loc< struct $sl$ end >> ->
        mkmod loc (Pmod_structure (str_item sl []))
    | <:module_expr@loc< ($me$ : $mt$) >> ->
        mkmod loc (Pmod_constraint (module_expr me) (module_type mt))
    | <:module_expr@loc< (value $e$ : $pt$) >> ->
        mkmod loc (Pmod_unpack (
                   mkexp loc (Pexp_constraint (expr e,
                              mktyp loc (Ptyp_package (package_type pt))))))
    | <:module_expr@loc< (value $e$) >> ->
        mkmod loc (Pmod_unpack (expr e))
    | MeAtt loc s str e ->
        let e = module_expr e in
        {(e) with pmod_attributes = e.pmod_attributes @ [attribute loc s str]}
    | <:module_expr@loc< $anti:_$ >> -> error loc "antiquotation in module_expr" ]
  and str_item s l =
    match s with
    [ <:str_item<>> -> l
    | StCls loc cd ->
        [mkstr loc (Pstr_class
           (List.map class_info_class_expr (list_of_class_expr cd []))) :: l]
    | StClt loc ctd ->
        [mkstr loc (Pstr_class_type
           (List.map class_info_class_type (list_of_class_type ctd []))) :: l]
    | <:str_item< $st1$; $st2$ >> -> str_item st1 (str_item st2 l)
    | StDir _ _ _ -> l
    | <:str_item@loc< exception $uid:s$ >> ->
        [mkstr loc (Pstr_exception {pcd_name=with_loc (conv_con s) loc; pcd_args=[]; pcd_attributes=[]; pcd_res=None; pcd_loc=mkloc loc}) :: l ]
    | <:str_item@loc< exception $uid:s$ of $t$ >> ->
        [mkstr loc (Pstr_exception {pcd_name=with_loc (conv_con s) loc; pcd_args=List.map ctyp (list_of_ctyp t []);pcd_attributes=[]; pcd_res=None; pcd_loc=mkloc loc}) :: l ]
    | <:str_item@loc< exception $uid:s$ = $i$ >> ->
        [mkstr loc (Pstr_exn_rebind (with_loc (conv_con s) loc) (long_uident ~conv_con i) []) :: l ]
    | <:str_item@loc< exception $uid:_$ of $_$ = $_$ >> ->
        error loc "type in exception alias"
    | StExc _ _ _ -> assert False (*FIXME*)
    | StExp loc e -> [mkstr loc (Pstr_eval (expr e) []) :: l]
    | StExt loc n t sl -> [mkstr loc (Pstr_primitive (mkvalue_desc loc (with_loc n loc) t (list_of_meta_list sl))) :: l]
    | StInc loc me -> [mkstr loc (Pstr_include (module_expr me, [])) :: l]
    | StMod loc n me -> [mkstr loc (Pstr_module {pmb_name=with_loc n loc;pmb_expr=module_expr me;pmb_attributes=[]}) :: l]
    | StRecMod loc mb ->
        [mkstr loc (Pstr_recmodule (module_str_binding mb [])) :: l]
    | StMty loc n mt ->
        let si =
          match mt with
          [ MtQuo _ _ -> None
          | _ -> Some (module_type mt) ]
        in
        [mkstr loc (Pstr_modtype {pmtd_name=with_loc n loc; pmtd_type=si; pmtd_attributes=[]}) :: l]
    | StOpn loc ov id ->
        let fresh = override_flag loc ov in
        [mkstr loc (Pstr_open fresh (long_uident id) []) :: l]
    | StTyp loc tdl -> [mkstr loc (Pstr_type (mktype_decl tdl [])) :: l]
    | StVal loc rf bi ->
        [mkstr loc (Pstr_value (mkrf rf) (binding bi [])) :: l]
    | <:str_item@loc< $anti:_$ >> -> error loc "antiquotation in str_item" ]
  and class_type =
    fun
    [ CtCon loc ViNil id tl ->
        mkcty loc
          (Pcty_constr (long_class_ident id) (List.map ctyp (list_of_opt_ctyp tl [])))
    | CtFun loc (TyLab _ lab t) ct ->
        mkcty loc (Pcty_arrow lab (ctyp t) (class_type ct))
    | CtFun loc (TyOlb loc1 lab t) ct ->
        let t = TyApp loc1 (predef_option loc1) t in
        mkcty loc (Pcty_arrow ("?" ^ lab) (ctyp t) (class_type ct))
    | CtFun loc t ct -> mkcty loc (Pcty_arrow "" (ctyp t) (class_type ct))
    | CtSig loc t_o ctfl ->
        let t =
          match t_o with
          [ <:ctyp<>> -> <:ctyp@loc< _ >>
          | t -> t ]
        in
        let cil = class_sig_item ctfl [] in
        mkcty loc (Pcty_signature {
          pcsig_self = ctyp t;
          pcsig_fields = cil;
        })
    | CtAtt loc s str e ->
        let e = class_type e in
        {(e) with pcty_attributes = e.pcty_attributes @ [attribute loc s str]}
    | CtCon loc _ _ _ ->
        error loc "invalid virtual class inside a class type"
    | CtAnt _ _ | CtEq _ _ _ | CtCol _ _ _ | CtAnd _ _ _ | CtNil _ ->
        assert False ]

  and class_info_class_expr ci =
    match ci with
    [ CeEq _ (CeCon loc vir (IdLid nloc name) params) ce ->
      let params =
        match params with
        [ <:ctyp<>> -> []
        | t -> class_parameters t [] ]
      in
      {pci_virt = mkvirtual vir;
       pci_params = params;
       pci_name = with_loc name nloc;
       pci_expr = class_expr ce;
       pci_loc = mkloc loc;
       pci_attributes = []
      }
    | ce -> error (loc_of_class_expr ce) "bad class definition" ]
  and class_info_class_type ci =
    match ci with
    [ CtEq _ (CtCon loc vir (IdLid nloc name) params) ct |
      CtCol _ (CtCon loc vir (IdLid nloc name) params) ct ->
      let params =
        match params with
        [ <:ctyp<>> -> []
        | t -> class_parameters t [] ]
      in
      {pci_virt = mkvirtual vir;
       pci_params = params;
       pci_name = with_loc name nloc;
       pci_expr = class_type ct;
       pci_attributes = [];
       pci_loc = mkloc loc
      }
    | ct -> error (loc_of_class_type ct)
              "bad class/class type declaration/definition" ]
  and class_sig_item c l =
    match c with
    [ <:class_sig_item<>> -> l
    | CgCtr loc t1 t2 -> [mkctf loc (Pctf_constraint (ctyp t1, ctyp t2)) :: l]
    | <:class_sig_item< $csg1$; $csg2$ >> ->
        class_sig_item csg1 (class_sig_item csg2 l)
    | CgInh loc ct -> [mkctf loc (Pctf_inherit (class_type ct)) :: l]
    | CgMth loc s pf t ->
        [mkctf loc (Pctf_method (s, mkprivate pf, Concrete, mkpolytype (ctyp t))) :: l]
    | CgVal loc s b v t ->
        [mkctf loc (Pctf_val (s, mkmutable b, mkvirtual v, ctyp t)) :: l]
    | CgVir loc s b t ->
        [mkctf loc (Pctf_method (s, mkprivate b, Virtual, mkpolytype (ctyp t))) :: l]
    | CgAnt _ _ -> assert False ]
  and class_expr =
    fun
    [ CeApp loc _ _ as c ->
        let (ce, el) = class_expr_fa [] c in
        let el = List.map label_expr el in
        mkcl loc (Pcl_apply (class_expr ce) el)
    | CeCon loc ViNil id tl ->
        mkcl loc
          (Pcl_constr (long_class_ident id) (List.map ctyp (list_of_opt_ctyp tl [])))
    | CeFun loc (PaLab _ lab po) ce ->
        mkcl loc
          (Pcl_fun lab None (patt_of_lab loc lab po) (class_expr ce))
    | CeFun loc (PaOlbi _ lab p e) ce ->
        let lab = paolab lab p in
        mkcl loc (Pcl_fun ("?" ^ lab) (Some (expr e)) (patt p) (class_expr ce))
    | CeFun loc (PaOlb _ lab p) ce ->
        let lab = paolab lab p in
        mkcl loc
          (Pcl_fun ("?" ^ lab) None (patt_of_lab loc lab p) (class_expr ce))
    | CeFun loc p ce -> mkcl loc (Pcl_fun "" None (patt p) (class_expr ce))
    | CeLet loc rf bi ce ->
        mkcl loc (Pcl_let (mkrf rf) (binding bi []) (class_expr ce))
    | CeStr loc po cfl ->
        let p =
          match po with
          [ <:patt<>> -> <:patt@loc< _ >>
          | p -> p ]
        in
        let cil = class_str_item cfl [] in
        mkcl loc (Pcl_structure {
          pcstr_self = patt p;
          pcstr_fields = cil;
        })
    | CeTyc loc ce ct ->
        mkcl loc (Pcl_constraint (class_expr ce) (class_type ct))
    | CeAtt loc s str e ->
        let e = class_expr e in
        {(e) with pcl_attributes = e.pcl_attributes @ [attribute loc s str]}
    | CeCon loc _ _ _ ->
        error loc "invalid virtual class inside a class expression"
    | CeAnt _ _ | CeEq _ _ _ | CeAnd _ _ _ | CeNil _ -> assert False ]
  and class_str_item c l =
    match c with
    [ CrNil _ -> l
    | CrCtr loc t1 t2 -> [mkcf loc (Pcf_constraint (ctyp t1, ctyp t2)) :: l]
    | <:class_str_item< $cst1$; $cst2$ >> ->
        class_str_item cst1 (class_str_item cst2 l)
    | CrInh loc ov ce pb ->
        let opb = if pb = "" then None else Some pb in
        [mkcf loc (Pcf_inherit (override_flag loc ov) (class_expr ce) opb) :: l]
    | CrIni loc e -> [mkcf loc (Pcf_initializer (expr e)) :: l]
    | CrMth loc s ov pf e t ->
        let t =
          match t with
          [ <:ctyp<>> -> None
          | t -> Some (mkpolytype (ctyp t)) ] in
        let e = mkexp loc (Pexp_poly (expr e) t) in
        [mkcf loc (Pcf_method (with_loc s loc, mkprivate pf, Cfk_concrete (override_flag loc ov, e))) :: l]
    | CrVal loc s ov mf e ->
        [mkcf loc (Pcf_val (with_loc s loc, mkmutable mf, Cfk_concrete (override_flag loc ov, expr e))) :: l]
    | CrVir loc s pf t ->
        [mkcf loc (Pcf_method (with_loc s loc, mkprivate pf, Cfk_virtual (mkpolytype (ctyp t)))) :: l]
    | CrVvr loc s mf t ->
        [mkcf loc (Pcf_val (with_loc s loc, mkmutable mf, Cfk_virtual (ctyp t))) :: l]
    | CrAnt _ _ -> assert False ];

  value sig_item ast = sig_item ast [];
  value str_item ast = str_item ast [];

  value directive =
    fun
    [ <:expr<>> -> Pdir_none
    | ExStr _ s -> Pdir_string s
    | ExInt _ i -> Pdir_int (int_of_string i)
    | <:expr< True >> -> Pdir_bool True
    | <:expr< False >> -> Pdir_bool False
    | e -> Pdir_ident (ident_noloc (ident_of_expr e)) ]
  ;

  value phrase =
    fun
    [ StDir _ d dp -> Ptop_dir d (directive dp)
    | si -> Ptop_def (str_item si) ]
  ;

  value attribute loc s str =
    (with_loc s loc, PStr (str_item str));

  value () =
    attribute_fwd.val := attribute;
end;
