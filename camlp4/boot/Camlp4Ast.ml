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
module Make (Loc : Sig.Loc) : Sig.Camlp4Ast with module Loc = Loc =
  struct
    module Loc = Loc;
    module Ast =
      struct
        include (Sig.MakeCamlp4Ast Loc);
        value safe_string_escaped s =
          if ((String.length s) > 2) && ((s.[0] = '\\') && (s.[1] = '$'))
          then s
          else String.escaped s;
      end;
    include Ast;
    external loc_of_ctyp : ctyp -> Loc.t = "%field0";
    external loc_of_patt : patt -> Loc.t = "%field0";
    external loc_of_expr : expr -> Loc.t = "%field0";
    external loc_of_module_type : module_type -> Loc.t = "%field0";
    external loc_of_module_expr : module_expr -> Loc.t = "%field0";
    external loc_of_sig_item : sig_item -> Loc.t = "%field0";
    external loc_of_str_item : str_item -> Loc.t = "%field0";
    external loc_of_class_type : class_type -> Loc.t = "%field0";
    external loc_of_class_sig_item : class_sig_item -> Loc.t = "%field0";
    external loc_of_class_expr : class_expr -> Loc.t = "%field0";
    external loc_of_class_str_item : class_str_item -> Loc.t = "%field0";
    external loc_of_with_constr : with_constr -> Loc.t = "%field0";
    external loc_of_binding : binding -> Loc.t = "%field0";
    external loc_of_rec_binding : rec_binding -> Loc.t = "%field0";
    external loc_of_module_binding : module_binding -> Loc.t = "%field0";
    external loc_of_match_case : match_case -> Loc.t = "%field0";
    external loc_of_ident : ident -> Loc.t = "%field0";
    value ghost = Loc.ghost;
    value rec is_module_longident =
      fun
      [ Ast.IdAcc _ _ i -> is_module_longident i
      | Ast.IdApp _ i1 i2 ->
          (is_module_longident i1) && (is_module_longident i2)
      | Ast.IdUid _ _ -> True
      | _ -> False ];
    value ident_of_expr =
      let error () =
        invalid_arg "ident_of_expr: this expression is not an identifier" in
      let rec self =
        fun
        [ Ast.ExApp _loc e1 e2 -> Ast.IdApp _loc (self e1) (self e2)
        | Ast.ExAcc _loc e1 e2 -> Ast.IdAcc _loc (self e1) (self e2)
        | Ast.ExId _ (Ast.IdLid _ _) -> error ()
        | Ast.ExId _ i -> if is_module_longident i then i else error ()
        | _ -> error () ]
      in
        fun [ Ast.ExId _ i -> i | Ast.ExApp _ _ _ -> error () | t -> self t ];
    value ident_of_ctyp =
      let error () =
        invalid_arg "ident_of_ctyp: this type is not an identifier" in
      let rec self =
        fun
        [ Ast.TyApp _loc t1 t2 -> Ast.IdApp _loc (self t1) (self t2)
        | Ast.TyId _ (Ast.IdLid _ _) -> error ()
        | Ast.TyId _ i -> if is_module_longident i then i else error ()
        | _ -> error () ]
      in fun [ Ast.TyId _ i -> i | t -> self t ];
    value ident_of_patt =
      let error () =
        invalid_arg "ident_of_patt: this pattern is not an identifier" in
      let rec self =
        fun
        [ Ast.PaApp _loc p1 p2 -> Ast.IdApp _loc (self p1) (self p2)
        | Ast.PaId _ (Ast.IdLid _ _) -> error ()
        | Ast.PaId _ i -> if is_module_longident i then i else error ()
        | _ -> error () ]
      in fun [ Ast.PaId _ i -> i | p -> self p ];
    value rec is_irrefut_patt =
      fun
      [ Ast.PaId _ (Ast.IdLid _ _) -> True
      | Ast.PaId _ (Ast.IdUid _ "()") -> True
      | Ast.PaAny _ -> True
      | Ast.PaNil _ -> True
      | (* why not *) Ast.PaAli _ x y ->
          (is_irrefut_patt x) && (is_irrefut_patt y)
      | Ast.PaRec _ p -> is_irrefut_patt p
      | Ast.PaEq _ _ p -> is_irrefut_patt p
      | Ast.PaSem _ p1 p2 -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
      | Ast.PaCom _ p1 p2 -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
      | Ast.PaOrp _ p1 p2 -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
      | (* could be more fine grained *) Ast.PaApp _ p1 p2 ->
          (is_irrefut_patt p1) && (is_irrefut_patt p2)
      | Ast.PaTyc _ p _ -> is_irrefut_patt p
      | Ast.PaTup _ pl -> is_irrefut_patt pl
      | Ast.PaOlb _ _ (Ast.PaNil _) -> True
      | Ast.PaOlb _ _ p -> is_irrefut_patt p
      | Ast.PaOlbi _ _ p _ -> is_irrefut_patt p
      | Ast.PaLab _ _ (Ast.PaNil _) -> True
      | Ast.PaLab _ _ p -> is_irrefut_patt p
      | Ast.PaLaz _ p -> is_irrefut_patt p
      | Ast.PaId _ _ -> False
      | (* here one need to know the arity of constructors *)
          Ast.PaVrn _ _ | Ast.PaStr _ _ | Ast.PaRng _ _ _ | Ast.PaFlo _ _ |
            Ast.PaNativeInt _ _ | Ast.PaInt64 _ _ | Ast.PaInt32 _ _ |
            Ast.PaInt _ _ | Ast.PaChr _ _ | Ast.PaTyp _ _ | Ast.PaArr _ _ |
            Ast.PaAnt _ _
          -> False ];
    value rec is_constructor =
      fun
      [ Ast.IdAcc _ _ i -> is_constructor i
      | Ast.IdUid _ _ -> True
      | Ast.IdLid _ _ | Ast.IdApp _ _ _ -> False
      | Ast.IdAnt _ _ -> assert False ];
    value is_patt_constructor =
      fun
      [ Ast.PaId _ i -> is_constructor i
      | Ast.PaVrn _ _ -> True
      | _ -> False ];
    value rec is_expr_constructor =
      fun
      [ Ast.ExId _ i -> is_constructor i
      | Ast.ExAcc _ e1 e2 ->
          (is_expr_constructor e1) && (is_expr_constructor e2)
      | Ast.ExVrn _ _ -> True
      | _ -> False ];
    value rec tyOr_of_list =
      fun
      [ [] -> Ast.TyNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_ctyp t in Ast.TyOr _loc t (tyOr_of_list ts) ];
    value rec tyAnd_of_list =
      fun
      [ [] -> Ast.TyNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_ctyp t in Ast.TyAnd _loc t (tyAnd_of_list ts) ];
    value rec tySem_of_list =
      fun
      [ [] -> Ast.TyNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_ctyp t in Ast.TySem _loc t (tySem_of_list ts) ];
    value rec tyCom_of_list =
      fun
      [ [] -> Ast.TyNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_ctyp t in Ast.TyCom _loc t (tyCom_of_list ts) ];
    value rec tyAmp_of_list =
      fun
      [ [] -> Ast.TyNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_ctyp t in Ast.TyAmp _loc t (tyAmp_of_list ts) ];
    value rec tySta_of_list =
      fun
      [ [] -> Ast.TyNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_ctyp t in Ast.TySta _loc t (tySta_of_list ts) ];
    value rec stSem_of_list =
      fun
      [ [] -> Ast.StNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_str_item t in Ast.StSem _loc t (stSem_of_list ts) ];
    value rec sgSem_of_list =
      fun
      [ [] -> Ast.SgNil ghost
      | [ t ] -> t
      | [ t :: ts ] ->
          let _loc = loc_of_sig_item t in Ast.SgSem _loc t (sgSem_of_list ts) ];
    value rec biAnd_of_list =
      fun
      [ [] -> Ast.BiNil ghost
      | [ b ] -> b
      | [ b :: bs ] ->
          let _loc = loc_of_binding b in Ast.BiAnd _loc b (biAnd_of_list bs) ];
    value rec rbSem_of_list =
      fun
      [ [] -> Ast.RbNil ghost
      | [ b ] -> b
      | [ b :: bs ] ->
          let _loc = loc_of_rec_binding b
          in Ast.RbSem _loc b (rbSem_of_list bs) ];
    value rec wcAnd_of_list =
      fun
      [ [] -> Ast.WcNil ghost
      | [ w ] -> w
      | [ w :: ws ] ->
          let _loc = loc_of_with_constr w
          in Ast.WcAnd _loc w (wcAnd_of_list ws) ];
    value rec idAcc_of_list =
      fun
      [ [] -> assert False
      | [ i ] -> i
      | [ i :: is ] ->
          let _loc = loc_of_ident i in Ast.IdAcc _loc i (idAcc_of_list is) ];
    value rec idApp_of_list =
      fun
      [ [] -> assert False
      | [ i ] -> i
      | [ i :: is ] ->
          let _loc = loc_of_ident i in Ast.IdApp _loc i (idApp_of_list is) ];
    value rec mcOr_of_list =
      fun
      [ [] -> Ast.McNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_match_case x in Ast.McOr _loc x (mcOr_of_list xs) ];
    value rec mbAnd_of_list =
      fun
      [ [] -> Ast.MbNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_module_binding x
          in Ast.MbAnd _loc x (mbAnd_of_list xs) ];
    value rec meApp_of_list =
      fun
      [ [] -> assert False
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_module_expr x
          in Ast.MeApp _loc x (meApp_of_list xs) ];
    value rec ceAnd_of_list =
      fun
      [ [] -> Ast.CeNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_class_expr x
          in Ast.CeAnd _loc x (ceAnd_of_list xs) ];
    value rec ctAnd_of_list =
      fun
      [ [] -> Ast.CtNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_class_type x
          in Ast.CtAnd _loc x (ctAnd_of_list xs) ];
    value rec cgSem_of_list =
      fun
      [ [] -> Ast.CgNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_class_sig_item x
          in Ast.CgSem _loc x (cgSem_of_list xs) ];
    value rec crSem_of_list =
      fun
      [ [] -> Ast.CrNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_class_str_item x
          in Ast.CrSem _loc x (crSem_of_list xs) ];
    value rec paSem_of_list =
      fun
      [ [] -> Ast.PaNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_patt x in Ast.PaSem _loc x (paSem_of_list xs) ];
    value rec paCom_of_list =
      fun
      [ [] -> Ast.PaNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_patt x in Ast.PaCom _loc x (paCom_of_list xs) ];
    value rec exSem_of_list =
      fun
      [ [] -> Ast.ExNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_expr x in Ast.ExSem _loc x (exSem_of_list xs) ];
    value rec exCom_of_list =
      fun
      [ [] -> Ast.ExNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_expr x in Ast.ExCom _loc x (exCom_of_list xs) ];
    value ty_of_stl =
      fun
      [ (_loc, s, []) -> Ast.TyId _loc (Ast.IdUid _loc s)
      | (_loc, s, tl) ->
          Ast.TyOf _loc (Ast.TyId _loc (Ast.IdUid _loc s)) (tyAnd_of_list tl) ];
    value ty_of_sbt =
      fun
      [ (_loc, s, True, t) ->
          Ast.TyCol _loc (Ast.TyId _loc (Ast.IdLid _loc s))
            (Ast.TyMut _loc t)
      | (_loc, s, False, t) ->
          Ast.TyCol _loc (Ast.TyId _loc (Ast.IdLid _loc s)) t ];
    value bi_of_pe (p, e) = let _loc = loc_of_patt p in Ast.BiEq _loc p e;
    value sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l);
    value record_type_of_list l = tySem_of_list (List.map ty_of_sbt l);
    value binding_of_pel l = biAnd_of_list (List.map bi_of_pe l);
    value rec pel_of_binding =
      fun
      [ Ast.BiAnd _ b1 b2 -> (pel_of_binding b1) @ (pel_of_binding b2)
      | Ast.BiEq _ p e -> [ (p, e) ]
      | _ -> assert False ];
    value rec list_of_binding x acc =
      match x with
      [ Ast.BiAnd _ b1 b2 -> list_of_binding b1 (list_of_binding b2 acc)
      | t -> [ t :: acc ] ];
    value rec list_of_rec_binding x acc =
      match x with
      [ Ast.RbSem _ b1 b2 ->
          list_of_rec_binding b1 (list_of_rec_binding b2 acc)
      | t -> [ t :: acc ] ];
    value rec list_of_with_constr x acc =
      match x with
      [ Ast.WcAnd _ w1 w2 ->
          list_of_with_constr w1 (list_of_with_constr w2 acc)
      | t -> [ t :: acc ] ];
    value rec list_of_ctyp x acc =
      match x with
      [ Ast.TyNil _ -> acc
      | Ast.TyAmp _ x y | Ast.TyCom _ x y | Ast.TySta _ x y | Ast.TySem _ x y
          | Ast.TyAnd _ x y | Ast.TyOr _ x y ->
          list_of_ctyp x (list_of_ctyp y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_patt x acc =
      match x with
      [ Ast.PaNil _ -> acc
      | Ast.PaCom _ x y | Ast.PaSem _ x y ->
          list_of_patt x (list_of_patt y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_expr x acc =
      match x with
      [ Ast.ExNil _ -> acc
      | Ast.ExCom _ x y | Ast.ExSem _ x y ->
          list_of_expr x (list_of_expr y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_str_item x acc =
      match x with
      [ Ast.StNil _ -> acc
      | Ast.StSem _ x y -> list_of_str_item x (list_of_str_item y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_sig_item x acc =
      match x with
      [ Ast.SgNil _ -> acc
      | Ast.SgSem _ x y -> list_of_sig_item x (list_of_sig_item y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_class_sig_item x acc =
      match x with
      [ Ast.CgNil _ -> acc
      | Ast.CgSem _ x y ->
          list_of_class_sig_item x (list_of_class_sig_item y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_class_str_item x acc =
      match x with
      [ Ast.CrNil _ -> acc
      | Ast.CrSem _ x y ->
          list_of_class_str_item x (list_of_class_str_item y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_class_type x acc =
      match x with
      [ Ast.CtAnd _ x y -> list_of_class_type x (list_of_class_type y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_class_expr x acc =
      match x with
      [ Ast.CeAnd _ x y -> list_of_class_expr x (list_of_class_expr y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_module_expr x acc =
      match x with
      [ Ast.MeApp _ x y -> list_of_module_expr x (list_of_module_expr y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_match_case x acc =
      match x with
      [ Ast.McNil _ -> acc
      | Ast.McOr _ x y -> list_of_match_case x (list_of_match_case y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_ident x acc =
      match x with
      [ Ast.IdAcc _ x y | Ast.IdApp _ x y ->
          list_of_ident x (list_of_ident y acc)
      | x -> [ x :: acc ] ];
    value rec list_of_module_binding x acc =
      match x with
      [ Ast.MbAnd _ x y ->
          list_of_module_binding x (list_of_module_binding y acc)
      | x -> [ x :: acc ] ];
    module Meta =
      struct
        module type META_LOC =
          sig
            value meta_loc_patt : Loc.t -> Loc.t -> Ast.patt;
            value meta_loc_expr : Loc.t -> Loc.t -> Ast.expr;
          end;
        module MetaLoc =
          struct
            value meta_loc_patt _loc location =
              let (a, b, c, d, e, f, g, h) = Loc.to_tuple location
              in
                Ast.PaApp _loc
                  (Ast.PaId _loc
                     (Ast.IdAcc _loc (Ast.IdUid _loc "Loc")
                        (Ast.IdLid _loc "of_tuple")))
                  (Ast.PaTup _loc
                     (Ast.PaCom _loc
                        (Ast.PaStr _loc (Ast.safe_string_escaped a))
                        (Ast.PaCom _loc
                           (Ast.PaCom _loc
                              (Ast.PaCom _loc
                                 (Ast.PaCom _loc
                                    (Ast.PaCom _loc
                                       (Ast.PaCom _loc
                                          (Ast.PaInt _loc (string_of_int b))
                                          (Ast.PaInt _loc (string_of_int c)))
                                       (Ast.PaInt _loc (string_of_int d)))
                                    (Ast.PaInt _loc (string_of_int e)))
                                 (Ast.PaInt _loc (string_of_int f)))
                              (Ast.PaInt _loc (string_of_int g)))
                           (if h
                            then Ast.PaId _loc (Ast.IdUid _loc "True")
                            else Ast.PaId _loc (Ast.IdUid _loc "False")))));
            value meta_loc_expr _loc location =
              let (a, b, c, d, e, f, g, h) = Loc.to_tuple location
              in
                Ast.ExApp _loc
                  (Ast.ExId _loc
                     (Ast.IdAcc _loc (Ast.IdUid _loc "Loc")
                        (Ast.IdLid _loc "of_tuple")))
                  (Ast.ExTup _loc
                     (Ast.ExCom _loc
                        (Ast.ExStr _loc (Ast.safe_string_escaped a))
                        (Ast.ExCom _loc
                           (Ast.ExCom _loc
                              (Ast.ExCom _loc
                                 (Ast.ExCom _loc
                                    (Ast.ExCom _loc
                                       (Ast.ExCom _loc
                                          (Ast.ExInt _loc (string_of_int b))
                                          (Ast.ExInt _loc (string_of_int c)))
                                       (Ast.ExInt _loc (string_of_int d)))
                                    (Ast.ExInt _loc (string_of_int e)))
                                 (Ast.ExInt _loc (string_of_int f)))
                              (Ast.ExInt _loc (string_of_int g)))
                           (if h
                            then Ast.ExId _loc (Ast.IdUid _loc "True")
                            else Ast.ExId _loc (Ast.IdUid _loc "False")))));
          end;
        module MetaGhostLoc =
          struct
            value meta_loc_patt _loc _ =
              Ast.PaId _loc
                (Ast.IdAcc _loc (Ast.IdUid _loc "Loc")
                   (Ast.IdLid _loc "ghost"));
            value meta_loc_expr _loc _ =
              Ast.ExId _loc
                (Ast.IdAcc _loc (Ast.IdUid _loc "Loc")
                   (Ast.IdLid _loc "ghost"));
          end;
        module MetaLocVar =
          struct
            value meta_loc_patt _loc _ =
              Ast.PaId _loc (Ast.IdLid _loc Loc.name.val);
            value meta_loc_expr _loc _ =
              Ast.ExId _loc (Ast.IdLid _loc Loc.name.val);
          end;
        module Make (MetaLoc : META_LOC) =
          struct
            open MetaLoc;
            value meta_loc = meta_loc_expr;
            module Expr =
              struct
                value meta_string _loc s = Ast.ExStr _loc s;
                value meta_int _loc s = Ast.ExInt _loc s;
                value meta_float _loc s = Ast.ExFlo _loc s;
                value meta_char _loc s = Ast.ExChr _loc s;
                value meta_bool _loc =
                  fun
                  [ False -> Ast.ExId _loc (Ast.IdUid _loc "False")
                  | True -> Ast.ExId _loc (Ast.IdUid _loc "True") ];
                value rec meta_list mf_a _loc =
                  fun
                  [ [] -> Ast.ExId _loc (Ast.IdUid _loc "[]")
                  | [ x :: xs ] ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc (Ast.ExId _loc (Ast.IdUid _loc "::"))
                           (mf_a _loc x))
                        (meta_list mf_a _loc xs) ];
                value rec meta_binding _loc =
                  fun
                  [ Ast.BiAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.BiEq x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "BiEq")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_expr _loc x2)
                  | Ast.BiAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "BiAnd")))
                              (meta_loc _loc x0))
                           (meta_binding _loc x1))
                        (meta_binding _loc x2)
                  | Ast.BiNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "BiNil")))
                        (meta_loc _loc x0) ]
                and meta_class_expr _loc =
                  fun
                  [ Ast.CeAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.CeEq x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeEq")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeAnd")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeTyc")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CeStr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeStr")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.CeLet x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CeLet")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_binding _loc x2))
                        (meta_class_expr _loc x3)
                  | Ast.CeFun x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeFun")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeCon x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CeCon")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_ident _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CeApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeApp")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.CeNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CeNil")))
                        (meta_loc _loc x0) ]
                and meta_class_sig_item _loc =
                  fun
                  [ Ast.CgAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.CgVir x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CgVir")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CgVal x0 x1 x2 x3 x4 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExApp _loc
                                    (Ast.ExId _loc
                                       (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                          (Ast.IdUid _loc "CgVal")))
                                    (meta_loc _loc x0))
                                 (meta_string _loc x1))
                              (meta_meta_bool _loc x2))
                           (meta_meta_bool _loc x3))
                        (meta_ctyp _loc x4)
                  | Ast.CgMth x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CgMth")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CgInh x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "CgInh")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.CgSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgSem")))
                              (meta_loc _loc x0))
                           (meta_class_sig_item _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CgCtr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgCtr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CgNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CgNil")))
                        (meta_loc _loc x0) ]
                and meta_class_str_item _loc =
                  fun
                  [ Ast.CrAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.CrVvr x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CrVvr")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CrVir x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CrVir")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CrVal x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CrVal")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_expr _loc x3)
                  | Ast.CrMth x0 x1 x2 x3 x4 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExApp _loc
                                    (Ast.ExId _loc
                                       (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                          (Ast.IdUid _loc "CrMth")))
                                    (meta_loc _loc x0))
                                 (meta_string _loc x1))
                              (meta_meta_bool _loc x2))
                           (meta_expr _loc x3))
                        (meta_ctyp _loc x4)
                  | Ast.CrIni x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "CrIni")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.CrInh x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrInh")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.CrCtr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrCtr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CrSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrSem")))
                              (meta_loc _loc x0))
                           (meta_class_str_item _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.CrNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CrNil")))
                        (meta_loc _loc x0) ]
                and meta_class_type _loc =
                  fun
                  [ Ast.CtAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.CtEq x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtEq")))
                              (meta_loc _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtCol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtCol")))
                              (meta_loc _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtAnd")))
                              (meta_loc _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtSig x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtSig")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CtFun x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtFun")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtCon x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CtCon")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_ident _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CtNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CtNil")))
                        (meta_loc _loc x0) ]
                and meta_ctyp _loc =
                  fun
                  [ Ast.TyAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.TyOfAmp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOfAmp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAmp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAmp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInfSup x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyVrnInfSup")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInf x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnInf")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnSup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnSup")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnEq x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnEq")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TySta x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySta")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyTup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyTup")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyMut x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyMut")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyPrv x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyPrv")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyOr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAnd")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOf x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOf")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySum x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TySum")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyCom x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCom")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySem")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCol")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyRec x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyRec")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrn")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuM x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuM")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuP x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuP")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyPol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyPol")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOlb x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOlb")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyObj x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyObj")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_meta_bool _loc x2)
                  | Ast.TyDcl x0 x1 x2 x3 x4 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExApp _loc
                                    (Ast.ExId _loc
                                       (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                          (Ast.IdUid _loc "TyDcl")))
                                    (meta_loc _loc x0))
                                 (meta_string _loc x1))
                              (meta_list meta_ctyp _loc x2))
                           (meta_ctyp _loc x3))
                        (meta_list
                           (fun _loc (x1, x2) ->
                              Ast.ExTup _loc
                                (Ast.ExCom _loc (meta_ctyp _loc x1)
                                   (meta_ctyp _loc x2)))
                           _loc x4)
                  | Ast.TyMan x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyMan")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyLab x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyLab")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCls x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyCls")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyArr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyArr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyApp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAny x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyAny")))
                        (meta_loc _loc x0)
                  | Ast.TyAli x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAli")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyNil")))
                        (meta_loc _loc x0) ]
                and meta_expr _loc =
                  fun
                  [ Ast.ExWhi x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExWhi")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExVrn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExVrn")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTyc")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.ExCom x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExCom")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExTup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExTup")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExTry x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTry")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.ExStr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExStr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExSte x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSte")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExSnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSnd")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.ExSeq x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExSeq")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExRec x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExRec")))
                              (meta_loc _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExOvr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExOvr")))
                           (meta_loc _loc x0))
                        (meta_rec_binding _loc x1)
                  | Ast.ExOlb x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExOlb")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExObj x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExObj")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.ExNew x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNew")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExMat x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExMat")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.ExLmd x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExLmd")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExLet x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExLet")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_binding _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExLaz x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExLaz")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExLab x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExLab")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExNativeInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNativeInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt64 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt64")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt32 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt32")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExIfe x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExIfe")))
                                 (meta_loc _loc x0))
                              (meta_expr _loc x1))
                           (meta_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExFun x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExFun")))
                           (meta_loc _loc x0))
                        (meta_match_case _loc x1)
                  | Ast.ExFor x0 x1 x2 x3 x4 x5 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExApp _loc
                                    (Ast.ExApp _loc
                                       (Ast.ExId _loc
                                          (Ast.IdAcc _loc
                                             (Ast.IdUid _loc "Ast")
                                             (Ast.IdUid _loc "ExFor")))
                                       (meta_loc _loc x0))
                                    (meta_string _loc x1))
                                 (meta_expr _loc x2))
                              (meta_expr _loc x3))
                           (meta_meta_bool _loc x4))
                        (meta_expr _loc x5)
                  | Ast.ExFlo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExFlo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExCoe x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExCoe")))
                                 (meta_loc _loc x0))
                              (meta_expr _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.ExChr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExChr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExAss x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAss")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExAsr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExAsr")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAsf x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExAsf")))
                        (meta_loc _loc x0)
                  | Ast.ExSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSem")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExArr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExArr")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAre x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAre")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExApp")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.ExAcc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAcc")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExNil")))
                        (meta_loc _loc x0) ]
                and meta_ident _loc =
                  fun
                  [ Ast.IdAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.IdUid x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdUid")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdLid x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdLid")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdApp")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.IdAcc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdAcc")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2) ]
                and meta_match_case _loc =
                  fun
                  [ Ast.McAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.McArr x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "McArr")))
                                 (meta_loc _loc x0))
                              (meta_patt _loc x1))
                           (meta_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.McOr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "McOr")))
                              (meta_loc _loc x0))
                           (meta_match_case _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.McNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "McNil")))
                        (meta_loc _loc x0) ]
                and meta_meta_bool _loc =
                  fun
                  [ Ast.BAnt x0 -> Ast.ExAnt _loc x0
                  | Ast.BFalse ->
                      Ast.ExId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "BFalse"))
                  | Ast.BTrue ->
                      Ast.ExId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "BTrue")) ]
                and meta_meta_list mf_a _loc =
                  fun
                  [ Ast.LAnt x0 -> Ast.ExAnt _loc x0
                  | Ast.LCons x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "LCons")))
                           (mf_a _loc x0))
                        (meta_meta_list mf_a _loc x1)
                  | Ast.LNil ->
                      Ast.ExId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "LNil")) ]
                and meta_meta_option mf_a _loc =
                  fun
                  [ Ast.OAnt x0 -> Ast.ExAnt _loc x0
                  | Ast.OSome x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "OSome")))
                        (mf_a _loc x0)
                  | Ast.ONone ->
                      Ast.ExId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "ONone")) ]
                and meta_module_binding _loc =
                  fun
                  [ Ast.MbAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.MbCol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MbCol")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.MbColEq x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MbColEq")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_expr _loc x3)
                  | Ast.MbAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MbAnd")))
                              (meta_loc _loc x0))
                           (meta_module_binding _loc x1))
                        (meta_module_binding _loc x2)
                  | Ast.MbNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MbNil")))
                        (meta_loc _loc x0) ]
                and meta_module_expr _loc =
                  fun
                  [ Ast.MeAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.MeTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MeTyc")))
                              (meta_loc _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.MeStr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeStr")))
                           (meta_loc _loc x0))
                        (meta_str_item _loc x1)
                  | Ast.MeFun x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MeFun")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_expr _loc x3)
                  | Ast.MeApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MeApp")))
                              (meta_loc _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.MeId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MeNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MeNil")))
                        (meta_loc _loc x0) ]
                and meta_module_type _loc =
                  fun
                  [ Ast.MtAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.MtWit x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MtWit")))
                              (meta_loc _loc x0))
                           (meta_module_type _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.MtSig x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtSig")))
                           (meta_loc _loc x0))
                        (meta_sig_item _loc x1)
                  | Ast.MtQuo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtQuo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.MtFun x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MtFun")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_type _loc x3)
                  | Ast.MtId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MtNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MtNil")))
                        (meta_loc _loc x0) ]
                and meta_patt _loc =
                  fun
                  [ Ast.PaLaz x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaLaz")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaVrn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaVrn")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaTyp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTyp")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaTyc")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.PaTup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTup")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaStr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaStr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaEq x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaEq")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaRec x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaRec")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaRng x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaRng")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaOrp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaOrp")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaOlbi x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "PaOlbi")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_patt _loc x2))
                        (meta_expr _loc x3)
                  | Ast.PaOlb x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaOlb")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaLab x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaLab")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaFlo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaFlo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaNativeInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaNativeInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt64 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt64")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt32 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt32")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaChr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaChr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaSem")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaCom x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaCom")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaArr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaArr")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaApp")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaAny x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaAny")))
                        (meta_loc _loc x0)
                  | Ast.PaAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.PaAli x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaAli")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaNil")))
                        (meta_loc _loc x0) ]
                and meta_rec_binding _loc =
                  fun
                  [ Ast.RbAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.RbEq x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "RbEq")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_expr _loc x2)
                  | Ast.RbSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "RbSem")))
                              (meta_loc _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_rec_binding _loc x2)
                  | Ast.RbNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "RbNil")))
                        (meta_loc _loc x0) ]
                and meta_sig_item _loc =
                  fun
                  [ Ast.SgAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.SgVal x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgVal")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.SgTyp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgTyp")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgOpn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgOpn")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.SgMty x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMty")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgRecMod x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgRecMod")))
                           (meta_loc _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.SgMod x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMod")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgInc x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgInc")))
                           (meta_loc _loc x0))
                        (meta_module_type _loc x1)
                  | Ast.SgExt x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "SgExt")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.SgExc x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgExc")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgDir x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgDir")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.SgSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgSem")))
                              (meta_loc _loc x0))
                           (meta_sig_item _loc x1))
                        (meta_sig_item _loc x2)
                  | Ast.SgClt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgClt")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgCls x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgCls")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "SgNil")))
                        (meta_loc _loc x0) ]
                and meta_str_item _loc =
                  fun
                  [ Ast.StAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.StVal x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StVal")))
                              (meta_loc _loc x0))
                           (meta_meta_bool _loc x1))
                        (meta_binding _loc x2)
                  | Ast.StTyp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StTyp")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.StOpn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StOpn")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.StMty x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMty")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.StRecMod x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StRecMod")))
                           (meta_loc _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.StMod x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMod")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.StInc x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StInc")))
                           (meta_loc _loc x0))
                        (meta_module_expr _loc x1)
                  | Ast.StExt x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "StExt")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.StExp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StExp")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.StExc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StExc")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_meta_option meta_ident _loc x2)
                  | Ast.StDir x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StDir")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.StSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StSem")))
                              (meta_loc _loc x0))
                           (meta_str_item _loc x1))
                        (meta_str_item _loc x2)
                  | Ast.StClt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StClt")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.StCls x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StCls")))
                           (meta_loc _loc x0))
                        (meta_class_expr _loc x1)
                  | Ast.StNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "StNil")))
                        (meta_loc _loc x0) ]
                and meta_with_constr _loc =
                  fun
                  [ Ast.WcAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.WcAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcAnd")))
                              (meta_loc _loc x0))
                           (meta_with_constr _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.WcMod x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcMod")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.WcTyp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcTyp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.WcNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "WcNil")))
                        (meta_loc _loc x0) ];
              end;
            value meta_loc = meta_loc_patt;
            module Patt =
              struct
                value meta_string _loc s = Ast.PaStr _loc s;
                value meta_int _loc s = Ast.PaInt _loc s;
                value meta_float _loc s = Ast.PaFlo _loc s;
                value meta_char _loc s = Ast.PaChr _loc s;
                value meta_bool _loc =
                  fun
                  [ False -> Ast.PaId _loc (Ast.IdUid _loc "False")
                  | True -> Ast.PaId _loc (Ast.IdUid _loc "True") ];
                value rec meta_list mf_a _loc =
                  fun
                  [ [] -> Ast.PaId _loc (Ast.IdUid _loc "[]")
                  | [ x :: xs ] ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc (Ast.PaId _loc (Ast.IdUid _loc "::"))
                           (mf_a _loc x))
                        (meta_list mf_a _loc xs) ];
                value rec meta_binding _loc =
                  fun
                  [ Ast.BiAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.BiEq x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "BiEq")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_expr _loc x2)
                  | Ast.BiAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "BiAnd")))
                              (meta_loc _loc x0))
                           (meta_binding _loc x1))
                        (meta_binding _loc x2)
                  | Ast.BiNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "BiNil")))
                        (meta_loc _loc x0) ]
                and meta_class_expr _loc =
                  fun
                  [ Ast.CeAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.CeEq x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeEq")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeAnd")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeTyc")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CeStr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeStr")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.CeLet x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CeLet")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_binding _loc x2))
                        (meta_class_expr _loc x3)
                  | Ast.CeFun x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeFun")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeCon x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CeCon")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_ident _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CeApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeApp")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.CeNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CeNil")))
                        (meta_loc _loc x0) ]
                and meta_class_sig_item _loc =
                  fun
                  [ Ast.CgAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.CgVir x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CgVir")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CgVal x0 x1 x2 x3 x4 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaApp _loc
                                    (Ast.PaId _loc
                                       (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                          (Ast.IdUid _loc "CgVal")))
                                    (meta_loc _loc x0))
                                 (meta_string _loc x1))
                              (meta_meta_bool _loc x2))
                           (meta_meta_bool _loc x3))
                        (meta_ctyp _loc x4)
                  | Ast.CgMth x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CgMth")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CgInh x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "CgInh")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.CgSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgSem")))
                              (meta_loc _loc x0))
                           (meta_class_sig_item _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CgCtr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgCtr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CgNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CgNil")))
                        (meta_loc _loc x0) ]
                and meta_class_str_item _loc =
                  fun
                  [ Ast.CrAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.CrVvr x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CrVvr")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CrVir x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CrVir")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CrVal x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CrVal")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_expr _loc x3)
                  | Ast.CrMth x0 x1 x2 x3 x4 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaApp _loc
                                    (Ast.PaId _loc
                                       (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                          (Ast.IdUid _loc "CrMth")))
                                    (meta_loc _loc x0))
                                 (meta_string _loc x1))
                              (meta_meta_bool _loc x2))
                           (meta_expr _loc x3))
                        (meta_ctyp _loc x4)
                  | Ast.CrIni x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "CrIni")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.CrInh x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrInh")))
                              (meta_loc _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.CrCtr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrCtr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CrSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrSem")))
                              (meta_loc _loc x0))
                           (meta_class_str_item _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.CrNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CrNil")))
                        (meta_loc _loc x0) ]
                and meta_class_type _loc =
                  fun
                  [ Ast.CtAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.CtEq x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtEq")))
                              (meta_loc _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtCol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtCol")))
                              (meta_loc _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtAnd")))
                              (meta_loc _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtSig x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtSig")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CtFun x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtFun")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtCon x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "CtCon")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_ident _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CtNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CtNil")))
                        (meta_loc _loc x0) ]
                and meta_ctyp _loc =
                  fun
                  [ Ast.TyAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.TyOfAmp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOfAmp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAmp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAmp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInfSup x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyVrnInfSup")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInf x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnInf")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnSup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnSup")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnEq x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnEq")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TySta x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySta")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyTup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyTup")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyMut x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyMut")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyPrv x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyPrv")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyOr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAnd")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOf x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOf")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySum x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TySum")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyCom x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCom")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySem")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCol")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyRec x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyRec")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrn")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuM x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuM")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuP x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuP")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyPol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyPol")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOlb x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOlb")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyObj x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyObj")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_meta_bool _loc x2)
                  | Ast.TyDcl x0 x1 x2 x3 x4 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaApp _loc
                                    (Ast.PaId _loc
                                       (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                          (Ast.IdUid _loc "TyDcl")))
                                    (meta_loc _loc x0))
                                 (meta_string _loc x1))
                              (meta_list meta_ctyp _loc x2))
                           (meta_ctyp _loc x3))
                        (meta_list
                           (fun _loc (x1, x2) ->
                              Ast.PaTup _loc
                                (Ast.PaCom _loc (meta_ctyp _loc x1)
                                   (meta_ctyp _loc x2)))
                           _loc x4)
                  | Ast.TyMan x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyMan")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyLab x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyLab")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCls x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyCls")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyArr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyArr")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyApp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAny x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyAny")))
                        (meta_loc _loc x0)
                  | Ast.TyAli x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAli")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyNil")))
                        (meta_loc _loc x0) ]
                and meta_expr _loc =
                  fun
                  [ Ast.ExWhi x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExWhi")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExVrn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExVrn")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTyc")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.ExCom x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExCom")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExTup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExTup")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExTry x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTry")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.ExStr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExStr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExSte x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSte")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExSnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSnd")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.ExSeq x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExSeq")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExRec x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExRec")))
                              (meta_loc _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExOvr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExOvr")))
                           (meta_loc _loc x0))
                        (meta_rec_binding _loc x1)
                  | Ast.ExOlb x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExOlb")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExObj x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExObj")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.ExNew x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNew")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExMat x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExMat")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.ExLmd x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExLmd")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExLet x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExLet")))
                                 (meta_loc _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_binding _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExLaz x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExLaz")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExLab x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExLab")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExNativeInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNativeInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt64 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt64")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt32 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt32")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExIfe x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExIfe")))
                                 (meta_loc _loc x0))
                              (meta_expr _loc x1))
                           (meta_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExFun x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExFun")))
                           (meta_loc _loc x0))
                        (meta_match_case _loc x1)
                  | Ast.ExFor x0 x1 x2 x3 x4 x5 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaApp _loc
                                    (Ast.PaApp _loc
                                       (Ast.PaId _loc
                                          (Ast.IdAcc _loc
                                             (Ast.IdUid _loc "Ast")
                                             (Ast.IdUid _loc "ExFor")))
                                       (meta_loc _loc x0))
                                    (meta_string _loc x1))
                                 (meta_expr _loc x2))
                              (meta_expr _loc x3))
                           (meta_meta_bool _loc x4))
                        (meta_expr _loc x5)
                  | Ast.ExFlo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExFlo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExCoe x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExCoe")))
                                 (meta_loc _loc x0))
                              (meta_expr _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.ExChr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExChr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExAss x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAss")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExAsr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExAsr")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAsf x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExAsf")))
                        (meta_loc _loc x0)
                  | Ast.ExSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSem")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExArr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExArr")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAre x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAre")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExApp")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.ExAcc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAcc")))
                              (meta_loc _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExNil")))
                        (meta_loc _loc x0) ]
                and meta_ident _loc =
                  fun
                  [ Ast.IdAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.IdUid x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdUid")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdLid x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdLid")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdApp")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.IdAcc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdAcc")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2) ]
                and meta_match_case _loc =
                  fun
                  [ Ast.McAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.McArr x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "McArr")))
                                 (meta_loc _loc x0))
                              (meta_patt _loc x1))
                           (meta_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.McOr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "McOr")))
                              (meta_loc _loc x0))
                           (meta_match_case _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.McNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "McNil")))
                        (meta_loc _loc x0) ]
                and meta_meta_bool _loc =
                  fun
                  [ Ast.BAnt x0 -> Ast.PaAnt _loc x0
                  | Ast.BFalse ->
                      Ast.PaId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "BFalse"))
                  | Ast.BTrue ->
                      Ast.PaId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "BTrue")) ]
                and meta_meta_list mf_a _loc =
                  fun
                  [ Ast.LAnt x0 -> Ast.PaAnt _loc x0
                  | Ast.LCons x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "LCons")))
                           (mf_a _loc x0))
                        (meta_meta_list mf_a _loc x1)
                  | Ast.LNil ->
                      Ast.PaId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "LNil")) ]
                and meta_meta_option mf_a _loc =
                  fun
                  [ Ast.OAnt x0 -> Ast.PaAnt _loc x0
                  | Ast.OSome x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "OSome")))
                        (mf_a _loc x0)
                  | Ast.ONone ->
                      Ast.PaId _loc
                        (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                           (Ast.IdUid _loc "ONone")) ]
                and meta_module_binding _loc =
                  fun
                  [ Ast.MbAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.MbCol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MbCol")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.MbColEq x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MbColEq")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_expr _loc x3)
                  | Ast.MbAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MbAnd")))
                              (meta_loc _loc x0))
                           (meta_module_binding _loc x1))
                        (meta_module_binding _loc x2)
                  | Ast.MbNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MbNil")))
                        (meta_loc _loc x0) ]
                and meta_module_expr _loc =
                  fun
                  [ Ast.MeAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.MeTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MeTyc")))
                              (meta_loc _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.MeStr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeStr")))
                           (meta_loc _loc x0))
                        (meta_str_item _loc x1)
                  | Ast.MeFun x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MeFun")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_expr _loc x3)
                  | Ast.MeApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MeApp")))
                              (meta_loc _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.MeId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MeNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MeNil")))
                        (meta_loc _loc x0) ]
                and meta_module_type _loc =
                  fun
                  [ Ast.MtAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.MtWit x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "MtWit")))
                              (meta_loc _loc x0))
                           (meta_module_type _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.MtSig x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtSig")))
                           (meta_loc _loc x0))
                        (meta_sig_item _loc x1)
                  | Ast.MtQuo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtQuo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.MtFun x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MtFun")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_type _loc x3)
                  | Ast.MtId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MtNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MtNil")))
                        (meta_loc _loc x0) ]
                and meta_patt _loc =
                  fun
                  [ Ast.PaLaz x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaLaz")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaVrn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaVrn")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaTyp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTyp")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaTyc")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.PaTup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTup")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaStr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaStr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaEq x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaEq")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaRec x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaRec")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaRng x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaRng")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaOrp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaOrp")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaOlbi x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "PaOlbi")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_patt _loc x2))
                        (meta_expr _loc x3)
                  | Ast.PaOlb x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaOlb")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaLab x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaLab")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaFlo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaFlo")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaNativeInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaNativeInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt64 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt64")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt32 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt32")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaChr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaChr")))
                           (meta_loc _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaSem")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaCom x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaCom")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaArr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaArr")))
                           (meta_loc _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaApp")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaAny x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaAny")))
                        (meta_loc _loc x0)
                  | Ast.PaAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.PaAli x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaAli")))
                              (meta_loc _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaId")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaNil")))
                        (meta_loc _loc x0) ]
                and meta_rec_binding _loc =
                  fun
                  [ Ast.RbAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.RbEq x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "RbEq")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_expr _loc x2)
                  | Ast.RbSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "RbSem")))
                              (meta_loc _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_rec_binding _loc x2)
                  | Ast.RbNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "RbNil")))
                        (meta_loc _loc x0) ]
                and meta_sig_item _loc =
                  fun
                  [ Ast.SgAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.SgVal x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgVal")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.SgTyp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgTyp")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgOpn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgOpn")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.SgMty x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMty")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgRecMod x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgRecMod")))
                           (meta_loc _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.SgMod x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMod")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgInc x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgInc")))
                           (meta_loc _loc x0))
                        (meta_module_type _loc x1)
                  | Ast.SgExt x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "SgExt")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.SgExc x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgExc")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgDir x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgDir")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.SgSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgSem")))
                              (meta_loc _loc x0))
                           (meta_sig_item _loc x1))
                        (meta_sig_item _loc x2)
                  | Ast.SgClt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgClt")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgCls x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgCls")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "SgNil")))
                        (meta_loc _loc x0) ]
                and meta_str_item _loc =
                  fun
                  [ Ast.StAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.StVal x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StVal")))
                              (meta_loc _loc x0))
                           (meta_meta_bool _loc x1))
                        (meta_binding _loc x2)
                  | Ast.StTyp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StTyp")))
                           (meta_loc _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.StOpn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StOpn")))
                           (meta_loc _loc x0))
                        (meta_ident _loc x1)
                  | Ast.StMty x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMty")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.StRecMod x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StRecMod")))
                           (meta_loc _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.StMod x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMod")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.StInc x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StInc")))
                           (meta_loc _loc x0))
                        (meta_module_expr _loc x1)
                  | Ast.StExt x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "StExt")))
                                 (meta_loc _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.StExp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StExp")))
                           (meta_loc _loc x0))
                        (meta_expr _loc x1)
                  | Ast.StExc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StExc")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_meta_option meta_ident _loc x2)
                  | Ast.StDir x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StDir")))
                              (meta_loc _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.StSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StSem")))
                              (meta_loc _loc x0))
                           (meta_str_item _loc x1))
                        (meta_str_item _loc x2)
                  | Ast.StClt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StClt")))
                           (meta_loc _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.StCls x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StCls")))
                           (meta_loc _loc x0))
                        (meta_class_expr _loc x1)
                  | Ast.StNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "StNil")))
                        (meta_loc _loc x0) ]
                and meta_with_constr _loc =
                  fun
                  [ Ast.WcAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.WcAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcAnd")))
                              (meta_loc _loc x0))
                           (meta_with_constr _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.WcMod x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcMod")))
                              (meta_loc _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.WcTyp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcTyp")))
                              (meta_loc _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.WcNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "WcNil")))
                        (meta_loc _loc x0) ];
              end;
          end;
      end;
    class map =
      object ((o : 'self_type))
        method string : string -> string = o#unknown;
        method list :
          ! 'a 'a_out. ('self_type -> 'a -> 'a_out) -> list 'a -> list 'a_out =
          fun _f_a ->
            fun
            [ [] -> []
            | [ _x :: _x_i1 ] ->
                let _x = _f_a o _x in
                let _x_i1 = o#list _f_a _x_i1 in [ _x :: _x_i1 ] ];
        method with_constr : with_constr -> with_constr =
          fun
          [ WcNil _x -> let _x = o#loc _x in WcNil _x
          | WcTyp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in WcTyp _x _x_i1 _x_i2
          | WcMod _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in
              let _x_i2 = o#ident _x_i2 in WcMod _x _x_i1 _x_i2
          | WcAnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#with_constr _x_i1 in
              let _x_i2 = o#with_constr _x_i2 in WcAnd _x _x_i1 _x_i2
          | WcAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in WcAnt _x _x_i1 ];
        method str_item : str_item -> str_item =
          fun
          [ StNil _x -> let _x = o#loc _x in StNil _x
          | StCls _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_expr _x_i1 in StCls _x _x_i1
          | StClt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in StClt _x _x_i1
          | StSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#str_item _x_i1 in
              let _x_i2 = o#str_item _x_i2 in StSem _x _x_i1 _x_i2
          | StDir _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#expr _x_i2 in StDir _x _x_i1 _x_i2
          | StExc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#meta_option (fun o -> o#ident) _x_i2
              in StExc _x _x_i1 _x_i2
          | StExp _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in StExp _x _x_i1
          | StExt _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in
              let _x_i3 = o#meta_list (fun o -> o#string) _x_i3
              in StExt _x _x_i1 _x_i2 _x_i3
          | StInc _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_expr _x_i1 in StInc _x _x_i1
          | StMod _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_expr _x_i2 in StMod _x _x_i1 _x_i2
          | StRecMod _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_binding _x_i1 in StRecMod _x _x_i1
          | StMty _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in StMty _x _x_i1 _x_i2
          | StOpn _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in StOpn _x _x_i1
          | StTyp _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in StTyp _x _x_i1
          | StVal _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#meta_bool _x_i1 in
              let _x_i2 = o#binding _x_i2 in StVal _x _x_i1 _x_i2
          | StAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in StAnt _x _x_i1 ];
        method sig_item : sig_item -> sig_item =
          fun
          [ SgNil _x -> let _x = o#loc _x in SgNil _x
          | SgCls _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in SgCls _x _x_i1
          | SgClt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in SgClt _x _x_i1
          | SgSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#sig_item _x_i1 in
              let _x_i2 = o#sig_item _x_i2 in SgSem _x _x_i1 _x_i2
          | SgDir _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#expr _x_i2 in SgDir _x _x_i1 _x_i2
          | SgExc _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in SgExc _x _x_i1
          | SgExt _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in
              let _x_i3 = o#meta_list (fun o -> o#string) _x_i3
              in SgExt _x _x_i1 _x_i2 _x_i3
          | SgInc _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_type _x_i1 in SgInc _x _x_i1
          | SgMod _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in SgMod _x _x_i1 _x_i2
          | SgRecMod _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_binding _x_i1 in SgRecMod _x _x_i1
          | SgMty _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in SgMty _x _x_i1 _x_i2
          | SgOpn _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in SgOpn _x _x_i1
          | SgTyp _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in SgTyp _x _x_i1
          | SgVal _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in SgVal _x _x_i1 _x_i2
          | SgAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in SgAnt _x _x_i1 ];
        method rec_binding : rec_binding -> rec_binding =
          fun
          [ RbNil _x -> let _x = o#loc _x in RbNil _x
          | RbSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#rec_binding _x_i1 in
              let _x_i2 = o#rec_binding _x_i2 in RbSem _x _x_i1 _x_i2
          | RbEq _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in
              let _x_i2 = o#expr _x_i2 in RbEq _x _x_i1 _x_i2
          | RbAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in RbAnt _x _x_i1 ];
        method patt : patt -> patt =
          fun
          [ PaNil _x -> let _x = o#loc _x in PaNil _x
          | PaId _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ident _x_i1 in PaId _x _x_i1
          | PaAli _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaAli _x _x_i1 _x_i2
          | PaAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaAnt _x _x_i1
          | PaAny _x -> let _x = o#loc _x in PaAny _x
          | PaApp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaApp _x _x_i1 _x_i2
          | PaArr _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#patt _x_i1 in PaArr _x _x_i1
          | PaCom _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaCom _x _x_i1 _x_i2
          | PaSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaSem _x _x_i1 _x_i2
          | PaChr _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaChr _x _x_i1
          | PaInt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaInt _x _x_i1
          | PaInt32 _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaInt32 _x _x_i1
          | PaInt64 _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaInt64 _x _x_i1
          | PaNativeInt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaNativeInt _x _x_i1
          | PaFlo _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaFlo _x _x_i1
          | PaLab _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaLab _x _x_i1 _x_i2
          | PaOlb _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaOlb _x _x_i1 _x_i2
          | PaOlbi _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#patt _x_i2 in
              let _x_i3 = o#expr _x_i3 in PaOlbi _x _x_i1 _x_i2 _x_i3
          | PaOrp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaOrp _x _x_i1 _x_i2
          | PaRng _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaRng _x _x_i1 _x_i2
          | PaRec _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#patt _x_i1 in PaRec _x _x_i1
          | PaEq _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in
              let _x_i2 = o#patt _x_i2 in PaEq _x _x_i1 _x_i2
          | PaStr _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaStr _x _x_i1
          | PaTup _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#patt _x_i1 in PaTup _x _x_i1
          | PaTyc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in PaTyc _x _x_i1 _x_i2
          | PaTyp _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in PaTyp _x _x_i1
          | PaVrn _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in PaVrn _x _x_i1
          | PaLaz _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#patt _x_i1 in PaLaz _x _x_i1 ];
        method module_type : module_type -> module_type =
          fun
          [ MtNil _x -> let _x = o#loc _x in MtNil _x
          | MtId _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ident _x_i1 in MtId _x _x_i1
          | MtFun _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in
              let _x_i3 = o#module_type _x_i3 in MtFun _x _x_i1 _x_i2 _x_i3
          | MtQuo _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in MtQuo _x _x_i1
          | MtSig _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#sig_item _x_i1 in MtSig _x _x_i1
          | MtWit _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_type _x_i1 in
              let _x_i2 = o#with_constr _x_i2 in MtWit _x _x_i1 _x_i2
          | MtAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in MtAnt _x _x_i1 ];
        method module_expr : module_expr -> module_expr =
          fun
          [ MeNil _x -> let _x = o#loc _x in MeNil _x
          | MeId _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ident _x_i1 in MeId _x _x_i1
          | MeApp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_expr _x_i1 in
              let _x_i2 = o#module_expr _x_i2 in MeApp _x _x_i1 _x_i2
          | MeFun _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in
              let _x_i3 = o#module_expr _x_i3 in MeFun _x _x_i1 _x_i2 _x_i3
          | MeStr _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#str_item _x_i1 in MeStr _x _x_i1
          | MeTyc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_expr _x_i1 in
              let _x_i2 = o#module_type _x_i2 in MeTyc _x _x_i1 _x_i2
          | MeAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in MeAnt _x _x_i1 ];
        method module_binding : module_binding -> module_binding =
          fun
          [ MbNil _x -> let _x = o#loc _x in MbNil _x
          | MbAnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#module_binding _x_i1 in
              let _x_i2 = o#module_binding _x_i2 in MbAnd _x _x_i1 _x_i2
          | MbColEq _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in
              let _x_i3 = o#module_expr _x_i3 in MbColEq _x _x_i1 _x_i2 _x_i3
          | MbCol _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_type _x_i2 in MbCol _x _x_i1 _x_i2
          | MbAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in MbAnt _x _x_i1 ];
        method meta_option :
          ! 'a 'a_out.
            ('self_type -> 'a -> 'a_out) ->
              meta_option 'a -> meta_option 'a_out =
          fun _f_a ->
            fun
            [ ONone -> ONone
            | OSome _x -> let _x = _f_a o _x in OSome _x
            | OAnt _x -> let _x = o#string _x in OAnt _x ];
        method meta_list :
          ! 'a 'a_out.
            ('self_type -> 'a -> 'a_out) -> meta_list 'a -> meta_list 'a_out =
          fun _f_a ->
            fun
            [ LNil -> LNil
            | LCons _x _x_i1 ->
                let _x = _f_a o _x in
                let _x_i1 = o#meta_list _f_a _x_i1 in LCons _x _x_i1
            | LAnt _x -> let _x = o#string _x in LAnt _x ];
        method meta_bool : meta_bool -> meta_bool =
          fun
          [ BTrue -> BTrue
          | BFalse -> BFalse
          | BAnt _x -> let _x = o#string _x in BAnt _x ];
        method match_case : match_case -> match_case =
          fun
          [ McNil _x -> let _x = o#loc _x in McNil _x
          | McOr _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#match_case _x_i1 in
              let _x_i2 = o#match_case _x_i2 in McOr _x _x_i1 _x_i2
          | McArr _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#expr _x_i2 in
              let _x_i3 = o#expr _x_i3 in McArr _x _x_i1 _x_i2 _x_i3
          | McAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in McAnt _x _x_i1 ];
        method loc : loc -> loc = o#unknown;
        method ident : ident -> ident =
          fun
          [ IdAcc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in
              let _x_i2 = o#ident _x_i2 in IdAcc _x _x_i1 _x_i2
          | IdApp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in
              let _x_i2 = o#ident _x_i2 in IdApp _x _x_i1 _x_i2
          | IdLid _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in IdLid _x _x_i1
          | IdUid _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in IdUid _x _x_i1
          | IdAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in IdAnt _x _x_i1 ];
        method expr : expr -> expr =
          fun
          [ ExNil _x -> let _x = o#loc _x in ExNil _x
          | ExId _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ident _x_i1 in ExId _x _x_i1
          | ExAcc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExAcc _x _x_i1 _x_i2
          | ExAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExAnt _x _x_i1
          | ExApp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExApp _x _x_i1 _x_i2
          | ExAre _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExAre _x _x_i1 _x_i2
          | ExArr _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in ExArr _x _x_i1
          | ExSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExSem _x _x_i1 _x_i2
          | ExAsf _x -> let _x = o#loc _x in ExAsf _x
          | ExAsr _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in ExAsr _x _x_i1
          | ExAss _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExAss _x _x_i1 _x_i2
          | ExChr _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExChr _x _x_i1
          | ExCoe _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in ExCoe _x _x_i1 _x_i2 _x_i3
          | ExFlo _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExFlo _x _x_i1
          | ExFor _x _x_i1 _x_i2 _x_i3 _x_i4 _x_i5 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#expr _x_i2 in
              let _x_i3 = o#expr _x_i3 in
              let _x_i4 = o#meta_bool _x_i4 in
              let _x_i5 = o#expr _x_i5
              in ExFor _x _x_i1 _x_i2 _x_i3 _x_i4 _x_i5
          | ExFun _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#match_case _x_i1 in ExFun _x _x_i1
          | ExIfe _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in
              let _x_i3 = o#expr _x_i3 in ExIfe _x _x_i1 _x_i2 _x_i3
          | ExInt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExInt _x _x_i1
          | ExInt32 _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExInt32 _x _x_i1
          | ExInt64 _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExInt64 _x _x_i1
          | ExNativeInt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExNativeInt _x _x_i1
          | ExLab _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExLab _x _x_i1 _x_i2
          | ExLaz _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in ExLaz _x _x_i1
          | ExLet _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#meta_bool _x_i1 in
              let _x_i2 = o#binding _x_i2 in
              let _x_i3 = o#expr _x_i3 in ExLet _x _x_i1 _x_i2 _x_i3
          | ExLmd _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#module_expr _x_i2 in
              let _x_i3 = o#expr _x_i3 in ExLmd _x _x_i1 _x_i2 _x_i3
          | ExMat _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#match_case _x_i2 in ExMat _x _x_i1 _x_i2
          | ExNew _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in ExNew _x _x_i1
          | ExObj _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#class_str_item _x_i2 in ExObj _x _x_i1 _x_i2
          | ExOlb _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExOlb _x _x_i1 _x_i2
          | ExOvr _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#rec_binding _x_i1 in ExOvr _x _x_i1
          | ExRec _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#rec_binding _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExRec _x _x_i1 _x_i2
          | ExSeq _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in ExSeq _x _x_i1
          | ExSnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#string _x_i2 in ExSnd _x _x_i1 _x_i2
          | ExSte _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExSte _x _x_i1 _x_i2
          | ExStr _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExStr _x _x_i1
          | ExTry _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#match_case _x_i2 in ExTry _x _x_i1 _x_i2
          | ExTup _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in ExTup _x _x_i1
          | ExCom _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExCom _x _x_i1 _x_i2
          | ExTyc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in ExTyc _x _x_i1 _x_i2
          | ExVrn _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in ExVrn _x _x_i1
          | ExWhi _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in ExWhi _x _x_i1 _x_i2 ];
        method ctyp : ctyp -> ctyp =
          fun
          [ TyNil _x -> let _x = o#loc _x in TyNil _x
          | TyAli _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyAli _x _x_i1 _x_i2
          | TyAny _x -> let _x = o#loc _x in TyAny _x
          | TyApp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyApp _x _x_i1 _x_i2
          | TyArr _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyArr _x _x_i1 _x_i2
          | TyCls _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ident _x_i1 in TyCls _x _x_i1
          | TyLab _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyLab _x _x_i1 _x_i2
          | TyId _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ident _x_i1 in TyId _x _x_i1
          | TyMan _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyMan _x _x_i1 _x_i2
          | TyDcl _x _x_i1 _x_i2 _x_i3 _x_i4 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#list (fun o -> o#ctyp) _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in
              let _x_i4 =
                o#list
                  (fun o (_x, _x_i1) ->
                     let _x = o#ctyp _x in
                     let _x_i1 = o#ctyp _x_i1 in (_x, _x_i1))
                  _x_i4
              in TyDcl _x _x_i1 _x_i2 _x_i3 _x_i4
          | TyObj _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in TyObj _x _x_i1 _x_i2
          | TyOlb _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyOlb _x _x_i1 _x_i2
          | TyPol _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyPol _x _x_i1 _x_i2
          | TyQuo _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in TyQuo _x _x_i1
          | TyQuP _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in TyQuP _x _x_i1
          | TyQuM _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in TyQuM _x _x_i1
          | TyVrn _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in TyVrn _x _x_i1
          | TyRec _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in TyRec _x _x_i1
          | TyCol _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyCol _x _x_i1 _x_i2
          | TySem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TySem _x _x_i1 _x_i2
          | TyCom _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyCom _x _x_i1 _x_i2
          | TySum _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in TySum _x _x_i1
          | TyOf _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyOf _x _x_i1 _x_i2
          | TyAnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyAnd _x _x_i1 _x_i2
          | TyOr _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyOr _x _x_i1 _x_i2
          | TyPrv _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in TyPrv _x _x_i1
          | TyMut _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in TyMut _x _x_i1
          | TyTup _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#ctyp _x_i1 in TyTup _x _x_i1
          | TySta _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TySta _x _x_i1 _x_i2
          | TyVrnEq _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in TyVrnEq _x _x_i1
          | TyVrnSup _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in TyVrnSup _x _x_i1
          | TyVrnInf _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in TyVrnInf _x _x_i1
          | TyVrnInfSup _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyVrnInfSup _x _x_i1 _x_i2
          | TyAmp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyAmp _x _x_i1 _x_i2
          | TyOfAmp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in TyOfAmp _x _x_i1 _x_i2
          | TyAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in TyAnt _x _x_i1 ];
        method class_type : class_type -> class_type =
          fun
          [ CtNil _x -> let _x = o#loc _x in CtNil _x
          | CtCon _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#meta_bool _x_i1 in
              let _x_i2 = o#ident _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in CtCon _x _x_i1 _x_i2 _x_i3
          | CtFun _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#class_type _x_i2 in CtFun _x _x_i1 _x_i2
          | CtSig _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#class_sig_item _x_i2 in CtSig _x _x_i1 _x_i2
          | CtAnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in
              let _x_i2 = o#class_type _x_i2 in CtAnd _x _x_i1 _x_i2
          | CtCol _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in
              let _x_i2 = o#class_type _x_i2 in CtCol _x _x_i1 _x_i2
          | CtEq _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in
              let _x_i2 = o#class_type _x_i2 in CtEq _x _x_i1 _x_i2
          | CtAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in CtAnt _x _x_i1 ];
        method class_str_item : class_str_item -> class_str_item =
          fun
          [ CrNil _x -> let _x = o#loc _x in CrNil _x
          | CrSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_str_item _x_i1 in
              let _x_i2 = o#class_str_item _x_i2 in CrSem _x _x_i1 _x_i2
          | CrCtr _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in CrCtr _x _x_i1 _x_i2
          | CrInh _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_expr _x_i1 in
              let _x_i2 = o#string _x_i2 in CrInh _x _x_i1 _x_i2
          | CrIni _x _x_i1 ->
              let _x = o#loc _x in let _x_i1 = o#expr _x_i1 in CrIni _x _x_i1
          | CrMth _x _x_i1 _x_i2 _x_i3 _x_i4 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#expr _x_i3 in
              let _x_i4 = o#ctyp _x_i4 in CrMth _x _x_i1 _x_i2 _x_i3 _x_i4
          | CrVal _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#expr _x_i3 in CrVal _x _x_i1 _x_i2 _x_i3
          | CrVir _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in CrVir _x _x_i1 _x_i2 _x_i3
          | CrVvr _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in CrVvr _x _x_i1 _x_i2 _x_i3
          | CrAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in CrAnt _x _x_i1 ];
        method class_sig_item : class_sig_item -> class_sig_item =
          fun
          [ CgNil _x -> let _x = o#loc _x in CgNil _x
          | CgCtr _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#ctyp _x_i1 in
              let _x_i2 = o#ctyp _x_i2 in CgCtr _x _x_i1 _x_i2
          | CgSem _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_sig_item _x_i1 in
              let _x_i2 = o#class_sig_item _x_i2 in CgSem _x _x_i1 _x_i2
          | CgInh _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_type _x_i1 in CgInh _x _x_i1
          | CgMth _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in CgMth _x _x_i1 _x_i2 _x_i3
          | CgVal _x _x_i1 _x_i2 _x_i3 _x_i4 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#meta_bool _x_i3 in
              let _x_i4 = o#ctyp _x_i4 in CgVal _x _x_i1 _x_i2 _x_i3 _x_i4
          | CgVir _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in
              let _x_i2 = o#meta_bool _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in CgVir _x _x_i1 _x_i2 _x_i3
          | CgAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in CgAnt _x _x_i1 ];
        method class_expr : class_expr -> class_expr =
          fun
          [ CeNil _x -> let _x = o#loc _x in CeNil _x
          | CeApp _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_expr _x_i1 in
              let _x_i2 = o#expr _x_i2 in CeApp _x _x_i1 _x_i2
          | CeCon _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#meta_bool _x_i1 in
              let _x_i2 = o#ident _x_i2 in
              let _x_i3 = o#ctyp _x_i3 in CeCon _x _x_i1 _x_i2 _x_i3
          | CeFun _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#class_expr _x_i2 in CeFun _x _x_i1 _x_i2
          | CeLet _x _x_i1 _x_i2 _x_i3 ->
              let _x = o#loc _x in
              let _x_i1 = o#meta_bool _x_i1 in
              let _x_i2 = o#binding _x_i2 in
              let _x_i3 = o#class_expr _x_i3 in CeLet _x _x_i1 _x_i2 _x_i3
          | CeStr _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#class_str_item _x_i2 in CeStr _x _x_i1 _x_i2
          | CeTyc _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_expr _x_i1 in
              let _x_i2 = o#class_type _x_i2 in CeTyc _x _x_i1 _x_i2
          | CeAnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_expr _x_i1 in
              let _x_i2 = o#class_expr _x_i2 in CeAnd _x _x_i1 _x_i2
          | CeEq _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#class_expr _x_i1 in
              let _x_i2 = o#class_expr _x_i2 in CeEq _x _x_i1 _x_i2
          | CeAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in CeAnt _x _x_i1 ];
        method binding : binding -> binding =
          fun
          [ BiNil _x -> let _x = o#loc _x in BiNil _x
          | BiAnd _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#binding _x_i1 in
              let _x_i2 = o#binding _x_i2 in BiAnd _x _x_i1 _x_i2
          | BiEq _x _x_i1 _x_i2 ->
              let _x = o#loc _x in
              let _x_i1 = o#patt _x_i1 in
              let _x_i2 = o#expr _x_i2 in BiEq _x _x_i1 _x_i2
          | BiAnt _x _x_i1 ->
              let _x = o#loc _x in
              let _x_i1 = o#string _x_i1 in BiAnt _x _x_i1 ];
        method unknown : ! 'a. 'a -> 'a = fun x -> x;
      end;
    class fold =
      object ((o : 'self_type))
        method string : string -> 'self_type = o#unknown;
        method list :
          ! 'a. ('self_type -> 'a -> 'self_type) -> list 'a -> 'self_type =
          fun _f_a ->
            fun
            [ [] -> o
            | [ _x :: _x_i1 ] ->
                let o = _f_a o _x in let o = o#list _f_a _x_i1 in o ];
        method with_constr : with_constr -> 'self_type =
          fun
          [ WcNil _x -> let o = o#loc _x in o
          | WcTyp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | WcMod _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ident _x_i1 in let o = o#ident _x_i2 in o
          | WcAnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#with_constr _x_i1 in let o = o#with_constr _x_i2 in o
          | WcAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method str_item : str_item -> 'self_type =
          fun
          [ StNil _x -> let o = o#loc _x in o
          | StCls _x _x_i1 ->
              let o = o#loc _x in let o = o#class_expr _x_i1 in o
          | StClt _x _x_i1 ->
              let o = o#loc _x in let o = o#class_type _x_i1 in o
          | StSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#str_item _x_i1 in let o = o#str_item _x_i2 in o
          | StDir _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#expr _x_i2 in o
          | StExc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in
              let o = o#meta_option (fun o -> o#ident) _x_i2 in o
          | StExp _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | StExt _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#ctyp _x_i2 in
              let o = o#meta_list (fun o -> o#string) _x_i3 in o
          | StInc _x _x_i1 ->
              let o = o#loc _x in let o = o#module_expr _x_i1 in o
          | StMod _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#module_expr _x_i2 in o
          | StRecMod _x _x_i1 ->
              let o = o#loc _x in let o = o#module_binding _x_i1 in o
          | StMty _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#module_type _x_i2 in o
          | StOpn _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | StTyp _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | StVal _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#meta_bool _x_i1 in let o = o#binding _x_i2 in o
          | StAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method sig_item : sig_item -> 'self_type =
          fun
          [ SgNil _x -> let o = o#loc _x in o
          | SgCls _x _x_i1 ->
              let o = o#loc _x in let o = o#class_type _x_i1 in o
          | SgClt _x _x_i1 ->
              let o = o#loc _x in let o = o#class_type _x_i1 in o
          | SgSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#sig_item _x_i1 in let o = o#sig_item _x_i2 in o
          | SgDir _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#expr _x_i2 in o
          | SgExc _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | SgExt _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#ctyp _x_i2 in
              let o = o#meta_list (fun o -> o#string) _x_i3 in o
          | SgInc _x _x_i1 ->
              let o = o#loc _x in let o = o#module_type _x_i1 in o
          | SgMod _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#module_type _x_i2 in o
          | SgRecMod _x _x_i1 ->
              let o = o#loc _x in let o = o#module_binding _x_i1 in o
          | SgMty _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#module_type _x_i2 in o
          | SgOpn _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | SgTyp _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | SgVal _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#ctyp _x_i2 in o
          | SgAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method rec_binding : rec_binding -> 'self_type =
          fun
          [ RbNil _x -> let o = o#loc _x in o
          | RbSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#rec_binding _x_i1 in let o = o#rec_binding _x_i2 in o
          | RbEq _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ident _x_i1 in let o = o#expr _x_i2 in o
          | RbAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method patt : patt -> 'self_type =
          fun
          [ PaNil _x -> let o = o#loc _x in o
          | PaId _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | PaAli _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#patt _x_i2 in o
          | PaAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | PaAny _x -> let o = o#loc _x in o
          | PaApp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#patt _x_i2 in o
          | PaArr _x _x_i1 -> let o = o#loc _x in let o = o#patt _x_i1 in o
          | PaCom _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#patt _x_i2 in o
          | PaSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#patt _x_i2 in o
          | PaChr _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | PaInt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | PaInt32 _x _x_i1 ->
              let o = o#loc _x in let o = o#string _x_i1 in o
          | PaInt64 _x _x_i1 ->
              let o = o#loc _x in let o = o#string _x_i1 in o
          | PaNativeInt _x _x_i1 ->
              let o = o#loc _x in let o = o#string _x_i1 in o
          | PaFlo _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | PaLab _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#patt _x_i2 in o
          | PaOlb _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#patt _x_i2 in o
          | PaOlbi _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#patt _x_i2 in let o = o#expr _x_i3 in o
          | PaOrp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#patt _x_i2 in o
          | PaRng _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#patt _x_i2 in o
          | PaRec _x _x_i1 -> let o = o#loc _x in let o = o#patt _x_i1 in o
          | PaEq _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ident _x_i1 in let o = o#patt _x_i2 in o
          | PaStr _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | PaTup _x _x_i1 -> let o = o#loc _x in let o = o#patt _x_i1 in o
          | PaTyc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#ctyp _x_i2 in o
          | PaTyp _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | PaVrn _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | PaLaz _x _x_i1 -> let o = o#loc _x in let o = o#patt _x_i1 in o ];
        method module_type : module_type -> 'self_type =
          fun
          [ MtNil _x -> let o = o#loc _x in o
          | MtId _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | MtFun _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#module_type _x_i2 in let o = o#module_type _x_i3 in o
          | MtQuo _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | MtSig _x _x_i1 ->
              let o = o#loc _x in let o = o#sig_item _x_i1 in o
          | MtWit _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#module_type _x_i1 in let o = o#with_constr _x_i2 in o
          | MtAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method module_expr : module_expr -> 'self_type =
          fun
          [ MeNil _x -> let o = o#loc _x in o
          | MeId _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | MeApp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#module_expr _x_i1 in let o = o#module_expr _x_i2 in o
          | MeFun _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#module_type _x_i2 in let o = o#module_expr _x_i3 in o
          | MeStr _x _x_i1 ->
              let o = o#loc _x in let o = o#str_item _x_i1 in o
          | MeTyc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#module_expr _x_i1 in let o = o#module_type _x_i2 in o
          | MeAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method module_binding : module_binding -> 'self_type =
          fun
          [ MbNil _x -> let o = o#loc _x in o
          | MbAnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#module_binding _x_i1 in
              let o = o#module_binding _x_i2 in o
          | MbColEq _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#module_type _x_i2 in let o = o#module_expr _x_i3 in o
          | MbCol _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#module_type _x_i2 in o
          | MbAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method meta_option :
          ! 'a.
            ('self_type -> 'a -> 'self_type) -> meta_option 'a -> 'self_type =
          fun _f_a ->
            fun
            [ ONone -> o
            | OSome _x -> let o = _f_a o _x in o
            | OAnt _x -> let o = o#string _x in o ];
        method meta_list :
          ! 'a.
            ('self_type -> 'a -> 'self_type) -> meta_list 'a -> 'self_type =
          fun _f_a ->
            fun
            [ LNil -> o
            | LCons _x _x_i1 ->
                let o = _f_a o _x in let o = o#meta_list _f_a _x_i1 in o
            | LAnt _x -> let o = o#string _x in o ];
        method meta_bool : meta_bool -> 'self_type =
          fun
          [ BTrue -> o
          | BFalse -> o
          | BAnt _x -> let o = o#string _x in o ];
        method match_case : match_case -> 'self_type =
          fun
          [ McNil _x -> let o = o#loc _x in o
          | McOr _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#match_case _x_i1 in let o = o#match_case _x_i2 in o
          | McArr _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in
              let o = o#expr _x_i2 in let o = o#expr _x_i3 in o
          | McAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method loc : loc -> 'self_type = o#unknown;
        method ident : ident -> 'self_type =
          fun
          [ IdAcc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ident _x_i1 in let o = o#ident _x_i2 in o
          | IdApp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ident _x_i1 in let o = o#ident _x_i2 in o
          | IdLid _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | IdUid _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | IdAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method expr : expr -> 'self_type =
          fun
          [ ExNil _x -> let o = o#loc _x in o
          | ExId _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | ExAcc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | ExApp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExAre _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExArr _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | ExSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExAsf _x -> let o = o#loc _x in o
          | ExAsr _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | ExAss _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExChr _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | ExCoe _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in
              let o = o#ctyp _x_i2 in let o = o#ctyp _x_i3 in o
          | ExFlo _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | ExFor _x _x_i1 _x_i2 _x_i3 _x_i4 _x_i5 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#expr _x_i2 in
              let o = o#expr _x_i3 in
              let o = o#meta_bool _x_i4 in let o = o#expr _x_i5 in o
          | ExFun _x _x_i1 ->
              let o = o#loc _x in let o = o#match_case _x_i1 in o
          | ExIfe _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in
              let o = o#expr _x_i2 in let o = o#expr _x_i3 in o
          | ExInt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | ExInt32 _x _x_i1 ->
              let o = o#loc _x in let o = o#string _x_i1 in o
          | ExInt64 _x _x_i1 ->
              let o = o#loc _x in let o = o#string _x_i1 in o
          | ExNativeInt _x _x_i1 ->
              let o = o#loc _x in let o = o#string _x_i1 in o
          | ExLab _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#expr _x_i2 in o
          | ExLaz _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | ExLet _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#meta_bool _x_i1 in
              let o = o#binding _x_i2 in let o = o#expr _x_i3 in o
          | ExLmd _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#module_expr _x_i2 in let o = o#expr _x_i3 in o
          | ExMat _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#match_case _x_i2 in o
          | ExNew _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | ExObj _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#class_str_item _x_i2 in o
          | ExOlb _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#expr _x_i2 in o
          | ExOvr _x _x_i1 ->
              let o = o#loc _x in let o = o#rec_binding _x_i1 in o
          | ExRec _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#rec_binding _x_i1 in let o = o#expr _x_i2 in o
          | ExSeq _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | ExSnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#string _x_i2 in o
          | ExSte _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExStr _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | ExTry _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#match_case _x_i2 in o
          | ExTup _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | ExCom _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o
          | ExTyc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#ctyp _x_i2 in o
          | ExVrn _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | ExWhi _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#expr _x_i1 in let o = o#expr _x_i2 in o ];
        method ctyp : ctyp -> 'self_type =
          fun
          [ TyNil _x -> let o = o#loc _x in o
          | TyAli _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyAny _x -> let o = o#loc _x in o
          | TyApp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyArr _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyCls _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | TyLab _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#ctyp _x_i2 in o
          | TyId _x _x_i1 -> let o = o#loc _x in let o = o#ident _x_i1 in o
          | TyMan _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyDcl _x _x_i1 _x_i2 _x_i3 _x_i4 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#list (fun o -> o#ctyp) _x_i2 in
              let o = o#ctyp _x_i3 in
              let o =
                o#list
                  (fun o (_x, _x_i1) ->
                     let o = o#ctyp _x in let o = o#ctyp _x_i1 in o)
                  _x_i4
              in o
          | TyObj _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#meta_bool _x_i2 in o
          | TyOlb _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in let o = o#ctyp _x_i2 in o
          | TyPol _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyQuo _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | TyQuP _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | TyQuM _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | TyVrn _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o
          | TyRec _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyCol _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TySem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyCom _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TySum _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyOf _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyAnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyOr _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyPrv _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyMut _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyTup _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TySta _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyVrnEq _x _x_i1 -> let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyVrnSup _x _x_i1 ->
              let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyVrnInf _x _x_i1 ->
              let o = o#loc _x in let o = o#ctyp _x_i1 in o
          | TyVrnInfSup _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyAmp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyOfAmp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | TyAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method class_type : class_type -> 'self_type =
          fun
          [ CtNil _x -> let o = o#loc _x in o
          | CtCon _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#meta_bool _x_i1 in
              let o = o#ident _x_i2 in let o = o#ctyp _x_i3 in o
          | CtFun _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#class_type _x_i2 in o
          | CtSig _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#class_sig_item _x_i2 in o
          | CtAnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_type _x_i1 in let o = o#class_type _x_i2 in o
          | CtCol _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_type _x_i1 in let o = o#class_type _x_i2 in o
          | CtEq _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_type _x_i1 in let o = o#class_type _x_i2 in o
          | CtAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method class_str_item : class_str_item -> 'self_type =
          fun
          [ CrNil _x -> let o = o#loc _x in o
          | CrSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_str_item _x_i1 in
              let o = o#class_str_item _x_i2 in o
          | CrCtr _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | CrInh _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_expr _x_i1 in let o = o#string _x_i2 in o
          | CrIni _x _x_i1 -> let o = o#loc _x in let o = o#expr _x_i1 in o
          | CrMth _x _x_i1 _x_i2 _x_i3 _x_i4 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in
              let o = o#expr _x_i3 in let o = o#ctyp _x_i4 in o
          | CrVal _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in let o = o#expr _x_i3 in o
          | CrVir _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in let o = o#ctyp _x_i3 in o
          | CrVvr _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in let o = o#ctyp _x_i3 in o
          | CrAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method class_sig_item : class_sig_item -> 'self_type =
          fun
          [ CgNil _x -> let o = o#loc _x in o
          | CgCtr _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#ctyp _x_i1 in let o = o#ctyp _x_i2 in o
          | CgSem _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_sig_item _x_i1 in
              let o = o#class_sig_item _x_i2 in o
          | CgInh _x _x_i1 ->
              let o = o#loc _x in let o = o#class_type _x_i1 in o
          | CgMth _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in let o = o#ctyp _x_i3 in o
          | CgVal _x _x_i1 _x_i2 _x_i3 _x_i4 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in
              let o = o#meta_bool _x_i3 in let o = o#ctyp _x_i4 in o
          | CgVir _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#string _x_i1 in
              let o = o#meta_bool _x_i2 in let o = o#ctyp _x_i3 in o
          | CgAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method class_expr : class_expr -> 'self_type =
          fun
          [ CeNil _x -> let o = o#loc _x in o
          | CeApp _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_expr _x_i1 in let o = o#expr _x_i2 in o
          | CeCon _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#meta_bool _x_i1 in
              let o = o#ident _x_i2 in let o = o#ctyp _x_i3 in o
          | CeFun _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#class_expr _x_i2 in o
          | CeLet _x _x_i1 _x_i2 _x_i3 ->
              let o = o#loc _x in
              let o = o#meta_bool _x_i1 in
              let o = o#binding _x_i2 in let o = o#class_expr _x_i3 in o
          | CeStr _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#class_str_item _x_i2 in o
          | CeTyc _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_expr _x_i1 in let o = o#class_type _x_i2 in o
          | CeAnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_expr _x_i1 in let o = o#class_expr _x_i2 in o
          | CeEq _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#class_expr _x_i1 in let o = o#class_expr _x_i2 in o
          | CeAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method binding : binding -> 'self_type =
          fun
          [ BiNil _x -> let o = o#loc _x in o
          | BiAnd _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#binding _x_i1 in let o = o#binding _x_i2 in o
          | BiEq _x _x_i1 _x_i2 ->
              let o = o#loc _x in
              let o = o#patt _x_i1 in let o = o#expr _x_i2 in o
          | BiAnt _x _x_i1 -> let o = o#loc _x in let o = o#string _x_i1 in o ];
        method unknown : ! 'a. 'a -> 'self_type = fun _ -> o;
      end;
    value map_expr f =
      object inherit map as super; method expr = fun x -> f (super#expr x);
      end;
    value map_patt f =
      object inherit map as super; method patt = fun x -> f (super#patt x);
      end;
    value map_ctyp f =
      object inherit map as super; method ctyp = fun x -> f (super#ctyp x);
      end;
    value map_str_item f =
      object
        inherit map as super;
        method str_item = fun x -> f (super#str_item x);
      end;
    value map_sig_item f =
      object
        inherit map as super;
        method sig_item = fun x -> f (super#sig_item x);
      end;
    value map_loc f =
      object inherit map as super; method loc = fun x -> f (super#loc x); end;
  end;

