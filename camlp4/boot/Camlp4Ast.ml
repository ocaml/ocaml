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
        include Sig.MakeCamlp4Ast(Loc);
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
      | Ast.PaAli _ x y -> (is_irrefut_patt x) && (is_irrefut_patt y)
      | Ast.PaRec _ p -> is_irrefut_patt p
      | Ast.PaEq _ (Ast.IdLid _ _) p -> is_irrefut_patt p
      | Ast.PaSem _ p1 p2 -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
      | Ast.PaCom _ p1 p2 -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
      | Ast.PaTyc _ p _ -> is_irrefut_patt p
      | Ast.PaTup _ pl -> is_irrefut_patt pl
      | Ast.PaOlb _ _ (Ast.PaNil _) -> True
      | Ast.PaOlb _ _ p -> is_irrefut_patt p
      | Ast.PaOlbi _ _ p _ -> is_irrefut_patt p
      | Ast.PaLab _ _ (Ast.PaNil _) -> True
      | Ast.PaLab _ _ p -> is_irrefut_patt p
      | _ -> False ];
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
            (** The first location is where to put the returned pattern.
          Generally it's _loc to match with <:patt< ... >> quotations.
          The second location is the one to treat. *)
            value meta_loc_patt : Loc.t -> Loc.t -> Ast.patt;
            (** The first location is where to put the returned expression.
          Generally it's _loc to match with <:expr< ... >> quotations.
          The second location is the one to treat. *)
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
            value meta_acc_Loc_t = meta_loc_expr;
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_expr _loc x2)
                  | Ast.BiAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "BiAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_binding _loc x1))
                        (meta_binding _loc x2)
                  | Ast.BiNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "BiNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeTyc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CeStr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeStr")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.CeNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CeNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                                 (meta_acc_Loc_t _loc x0))
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
                                    (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CgInh x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "CgInh")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.CgSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_sig_item _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CgCtr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgCtr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CgNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CgNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                                 (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                                    (meta_acc_Loc_t _loc x0))
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
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.CrInh x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrInh")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.CrCtr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrCtr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CrSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_str_item _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.CrNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CrNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtCol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtCol")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtSig x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtSig")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CtFun x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtFun")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_ident _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CtNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CtNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAmp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAmp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInfSup x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyVrnInfSup")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInf x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnInf")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnSup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnSup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnEq x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnEq")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TySta x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySta")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyTup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyTup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyMut x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyMut")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyPrv x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyPrv")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyOr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOf x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOf")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySum x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TySum")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyCom x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCom")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCol")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyRec x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyRec")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuM x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuM")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuP x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuP")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuo")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyPol x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyPol")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOlb x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOlb")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyObj x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyObj")))
                              (meta_acc_Loc_t _loc x0))
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
                                    (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyLab x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyLab")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCls x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyCls")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyArr x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyArr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyApp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAny x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyAny")))
                        (meta_acc_Loc_t _loc x0)
                  | Ast.TyAli x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAli")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyNil")))
                        (meta_acc_Loc_t _loc x0) ]
                and meta_expr _loc =
                  fun
                  [ Ast.ExWhi x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExWhi")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExVrn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExVrn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTyc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.ExCom x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExCom")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExTup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExTup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExTry x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTry")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.ExStr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExStr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExSte x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSte")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExSnd x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.ExSeq x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExSeq")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExRec x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExRec")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExOvr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExOvr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_rec_binding _loc x1)
                  | Ast.ExOlb x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExOlb")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExObj x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExObj")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.ExNew x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNew")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExMat x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExMat")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_binding _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExLaz x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExLaz")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExLab x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExLab")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExNativeInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNativeInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt64 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt64")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt32 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt32")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExIfe x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExIfe")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_expr _loc x1))
                           (meta_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExFun x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExFun")))
                           (meta_acc_Loc_t _loc x0))
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
                                       (meta_acc_Loc_t _loc x0))
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
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExCoe x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExCoe")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_expr _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.ExChr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExChr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExAss x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAss")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExAsr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExAsr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAsf x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExAsf")))
                        (meta_acc_Loc_t _loc x0)
                  | Ast.ExSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExArr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExArr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAre x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAre")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExApp")))
                              (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExNil")))
                        (meta_acc_Loc_t _loc x0) ]
                and meta_ident _loc =
                  fun
                  [ Ast.IdAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.IdUid x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdUid")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdLid x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdLid")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdApp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.IdAcc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdAcc")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_match_case _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.McNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "McNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_binding _loc x1))
                        (meta_module_binding _loc x2)
                  | Ast.MbNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MbNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.MeStr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeStr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_str_item _loc x1)
                  | Ast.MeFun x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MeFun")))
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.MeId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MeNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MeNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_type _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.MtSig x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtSig")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_sig_item _loc x1)
                  | Ast.MtQuo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtQuo")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.MtFun x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MtFun")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_type _loc x3)
                  | Ast.MtId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MtNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MtNil")))
                        (meta_acc_Loc_t _loc x0) ]
                and meta_patt _loc =
                  fun
                  [ Ast.PaVrn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaVrn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaTyp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTyp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaTyc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaTyc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.PaTup x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaStr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaStr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaEq x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaEq")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaRec x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaRec")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaRng x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaRng")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaOrp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaOrp")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaLab x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaLab")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaFlo x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaFlo")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaNativeInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaNativeInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt64 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt64")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt32 x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt32")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaChr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaChr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaCom x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaCom")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaArr x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaArr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaApp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaApp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaAny x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaAny")))
                        (meta_acc_Loc_t _loc x0)
                  | Ast.PaAnt x0 x1 -> Ast.ExAnt x0 x1
                  | Ast.PaAli x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaAli")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaId x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_expr _loc x2)
                  | Ast.RbSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "RbSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_rec_binding _loc x2)
                  | Ast.RbNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "RbNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.SgTyp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgTyp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgOpn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgOpn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.SgMty x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMty")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgRecMod x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgRecMod")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.SgMod x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMod")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgInc x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgInc")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_type _loc x1)
                  | Ast.SgExt x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "SgExt")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.SgExc x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgExc")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgDir x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgDir")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.SgSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_sig_item _loc x1))
                        (meta_sig_item _loc x2)
                  | Ast.SgClt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgClt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgCls x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgCls")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "SgNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_meta_bool _loc x1))
                        (meta_binding _loc x2)
                  | Ast.StTyp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StTyp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.StOpn x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StOpn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.StMty x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMty")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.StRecMod x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StRecMod")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.StMod x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMod")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.StInc x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StInc")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_expr _loc x1)
                  | Ast.StExt x0 x1 x2 x3 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExApp _loc
                                 (Ast.ExId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "StExt")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.StExp x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StExp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.StExc x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StExc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_meta_option meta_ident _loc x2)
                  | Ast.StDir x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StDir")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.StSem x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_str_item _loc x1))
                        (meta_str_item _loc x2)
                  | Ast.StClt x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StClt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.StCls x0 x1 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StCls")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_expr _loc x1)
                  | Ast.StNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "StNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_with_constr _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.WcMod x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcMod")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.WcTyp x0 x1 x2 ->
                      Ast.ExApp _loc
                        (Ast.ExApp _loc
                           (Ast.ExApp _loc
                              (Ast.ExId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcTyp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.WcNil x0 ->
                      Ast.ExApp _loc
                        (Ast.ExId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "WcNil")))
                        (meta_acc_Loc_t _loc x0) ];
              end;
            value meta_acc_Loc_t = meta_loc_patt;
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_expr _loc x2)
                  | Ast.BiAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "BiAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_binding _loc x1))
                        (meta_binding _loc x2)
                  | Ast.BiNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "BiNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_expr _loc x2)
                  | Ast.CeTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeTyc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CeStr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CeStr")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.CeNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CeNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                                 (meta_acc_Loc_t _loc x0))
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
                                    (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_meta_bool _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CgInh x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "CgInh")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.CgSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_sig_item _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CgCtr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CgCtr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CgNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CgNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                                 (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                                    (meta_acc_Loc_t _loc x0))
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
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.CrInh x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrInh")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.CrCtr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrCtr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.CrSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CrSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_str_item _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.CrNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CrNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtCol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtCol")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_class_type _loc x1))
                        (meta_class_type _loc x2)
                  | Ast.CtSig x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtSig")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_class_sig_item _loc x2)
                  | Ast.CtFun x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "CtFun")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_ident _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.CtNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "CtNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAmp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAmp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInfSup x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyVrnInfSup")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyVrnInf x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnInf")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnSup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnSup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrnEq x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrnEq")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TySta x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySta")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyTup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyTup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyMut x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyMut")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyPrv x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyPrv")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyOr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOf x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOf")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySum x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TySum")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyCom x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCom")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TySem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TySem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyCol")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyRec x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyRec")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.TyVrn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyVrn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuM x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuM")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuP x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuP")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyQuo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyQuo")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.TyPol x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyPol")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyOlb x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyOlb")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyObj x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyObj")))
                              (meta_acc_Loc_t _loc x0))
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
                                    (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyLab x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyLab")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyCls x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "TyCls")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.TyArr x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyArr")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyApp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyAny x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyAny")))
                        (meta_acc_Loc_t _loc x0)
                  | Ast.TyAli x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "TyAli")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.TyNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "TyNil")))
                        (meta_acc_Loc_t _loc x0) ]
                and meta_expr _loc =
                  fun
                  [ Ast.ExWhi x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExWhi")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExVrn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExVrn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTyc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.ExCom x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExCom")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExTup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExTup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExTry x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExTry")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.ExStr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExStr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExSte x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSte")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExSnd x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSnd")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_string _loc x2)
                  | Ast.ExSeq x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExSeq")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExRec x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExRec")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExOvr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExOvr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_rec_binding _loc x1)
                  | Ast.ExOlb x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExOlb")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExObj x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExObj")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_class_str_item _loc x2)
                  | Ast.ExNew x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNew")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExMat x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExMat")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
                              (meta_meta_bool _loc x1))
                           (meta_binding _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExLaz x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExLaz")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExLab x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExLab")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExNativeInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExNativeInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt64 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt64")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt32 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt32")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExIfe x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExIfe")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_expr _loc x1))
                           (meta_expr _loc x2))
                        (meta_expr _loc x3)
                  | Ast.ExFun x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExFun")))
                           (meta_acc_Loc_t _loc x0))
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
                                       (meta_acc_Loc_t _loc x0))
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
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExCoe x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "ExCoe")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_expr _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_ctyp _loc x3)
                  | Ast.ExChr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExChr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.ExAss x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAss")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExAsr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExAsr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAsf x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExAsf")))
                        (meta_acc_Loc_t _loc x0)
                  | Ast.ExSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExArr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExArr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.ExAre x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExAre")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "ExApp")))
                              (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_expr _loc x1))
                        (meta_expr _loc x2)
                  | Ast.ExId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "ExId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.ExNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "ExNil")))
                        (meta_acc_Loc_t _loc x0) ]
                and meta_ident _loc =
                  fun
                  [ Ast.IdAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.IdUid x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdUid")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdLid x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "IdLid")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.IdApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdApp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.IdAcc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "IdAcc")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_match_case _loc x1))
                        (meta_match_case _loc x2)
                  | Ast.McNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "McNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_binding _loc x1))
                        (meta_module_binding _loc x2)
                  | Ast.MbNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MbNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.MeStr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeStr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_str_item _loc x1)
                  | Ast.MeFun x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MeFun")))
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_expr _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.MeId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MeId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MeNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MeNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_module_type _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.MtSig x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtSig")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_sig_item _loc x1)
                  | Ast.MtQuo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtQuo")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.MtFun x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "MtFun")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_module_type _loc x2))
                        (meta_module_type _loc x3)
                  | Ast.MtId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "MtId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.MtNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "MtNil")))
                        (meta_acc_Loc_t _loc x0) ]
                and meta_patt _loc =
                  fun
                  [ Ast.PaVrn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaVrn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaTyp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTyp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaTyc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaTyc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.PaTup x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaTup")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaStr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaStr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaEq x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaEq")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaRec x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaRec")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaRng x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaRng")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaOrp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaOrp")))
                              (meta_acc_Loc_t _loc x0))
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
                                 (meta_acc_Loc_t _loc x0))
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaLab x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaLab")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaFlo x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaFlo")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaNativeInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaNativeInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt64 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt64")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt32 x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt32")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaInt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaInt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaChr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaChr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_string _loc x1)
                  | Ast.PaSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaCom x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaCom")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaArr x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaArr")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_patt _loc x1)
                  | Ast.PaApp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaApp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaAny x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaAny")))
                        (meta_acc_Loc_t _loc x0)
                  | Ast.PaAnt x0 x1 -> Ast.PaAnt x0 x1
                  | Ast.PaAli x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "PaAli")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_patt _loc x1))
                        (meta_patt _loc x2)
                  | Ast.PaId x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "PaId")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.PaNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "PaNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_expr _loc x2)
                  | Ast.RbSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "RbSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_rec_binding _loc x1))
                        (meta_rec_binding _loc x2)
                  | Ast.RbNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "RbNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.SgTyp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgTyp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgOpn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgOpn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.SgMty x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMty")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgRecMod x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgRecMod")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.SgMod x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgMod")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.SgInc x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgInc")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_type _loc x1)
                  | Ast.SgExt x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "SgExt")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.SgExc x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgExc")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.SgDir x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgDir")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.SgSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "SgSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_sig_item _loc x1))
                        (meta_sig_item _loc x2)
                  | Ast.SgClt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgClt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgCls x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "SgCls")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.SgNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "SgNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_meta_bool _loc x1))
                        (meta_binding _loc x2)
                  | Ast.StTyp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StTyp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ctyp _loc x1)
                  | Ast.StOpn x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StOpn")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_ident _loc x1)
                  | Ast.StMty x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMty")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_type _loc x2)
                  | Ast.StRecMod x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StRecMod")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_binding _loc x1)
                  | Ast.StMod x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StMod")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_module_expr _loc x2)
                  | Ast.StInc x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StInc")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_module_expr _loc x1)
                  | Ast.StExt x0 x1 x2 x3 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaApp _loc
                                 (Ast.PaId _loc
                                    (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                       (Ast.IdUid _loc "StExt")))
                                 (meta_acc_Loc_t _loc x0))
                              (meta_string _loc x1))
                           (meta_ctyp _loc x2))
                        (meta_meta_list meta_string _loc x3)
                  | Ast.StExp x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StExp")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_expr _loc x1)
                  | Ast.StExc x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StExc")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_meta_option meta_ident _loc x2)
                  | Ast.StDir x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StDir")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_string _loc x1))
                        (meta_expr _loc x2)
                  | Ast.StSem x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "StSem")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_str_item _loc x1))
                        (meta_str_item _loc x2)
                  | Ast.StClt x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StClt")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_type _loc x1)
                  | Ast.StCls x0 x1 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaId _loc
                              (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                 (Ast.IdUid _loc "StCls")))
                           (meta_acc_Loc_t _loc x0))
                        (meta_class_expr _loc x1)
                  | Ast.StNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "StNil")))
                        (meta_acc_Loc_t _loc x0) ]
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
                              (meta_acc_Loc_t _loc x0))
                           (meta_with_constr _loc x1))
                        (meta_with_constr _loc x2)
                  | Ast.WcMod x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcMod")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ident _loc x1))
                        (meta_ident _loc x2)
                  | Ast.WcTyp x0 x1 x2 ->
                      Ast.PaApp _loc
                        (Ast.PaApp _loc
                           (Ast.PaApp _loc
                              (Ast.PaId _loc
                                 (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                                    (Ast.IdUid _loc "WcTyp")))
                              (meta_acc_Loc_t _loc x0))
                           (meta_ctyp _loc x1))
                        (meta_ctyp _loc x2)
                  | Ast.WcNil x0 ->
                      Ast.PaApp _loc
                        (Ast.PaId _loc
                           (Ast.IdAcc _loc (Ast.IdUid _loc "Ast")
                              (Ast.IdUid _loc "WcNil")))
                        (meta_acc_Loc_t _loc x0) ];
              end;
          end;
      end;
    class map =
      object (o)
        method string = fun x -> (x : string);
        method int = fun x -> (x : int);
        method float = fun x -> (x : float);
        method bool = fun x -> (x : bool);
        method list : ! 'a 'b. ('a -> 'b) -> list 'a -> list 'b = List.map;
        method option : ! 'a 'b. ('a -> 'b) -> option 'a -> option 'b =
          fun f -> fun [ None -> None | Some x -> Some (f x) ];
        method array : ! 'a 'b. ('a -> 'b) -> array 'a -> array 'b = Array.
          map;
        method ref : ! 'a 'b. ('a -> 'b) -> ref 'a -> ref 'b =
          fun f { val = x } -> { val = f x; };
        method _Loc_t : Loc.t -> Loc.t = fun x -> x;
        method with_constr : with_constr -> with_constr =
          fun
          [ WcNil _x0 -> WcNil (o#_Loc_t _x0)
          | WcTyp _x0 _x1 _x2 ->
              WcTyp (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | WcMod _x0 _x1 _x2 ->
              WcMod (o#_Loc_t _x0) (o#ident _x1) (o#ident _x2)
          | WcAnd _x0 _x1 _x2 ->
              WcAnd (o#_Loc_t _x0) (o#with_constr _x1) (o#with_constr _x2)
          | WcAnt _x0 _x1 -> WcAnt (o#_Loc_t _x0) (o#string _x1) ];
        method str_item : str_item -> str_item =
          fun
          [ StNil _x0 -> StNil (o#_Loc_t _x0)
          | StCls _x0 _x1 -> StCls (o#_Loc_t _x0) (o#class_expr _x1)
          | StClt _x0 _x1 -> StClt (o#_Loc_t _x0) (o#class_type _x1)
          | StSem _x0 _x1 _x2 ->
              StSem (o#_Loc_t _x0) (o#str_item _x1) (o#str_item _x2)
          | StDir _x0 _x1 _x2 ->
              StDir (o#_Loc_t _x0) (o#string _x1) (o#expr _x2)
          | StExc _x0 _x1 _x2 ->
              StExc (o#_Loc_t _x0) (o#ctyp _x1) (o#meta_option o#ident _x2)
          | StExp _x0 _x1 -> StExp (o#_Loc_t _x0) (o#expr _x1)
          | StExt _x0 _x1 _x2 _x3 ->
              StExt (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2)
                (o#meta_list o#string _x3)
          | StInc _x0 _x1 -> StInc (o#_Loc_t _x0) (o#module_expr _x1)
          | StMod _x0 _x1 _x2 ->
              StMod (o#_Loc_t _x0) (o#string _x1) (o#module_expr _x2)
          | StRecMod _x0 _x1 ->
              StRecMod (o#_Loc_t _x0) (o#module_binding _x1)
          | StMty _x0 _x1 _x2 ->
              StMty (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
          | StOpn _x0 _x1 -> StOpn (o#_Loc_t _x0) (o#ident _x1)
          | StTyp _x0 _x1 -> StTyp (o#_Loc_t _x0) (o#ctyp _x1)
          | StVal _x0 _x1 _x2 ->
              StVal (o#_Loc_t _x0) (o#meta_bool _x1) (o#binding _x2)
          | StAnt _x0 _x1 -> StAnt (o#_Loc_t _x0) (o#string _x1) ];
        method sig_item : sig_item -> sig_item =
          fun
          [ SgNil _x0 -> SgNil (o#_Loc_t _x0)
          | SgCls _x0 _x1 -> SgCls (o#_Loc_t _x0) (o#class_type _x1)
          | SgClt _x0 _x1 -> SgClt (o#_Loc_t _x0) (o#class_type _x1)
          | SgSem _x0 _x1 _x2 ->
              SgSem (o#_Loc_t _x0) (o#sig_item _x1) (o#sig_item _x2)
          | SgDir _x0 _x1 _x2 ->
              SgDir (o#_Loc_t _x0) (o#string _x1) (o#expr _x2)
          | SgExc _x0 _x1 -> SgExc (o#_Loc_t _x0) (o#ctyp _x1)
          | SgExt _x0 _x1 _x2 _x3 ->
              SgExt (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2)
                (o#meta_list o#string _x3)
          | SgInc _x0 _x1 -> SgInc (o#_Loc_t _x0) (o#module_type _x1)
          | SgMod _x0 _x1 _x2 ->
              SgMod (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
          | SgRecMod _x0 _x1 ->
              SgRecMod (o#_Loc_t _x0) (o#module_binding _x1)
          | SgMty _x0 _x1 _x2 ->
              SgMty (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
          | SgOpn _x0 _x1 -> SgOpn (o#_Loc_t _x0) (o#ident _x1)
          | SgTyp _x0 _x1 -> SgTyp (o#_Loc_t _x0) (o#ctyp _x1)
          | SgVal _x0 _x1 _x2 ->
              SgVal (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2)
          | SgAnt _x0 _x1 -> SgAnt (o#_Loc_t _x0) (o#string _x1) ];
        method rec_binding : rec_binding -> rec_binding =
          fun
          [ RbNil _x0 -> RbNil (o#_Loc_t _x0)
          | RbSem _x0 _x1 _x2 ->
              RbSem (o#_Loc_t _x0) (o#rec_binding _x1) (o#rec_binding _x2)
          | RbEq _x0 _x1 _x2 ->
              RbEq (o#_Loc_t _x0) (o#ident _x1) (o#expr _x2)
          | RbAnt _x0 _x1 -> RbAnt (o#_Loc_t _x0) (o#string _x1) ];
        method patt : patt -> patt =
          fun
          [ PaNil _x0 -> PaNil (o#_Loc_t _x0)
          | PaId _x0 _x1 -> PaId (o#_Loc_t _x0) (o#ident _x1)
          | PaAli _x0 _x1 _x2 ->
              PaAli (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaAnt _x0 _x1 -> PaAnt (o#_Loc_t _x0) (o#string _x1)
          | PaAny _x0 -> PaAny (o#_Loc_t _x0)
          | PaApp _x0 _x1 _x2 ->
              PaApp (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaArr _x0 _x1 -> PaArr (o#_Loc_t _x0) (o#patt _x1)
          | PaCom _x0 _x1 _x2 ->
              PaCom (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaSem _x0 _x1 _x2 ->
              PaSem (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaChr _x0 _x1 -> PaChr (o#_Loc_t _x0) (o#string _x1)
          | PaInt _x0 _x1 -> PaInt (o#_Loc_t _x0) (o#string _x1)
          | PaInt32 _x0 _x1 -> PaInt32 (o#_Loc_t _x0) (o#string _x1)
          | PaInt64 _x0 _x1 -> PaInt64 (o#_Loc_t _x0) (o#string _x1)
          | PaNativeInt _x0 _x1 -> PaNativeInt (o#_Loc_t _x0) (o#string _x1)
          | PaFlo _x0 _x1 -> PaFlo (o#_Loc_t _x0) (o#string _x1)
          | PaLab _x0 _x1 _x2 ->
              PaLab (o#_Loc_t _x0) (o#string _x1) (o#patt _x2)
          | PaOlb _x0 _x1 _x2 ->
              PaOlb (o#_Loc_t _x0) (o#string _x1) (o#patt _x2)
          | PaOlbi _x0 _x1 _x2 _x3 ->
              PaOlbi (o#_Loc_t _x0) (o#string _x1) (o#patt _x2) (o#expr _x3)
          | PaOrp _x0 _x1 _x2 ->
              PaOrp (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaRng _x0 _x1 _x2 ->
              PaRng (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaRec _x0 _x1 -> PaRec (o#_Loc_t _x0) (o#patt _x1)
          | PaEq _x0 _x1 _x2 ->
              PaEq (o#_Loc_t _x0) (o#ident _x1) (o#patt _x2)
          | PaStr _x0 _x1 -> PaStr (o#_Loc_t _x0) (o#string _x1)
          | PaTup _x0 _x1 -> PaTup (o#_Loc_t _x0) (o#patt _x1)
          | PaTyc _x0 _x1 _x2 ->
              PaTyc (o#_Loc_t _x0) (o#patt _x1) (o#ctyp _x2)
          | PaTyp _x0 _x1 -> PaTyp (o#_Loc_t _x0) (o#ident _x1)
          | PaVrn _x0 _x1 -> PaVrn (o#_Loc_t _x0) (o#string _x1) ];
        method module_type : module_type -> module_type =
          fun
          [ MtNil _x0 -> MtNil (o#_Loc_t _x0)
          | MtId _x0 _x1 -> MtId (o#_Loc_t _x0) (o#ident _x1)
          | MtFun _x0 _x1 _x2 _x3 ->
              MtFun (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
                (o#module_type _x3)
          | MtQuo _x0 _x1 -> MtQuo (o#_Loc_t _x0) (o#string _x1)
          | MtSig _x0 _x1 -> MtSig (o#_Loc_t _x0) (o#sig_item _x1)
          | MtWit _x0 _x1 _x2 ->
              MtWit (o#_Loc_t _x0) (o#module_type _x1) (o#with_constr _x2)
          | MtAnt _x0 _x1 -> MtAnt (o#_Loc_t _x0) (o#string _x1) ];
        method module_expr : module_expr -> module_expr =
          fun
          [ MeNil _x0 -> MeNil (o#_Loc_t _x0)
          | MeId _x0 _x1 -> MeId (o#_Loc_t _x0) (o#ident _x1)
          | MeApp _x0 _x1 _x2 ->
              MeApp (o#_Loc_t _x0) (o#module_expr _x1) (o#module_expr _x2)
          | MeFun _x0 _x1 _x2 _x3 ->
              MeFun (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
                (o#module_expr _x3)
          | MeStr _x0 _x1 -> MeStr (o#_Loc_t _x0) (o#str_item _x1)
          | MeTyc _x0 _x1 _x2 ->
              MeTyc (o#_Loc_t _x0) (o#module_expr _x1) (o#module_type _x2)
          | MeAnt _x0 _x1 -> MeAnt (o#_Loc_t _x0) (o#string _x1) ];
        method module_binding : module_binding -> module_binding =
          fun
          [ MbNil _x0 -> MbNil (o#_Loc_t _x0)
          | MbAnd _x0 _x1 _x2 ->
              MbAnd (o#_Loc_t _x0) (o#module_binding _x1)
                (o#module_binding _x2)
          | MbColEq _x0 _x1 _x2 _x3 ->
              MbColEq (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
                (o#module_expr _x3)
          | MbCol _x0 _x1 _x2 ->
              MbCol (o#_Loc_t _x0) (o#string _x1) (o#module_type _x2)
          | MbAnt _x0 _x1 -> MbAnt (o#_Loc_t _x0) (o#string _x1) ];
        method meta_option :
          ! 'a 'b. ('a -> 'b) -> meta_option 'a -> meta_option 'b =
          fun _f_a ->
            fun
            [ ONone -> ONone
            | OSome _x0 -> OSome (_f_a _x0)
            | OAnt _x0 -> OAnt (o#string _x0) ];
        method meta_list :
          ! 'a 'b. ('a -> 'b) -> meta_list 'a -> meta_list 'b =
          fun _f_a ->
            fun
            [ LNil -> LNil
            | LCons _x0 _x1 -> LCons (_f_a _x0) (o#meta_list _f_a _x1)
            | LAnt _x0 -> LAnt (o#string _x0) ];
        method meta_bool : meta_bool -> meta_bool =
          fun
          [ BTrue -> BTrue
          | BFalse -> BFalse
          | BAnt _x0 -> BAnt (o#string _x0) ];
        method match_case : match_case -> match_case =
          fun
          [ McNil _x0 -> McNil (o#_Loc_t _x0)
          | McOr _x0 _x1 _x2 ->
              McOr (o#_Loc_t _x0) (o#match_case _x1) (o#match_case _x2)
          | McArr _x0 _x1 _x2 _x3 ->
              McArr (o#_Loc_t _x0) (o#patt _x1) (o#expr _x2) (o#expr _x3)
          | McAnt _x0 _x1 -> McAnt (o#_Loc_t _x0) (o#string _x1) ];
        method ident : ident -> ident =
          fun
          [ IdAcc _x0 _x1 _x2 ->
              IdAcc (o#_Loc_t _x0) (o#ident _x1) (o#ident _x2)
          | IdApp _x0 _x1 _x2 ->
              IdApp (o#_Loc_t _x0) (o#ident _x1) (o#ident _x2)
          | IdLid _x0 _x1 -> IdLid (o#_Loc_t _x0) (o#string _x1)
          | IdUid _x0 _x1 -> IdUid (o#_Loc_t _x0) (o#string _x1)
          | IdAnt _x0 _x1 -> IdAnt (o#_Loc_t _x0) (o#string _x1) ];
        method expr : expr -> expr =
          fun
          [ ExNil _x0 -> ExNil (o#_Loc_t _x0)
          | ExId _x0 _x1 -> ExId (o#_Loc_t _x0) (o#ident _x1)
          | ExAcc _x0 _x1 _x2 ->
              ExAcc (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExAnt _x0 _x1 -> ExAnt (o#_Loc_t _x0) (o#string _x1)
          | ExApp _x0 _x1 _x2 ->
              ExApp (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExAre _x0 _x1 _x2 ->
              ExAre (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExArr _x0 _x1 -> ExArr (o#_Loc_t _x0) (o#expr _x1)
          | ExSem _x0 _x1 _x2 ->
              ExSem (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExAsf _x0 -> ExAsf (o#_Loc_t _x0)
          | ExAsr _x0 _x1 -> ExAsr (o#_Loc_t _x0) (o#expr _x1)
          | ExAss _x0 _x1 _x2 ->
              ExAss (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExChr _x0 _x1 -> ExChr (o#_Loc_t _x0) (o#string _x1)
          | ExCoe _x0 _x1 _x2 _x3 ->
              ExCoe (o#_Loc_t _x0) (o#expr _x1) (o#ctyp _x2) (o#ctyp _x3)
          | ExFlo _x0 _x1 -> ExFlo (o#_Loc_t _x0) (o#string _x1)
          | ExFor _x0 _x1 _x2 _x3 _x4 _x5 ->
              ExFor (o#_Loc_t _x0) (o#string _x1) (o#expr _x2) (o#expr _x3)
                (o#meta_bool _x4) (o#expr _x5)
          | ExFun _x0 _x1 -> ExFun (o#_Loc_t _x0) (o#match_case _x1)
          | ExIfe _x0 _x1 _x2 _x3 ->
              ExIfe (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2) (o#expr _x3)
          | ExInt _x0 _x1 -> ExInt (o#_Loc_t _x0) (o#string _x1)
          | ExInt32 _x0 _x1 -> ExInt32 (o#_Loc_t _x0) (o#string _x1)
          | ExInt64 _x0 _x1 -> ExInt64 (o#_Loc_t _x0) (o#string _x1)
          | ExNativeInt _x0 _x1 -> ExNativeInt (o#_Loc_t _x0) (o#string _x1)
          | ExLab _x0 _x1 _x2 ->
              ExLab (o#_Loc_t _x0) (o#string _x1) (o#expr _x2)
          | ExLaz _x0 _x1 -> ExLaz (o#_Loc_t _x0) (o#expr _x1)
          | ExLet _x0 _x1 _x2 _x3 ->
              ExLet (o#_Loc_t _x0) (o#meta_bool _x1) (o#binding _x2)
                (o#expr _x3)
          | ExLmd _x0 _x1 _x2 _x3 ->
              ExLmd (o#_Loc_t _x0) (o#string _x1) (o#module_expr _x2)
                (o#expr _x3)
          | ExMat _x0 _x1 _x2 ->
              ExMat (o#_Loc_t _x0) (o#expr _x1) (o#match_case _x2)
          | ExNew _x0 _x1 -> ExNew (o#_Loc_t _x0) (o#ident _x1)
          | ExObj _x0 _x1 _x2 ->
              ExObj (o#_Loc_t _x0) (o#patt _x1) (o#class_str_item _x2)
          | ExOlb _x0 _x1 _x2 ->
              ExOlb (o#_Loc_t _x0) (o#string _x1) (o#expr _x2)
          | ExOvr _x0 _x1 -> ExOvr (o#_Loc_t _x0) (o#rec_binding _x1)
          | ExRec _x0 _x1 _x2 ->
              ExRec (o#_Loc_t _x0) (o#rec_binding _x1) (o#expr _x2)
          | ExSeq _x0 _x1 -> ExSeq (o#_Loc_t _x0) (o#expr _x1)
          | ExSnd _x0 _x1 _x2 ->
              ExSnd (o#_Loc_t _x0) (o#expr _x1) (o#string _x2)
          | ExSte _x0 _x1 _x2 ->
              ExSte (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExStr _x0 _x1 -> ExStr (o#_Loc_t _x0) (o#string _x1)
          | ExTry _x0 _x1 _x2 ->
              ExTry (o#_Loc_t _x0) (o#expr _x1) (o#match_case _x2)
          | ExTup _x0 _x1 -> ExTup (o#_Loc_t _x0) (o#expr _x1)
          | ExCom _x0 _x1 _x2 ->
              ExCom (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExTyc _x0 _x1 _x2 ->
              ExTyc (o#_Loc_t _x0) (o#expr _x1) (o#ctyp _x2)
          | ExVrn _x0 _x1 -> ExVrn (o#_Loc_t _x0) (o#string _x1)
          | ExWhi _x0 _x1 _x2 ->
              ExWhi (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2) ];
        method ctyp : ctyp -> ctyp =
          fun
          [ TyNil _x0 -> TyNil (o#_Loc_t _x0)
          | TyAli _x0 _x1 _x2 ->
              TyAli (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyAny _x0 -> TyAny (o#_Loc_t _x0)
          | TyApp _x0 _x1 _x2 ->
              TyApp (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyArr _x0 _x1 _x2 ->
              TyArr (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyCls _x0 _x1 -> TyCls (o#_Loc_t _x0) (o#ident _x1)
          | TyLab _x0 _x1 _x2 ->
              TyLab (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2)
          | TyId _x0 _x1 -> TyId (o#_Loc_t _x0) (o#ident _x1)
          | TyMan _x0 _x1 _x2 ->
              TyMan (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyDcl _x0 _x1 _x2 _x3 _x4 ->
              TyDcl (o#_Loc_t _x0) (o#string _x1) (o#list o#ctyp _x2)
                (o#ctyp _x3)
                (o#list (fun (_x0, _x1) -> ((o#ctyp _x0), (o#ctyp _x1))) _x4)
          | TyObj _x0 _x1 _x2 ->
              TyObj (o#_Loc_t _x0) (o#ctyp _x1) (o#meta_bool _x2)
          | TyOlb _x0 _x1 _x2 ->
              TyOlb (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2)
          | TyPol _x0 _x1 _x2 ->
              TyPol (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyQuo _x0 _x1 -> TyQuo (o#_Loc_t _x0) (o#string _x1)
          | TyQuP _x0 _x1 -> TyQuP (o#_Loc_t _x0) (o#string _x1)
          | TyQuM _x0 _x1 -> TyQuM (o#_Loc_t _x0) (o#string _x1)
          | TyVrn _x0 _x1 -> TyVrn (o#_Loc_t _x0) (o#string _x1)
          | TyRec _x0 _x1 -> TyRec (o#_Loc_t _x0) (o#ctyp _x1)
          | TyCol _x0 _x1 _x2 ->
              TyCol (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TySem _x0 _x1 _x2 ->
              TySem (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyCom _x0 _x1 _x2 ->
              TyCom (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TySum _x0 _x1 -> TySum (o#_Loc_t _x0) (o#ctyp _x1)
          | TyOf _x0 _x1 _x2 -> TyOf (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyAnd _x0 _x1 _x2 ->
              TyAnd (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyOr _x0 _x1 _x2 -> TyOr (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyPrv _x0 _x1 -> TyPrv (o#_Loc_t _x0) (o#ctyp _x1)
          | TyMut _x0 _x1 -> TyMut (o#_Loc_t _x0) (o#ctyp _x1)
          | TyTup _x0 _x1 -> TyTup (o#_Loc_t _x0) (o#ctyp _x1)
          | TySta _x0 _x1 _x2 ->
              TySta (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyVrnEq _x0 _x1 -> TyVrnEq (o#_Loc_t _x0) (o#ctyp _x1)
          | TyVrnSup _x0 _x1 -> TyVrnSup (o#_Loc_t _x0) (o#ctyp _x1)
          | TyVrnInf _x0 _x1 -> TyVrnInf (o#_Loc_t _x0) (o#ctyp _x1)
          | TyVrnInfSup _x0 _x1 _x2 ->
              TyVrnInfSup (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyAmp _x0 _x1 _x2 ->
              TyAmp (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyOfAmp _x0 _x1 _x2 ->
              TyOfAmp (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | TyAnt _x0 _x1 -> TyAnt (o#_Loc_t _x0) (o#string _x1) ];
        method class_type : class_type -> class_type =
          fun
          [ CtNil _x0 -> CtNil (o#_Loc_t _x0)
          | CtCon _x0 _x1 _x2 _x3 ->
              CtCon (o#_Loc_t _x0) (o#meta_bool _x1) (o#ident _x2)
                (o#ctyp _x3)
          | CtFun _x0 _x1 _x2 ->
              CtFun (o#_Loc_t _x0) (o#ctyp _x1) (o#class_type _x2)
          | CtSig _x0 _x1 _x2 ->
              CtSig (o#_Loc_t _x0) (o#ctyp _x1) (o#class_sig_item _x2)
          | CtAnd _x0 _x1 _x2 ->
              CtAnd (o#_Loc_t _x0) (o#class_type _x1) (o#class_type _x2)
          | CtCol _x0 _x1 _x2 ->
              CtCol (o#_Loc_t _x0) (o#class_type _x1) (o#class_type _x2)
          | CtEq _x0 _x1 _x2 ->
              CtEq (o#_Loc_t _x0) (o#class_type _x1) (o#class_type _x2)
          | CtAnt _x0 _x1 -> CtAnt (o#_Loc_t _x0) (o#string _x1) ];
        method class_str_item : class_str_item -> class_str_item =
          fun
          [ CrNil _x0 -> CrNil (o#_Loc_t _x0)
          | CrSem _x0 _x1 _x2 ->
              CrSem (o#_Loc_t _x0) (o#class_str_item _x1)
                (o#class_str_item _x2)
          | CrCtr _x0 _x1 _x2 ->
              CrCtr (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | CrInh _x0 _x1 _x2 ->
              CrInh (o#_Loc_t _x0) (o#class_expr _x1) (o#string _x2)
          | CrIni _x0 _x1 -> CrIni (o#_Loc_t _x0) (o#expr _x1)
          | CrMth _x0 _x1 _x2 _x3 _x4 ->
              CrMth (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#expr _x3) (o#ctyp _x4)
          | CrVal _x0 _x1 _x2 _x3 ->
              CrVal (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#expr _x3)
          | CrVir _x0 _x1 _x2 _x3 ->
              CrVir (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#ctyp _x3)
          | CrVvr _x0 _x1 _x2 _x3 ->
              CrVvr (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#ctyp _x3)
          | CrAnt _x0 _x1 -> CrAnt (o#_Loc_t _x0) (o#string _x1) ];
        method class_sig_item : class_sig_item -> class_sig_item =
          fun
          [ CgNil _x0 -> CgNil (o#_Loc_t _x0)
          | CgCtr _x0 _x1 _x2 ->
              CgCtr (o#_Loc_t _x0) (o#ctyp _x1) (o#ctyp _x2)
          | CgSem _x0 _x1 _x2 ->
              CgSem (o#_Loc_t _x0) (o#class_sig_item _x1)
                (o#class_sig_item _x2)
          | CgInh _x0 _x1 -> CgInh (o#_Loc_t _x0) (o#class_type _x1)
          | CgMth _x0 _x1 _x2 _x3 ->
              CgMth (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#ctyp _x3)
          | CgVal _x0 _x1 _x2 _x3 _x4 ->
              CgVal (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#meta_bool _x3) (o#ctyp _x4)
          | CgVir _x0 _x1 _x2 _x3 ->
              CgVir (o#_Loc_t _x0) (o#string _x1) (o#meta_bool _x2)
                (o#ctyp _x3)
          | CgAnt _x0 _x1 -> CgAnt (o#_Loc_t _x0) (o#string _x1) ];
        method class_expr : class_expr -> class_expr =
          fun
          [ CeNil _x0 -> CeNil (o#_Loc_t _x0)
          | CeApp _x0 _x1 _x2 ->
              CeApp (o#_Loc_t _x0) (o#class_expr _x1) (o#expr _x2)
          | CeCon _x0 _x1 _x2 _x3 ->
              CeCon (o#_Loc_t _x0) (o#meta_bool _x1) (o#ident _x2)
                (o#ctyp _x3)
          | CeFun _x0 _x1 _x2 ->
              CeFun (o#_Loc_t _x0) (o#patt _x1) (o#class_expr _x2)
          | CeLet _x0 _x1 _x2 _x3 ->
              CeLet (o#_Loc_t _x0) (o#meta_bool _x1) (o#binding _x2)
                (o#class_expr _x3)
          | CeStr _x0 _x1 _x2 ->
              CeStr (o#_Loc_t _x0) (o#patt _x1) (o#class_str_item _x2)
          | CeTyc _x0 _x1 _x2 ->
              CeTyc (o#_Loc_t _x0) (o#class_expr _x1) (o#class_type _x2)
          | CeAnd _x0 _x1 _x2 ->
              CeAnd (o#_Loc_t _x0) (o#class_expr _x1) (o#class_expr _x2)
          | CeEq _x0 _x1 _x2 ->
              CeEq (o#_Loc_t _x0) (o#class_expr _x1) (o#class_expr _x2)
          | CeAnt _x0 _x1 -> CeAnt (o#_Loc_t _x0) (o#string _x1) ];
        method binding : binding -> binding =
          fun
          [ BiNil _x0 -> BiNil (o#_Loc_t _x0)
          | BiAnd _x0 _x1 _x2 ->
              BiAnd (o#_Loc_t _x0) (o#binding _x1) (o#binding _x2)
          | BiEq _x0 _x1 _x2 -> BiEq (o#_Loc_t _x0) (o#patt _x1) (o#expr _x2)
          | BiAnt _x0 _x1 -> BiAnt (o#_Loc_t _x0) (o#string _x1) ];
      end;
    class fold =
      object ((o : 'self_type))
        method string = fun (_ : string) -> (o : 'self_type);
        method int = fun (_ : int) -> (o : 'self_type);
        method float = fun (_ : float) -> (o : 'self_type);
        method bool = fun (_ : bool) -> (o : 'self_type);
        method list :
          ! 'a. ('self_type -> 'a -> 'self_type) -> list 'a -> 'self_type =
          fun f -> List.fold_left f o;
        method option :
          ! 'a. ('self_type -> 'a -> 'self_type) -> option 'a -> 'self_type =
          fun f -> fun [ None -> o | Some x -> f o x ];
        method array :
          ! 'a. ('self_type -> 'a -> 'self_type) -> array 'a -> 'self_type =
          fun f -> Array.fold_left f o;
        method ref :
          ! 'a. ('self_type -> 'a -> 'self_type) -> ref 'a -> 'self_type =
          fun f { val = x } -> f o x;
        method _Loc_t : Loc.t -> 'self_type = fun _ -> o;
        method with_constr : with_constr -> 'self_type =
          fun
          [ WcNil _x0 -> o#_Loc_t _x0
          | WcTyp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | WcMod _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ident _x1)#ident _x2
          | WcAnd _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#with_constr _x1)#with_constr _x2
          | WcAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method str_item : str_item -> 'self_type =
          fun
          [ StNil _x0 -> o#_Loc_t _x0
          | StCls _x0 _x1 -> (o#_Loc_t _x0)#class_expr _x1
          | StClt _x0 _x1 -> (o#_Loc_t _x0)#class_type _x1
          | StSem _x0 _x1 _x2 -> ((o#_Loc_t _x0)#str_item _x1)#str_item _x2
          | StDir _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#expr _x2
          | StExc _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#ctyp _x1)#meta_option (fun o -> o#ident) _x2
          | StExp _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | StExt _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#ctyp _x2)#meta_list
                (fun o -> o#string) _x3
          | StInc _x0 _x1 -> (o#_Loc_t _x0)#module_expr _x1
          | StMod _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#module_expr _x2
          | StRecMod _x0 _x1 -> (o#_Loc_t _x0)#module_binding _x1
          | StMty _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#module_type _x2
          | StOpn _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | StTyp _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | StVal _x0 _x1 _x2 -> ((o#_Loc_t _x0)#meta_bool _x1)#binding _x2
          | StAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method sig_item : sig_item -> 'self_type =
          fun
          [ SgNil _x0 -> o#_Loc_t _x0
          | SgCls _x0 _x1 -> (o#_Loc_t _x0)#class_type _x1
          | SgClt _x0 _x1 -> (o#_Loc_t _x0)#class_type _x1
          | SgSem _x0 _x1 _x2 -> ((o#_Loc_t _x0)#sig_item _x1)#sig_item _x2
          | SgDir _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#expr _x2
          | SgExc _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | SgExt _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#ctyp _x2)#meta_list
                (fun o -> o#string) _x3
          | SgInc _x0 _x1 -> (o#_Loc_t _x0)#module_type _x1
          | SgMod _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#module_type _x2
          | SgRecMod _x0 _x1 -> (o#_Loc_t _x0)#module_binding _x1
          | SgMty _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#module_type _x2
          | SgOpn _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | SgTyp _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | SgVal _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#ctyp _x2
          | SgAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method rec_binding : rec_binding -> 'self_type =
          fun
          [ RbNil _x0 -> o#_Loc_t _x0
          | RbSem _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#rec_binding _x1)#rec_binding _x2
          | RbEq _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ident _x1)#expr _x2
          | RbAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method patt : patt -> 'self_type =
          fun
          [ PaNil _x0 -> o#_Loc_t _x0
          | PaId _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | PaAli _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#patt _x2
          | PaAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaAny _x0 -> o#_Loc_t _x0
          | PaApp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#patt _x2
          | PaArr _x0 _x1 -> (o#_Loc_t _x0)#patt _x1
          | PaCom _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#patt _x2
          | PaSem _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#patt _x2
          | PaChr _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaInt _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaInt32 _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaInt64 _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaNativeInt _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaFlo _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaLab _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#patt _x2
          | PaOlb _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#patt _x2
          | PaOlbi _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#patt _x2)#expr _x3
          | PaOrp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#patt _x2
          | PaRng _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#patt _x2
          | PaRec _x0 _x1 -> (o#_Loc_t _x0)#patt _x1
          | PaEq _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ident _x1)#patt _x2
          | PaStr _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | PaTup _x0 _x1 -> (o#_Loc_t _x0)#patt _x1
          | PaTyc _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#ctyp _x2
          | PaTyp _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | PaVrn _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method module_type : module_type -> 'self_type =
          fun
          [ MtNil _x0 -> o#_Loc_t _x0
          | MtId _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | MtFun _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#module_type _x2)#module_type _x3
          | MtQuo _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | MtSig _x0 _x1 -> (o#_Loc_t _x0)#sig_item _x1
          | MtWit _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#module_type _x1)#with_constr _x2
          | MtAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method module_expr : module_expr -> 'self_type =
          fun
          [ MeNil _x0 -> o#_Loc_t _x0
          | MeId _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | MeApp _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#module_expr _x1)#module_expr _x2
          | MeFun _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#module_type _x2)#module_expr _x3
          | MeStr _x0 _x1 -> (o#_Loc_t _x0)#str_item _x1
          | MeTyc _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#module_expr _x1)#module_type _x2
          | MeAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method module_binding : module_binding -> 'self_type =
          fun
          [ MbNil _x0 -> o#_Loc_t _x0
          | MbAnd _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#module_binding _x1)#module_binding _x2
          | MbColEq _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#module_type _x2)#module_expr _x3
          | MbCol _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#module_type _x2
          | MbAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method meta_option :
          ! 'a.
            ('self_type -> 'a -> 'self_type) -> meta_option 'a -> 'self_type =
          fun _f_a ->
            fun
            [ ONone -> o
            | OSome _x0 -> _f_a o _x0
            | OAnt _x0 -> o#string _x0 ];
        method meta_list :
          ! 'a.
            ('self_type -> 'a -> 'self_type) -> meta_list 'a -> 'self_type =
          fun _f_a ->
            fun
            [ LNil -> o
            | LCons _x0 _x1 -> (_f_a o _x0)#meta_list (fun o -> _f_a o) _x1
            | LAnt _x0 -> o#string _x0 ];
        method meta_bool : meta_bool -> 'self_type =
          fun [ BTrue -> o | BFalse -> o | BAnt _x0 -> o#string _x0 ];
        method match_case : match_case -> 'self_type =
          fun
          [ McNil _x0 -> o#_Loc_t _x0
          | McOr _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#match_case _x1)#match_case _x2
          | McArr _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#patt _x1)#expr _x2)#expr _x3
          | McAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method ident : ident -> 'self_type =
          fun
          [ IdAcc _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ident _x1)#ident _x2
          | IdApp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ident _x1)#ident _x2
          | IdLid _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | IdUid _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | IdAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method expr : expr -> 'self_type =
          fun
          [ ExNil _x0 -> o#_Loc_t _x0
          | ExId _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | ExAcc _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExApp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExAre _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExArr _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | ExSem _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExAsf _x0 -> o#_Loc_t _x0
          | ExAsr _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | ExAss _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExChr _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExCoe _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#expr _x1)#ctyp _x2)#ctyp _x3
          | ExFlo _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExFor _x0 _x1 _x2 _x3 _x4 _x5 ->
              (((((o#_Loc_t _x0)#string _x1)#expr _x2)#expr _x3)#meta_bool
                 _x4)#
                expr _x5
          | ExFun _x0 _x1 -> (o#_Loc_t _x0)#match_case _x1
          | ExIfe _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#expr _x1)#expr _x2)#expr _x3
          | ExInt _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExInt32 _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExInt64 _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExNativeInt _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExLab _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#expr _x2
          | ExLaz _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | ExLet _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#meta_bool _x1)#binding _x2)#expr _x3
          | ExLmd _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#module_expr _x2)#expr _x3
          | ExMat _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#match_case _x2
          | ExNew _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | ExObj _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#class_str_item _x2
          | ExOlb _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#expr _x2
          | ExOvr _x0 _x1 -> (o#_Loc_t _x0)#rec_binding _x1
          | ExRec _x0 _x1 _x2 -> ((o#_Loc_t _x0)#rec_binding _x1)#expr _x2
          | ExSeq _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | ExSnd _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#string _x2
          | ExSte _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExStr _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExTry _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#match_case _x2
          | ExTup _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | ExCom _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2
          | ExTyc _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#ctyp _x2
          | ExVrn _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | ExWhi _x0 _x1 _x2 -> ((o#_Loc_t _x0)#expr _x1)#expr _x2 ];
        method ctyp : ctyp -> 'self_type =
          fun
          [ TyNil _x0 -> o#_Loc_t _x0
          | TyAli _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyAny _x0 -> o#_Loc_t _x0
          | TyApp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyArr _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyCls _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | TyLab _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#ctyp _x2
          | TyId _x0 _x1 -> (o#_Loc_t _x0)#ident _x1
          | TyMan _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyDcl _x0 _x1 _x2 _x3 _x4 ->
              ((((o#_Loc_t _x0)#string _x1)#list (fun o -> o#ctyp) _x2)#ctyp
                 _x3)#
                list (fun o (_x0, _x1) -> (o#ctyp _x0)#ctyp _x1) _x4
          | TyObj _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#meta_bool _x2
          | TyOlb _x0 _x1 _x2 -> ((o#_Loc_t _x0)#string _x1)#ctyp _x2
          | TyPol _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyQuo _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | TyQuP _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | TyQuM _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | TyVrn _x0 _x1 -> (o#_Loc_t _x0)#string _x1
          | TyRec _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyCol _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TySem _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyCom _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TySum _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyOf _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyAnd _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyOr _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyPrv _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyMut _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyTup _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TySta _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyVrnEq _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyVrnSup _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyVrnInf _x0 _x1 -> (o#_Loc_t _x0)#ctyp _x1
          | TyVrnInfSup _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyAmp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyOfAmp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | TyAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method class_type : class_type -> 'self_type =
          fun
          [ CtNil _x0 -> o#_Loc_t _x0
          | CtCon _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#meta_bool _x1)#ident _x2)#ctyp _x3
          | CtFun _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#class_type _x2
          | CtSig _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#class_sig_item _x2
          | CtAnd _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_type _x1)#class_type _x2
          | CtCol _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_type _x1)#class_type _x2
          | CtEq _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_type _x1)#class_type _x2
          | CtAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method class_str_item : class_str_item -> 'self_type =
          fun
          [ CrNil _x0 -> o#_Loc_t _x0
          | CrSem _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_str_item _x1)#class_str_item _x2
          | CrCtr _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | CrInh _x0 _x1 _x2 -> ((o#_Loc_t _x0)#class_expr _x1)#string _x2
          | CrIni _x0 _x1 -> (o#_Loc_t _x0)#expr _x1
          | CrMth _x0 _x1 _x2 _x3 _x4 ->
              ((((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#expr _x3)#ctyp _x4
          | CrVal _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#expr _x3
          | CrVir _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#ctyp _x3
          | CrVvr _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#ctyp _x3
          | CrAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method class_sig_item : class_sig_item -> 'self_type =
          fun
          [ CgNil _x0 -> o#_Loc_t _x0
          | CgCtr _x0 _x1 _x2 -> ((o#_Loc_t _x0)#ctyp _x1)#ctyp _x2
          | CgSem _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_sig_item _x1)#class_sig_item _x2
          | CgInh _x0 _x1 -> (o#_Loc_t _x0)#class_type _x1
          | CgMth _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#ctyp _x3
          | CgVal _x0 _x1 _x2 _x3 _x4 ->
              ((((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#meta_bool _x3)#
                ctyp _x4
          | CgVir _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#string _x1)#meta_bool _x2)#ctyp _x3
          | CgAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method class_expr : class_expr -> 'self_type =
          fun
          [ CeNil _x0 -> o#_Loc_t _x0
          | CeApp _x0 _x1 _x2 -> ((o#_Loc_t _x0)#class_expr _x1)#expr _x2
          | CeCon _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#meta_bool _x1)#ident _x2)#ctyp _x3
          | CeFun _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#class_expr _x2
          | CeLet _x0 _x1 _x2 _x3 ->
              (((o#_Loc_t _x0)#meta_bool _x1)#binding _x2)#class_expr _x3
          | CeStr _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#class_str_item _x2
          | CeTyc _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_expr _x1)#class_type _x2
          | CeAnd _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_expr _x1)#class_expr _x2
          | CeEq _x0 _x1 _x2 ->
              ((o#_Loc_t _x0)#class_expr _x1)#class_expr _x2
          | CeAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
        method binding : binding -> 'self_type =
          fun
          [ BiNil _x0 -> o#_Loc_t _x0
          | BiAnd _x0 _x1 _x2 -> ((o#_Loc_t _x0)#binding _x1)#binding _x2
          | BiEq _x0 _x1 _x2 -> ((o#_Loc_t _x0)#patt _x1)#expr _x2
          | BiAnt _x0 _x1 -> (o#_Loc_t _x0)#string _x1 ];
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
      object
        inherit map as super;
        method _Loc_t = fun x -> f (super#_Loc_t x);
      end;
  end;

