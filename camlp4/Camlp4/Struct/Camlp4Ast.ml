(* Generated file! Do not edit by hand *)
module Make (Loc : Sig.Loc.S) : Sig.Camlp4Ast.S with module Loc = Loc =
  struct
    module Loc = Loc;
    module Ast = Sig.Camlp4Ast.Make(Loc);
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
    external loc_of_module_binding : module_binding -> Loc.t = "%field0";
    external loc_of_assoc : assoc -> Loc.t = "%field0";
    external loc_of_ident : ident -> Loc.t = "%field0";
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
          fun f { \val = x } -> {  \val = f x; };
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
              StExt (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2) (o#string _x3)
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
              SgExt (o#_Loc_t _x0) (o#string _x1) (o#ctyp _x2) (o#string _x3)
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
          | PaEq _x0 _x1 _x2 -> PaEq (o#_Loc_t _x0) (o#patt _x1) (o#patt _x2)
          | PaStr _x0 _x1 -> PaStr (o#_Loc_t _x0) (o#string _x1)
          | PaTup _x0 _x1 -> PaTup (o#_Loc_t _x0) (o#patt _x1)
          | PaTyc _x0 _x1 _x2 ->
              PaTyc (o#_Loc_t _x0) (o#patt _x1) (o#ctyp _x2)
          | PaTyp _x0 _x1 -> PaTyp (o#_Loc_t _x0) (o#ident _x1)
          | PaVrn _x0 _x1 -> PaVrn (o#_Loc_t _x0) (o#string _x1) ];
        method module_type : module_type -> module_type =
          fun
          [ MtId _x0 _x1 -> MtId (o#_Loc_t _x0) (o#ident _x1)
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
          [ MeId _x0 _x1 -> MeId (o#_Loc_t _x0) (o#ident _x1)
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
        method meta_bool : meta_bool -> meta_bool =
          fun
          [ BTrue -> BTrue
          | BFalse -> BFalse
          | BAnt _x0 -> BAnt (o#string _x0) ];
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
          | ExFun _x0 _x1 -> ExFun (o#_Loc_t _x0) (o#assoc _x1)
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
              ExMat (o#_Loc_t _x0) (o#expr _x1) (o#assoc _x2)
          | ExNew _x0 _x1 -> ExNew (o#_Loc_t _x0) (o#ident _x1)
          | ExObj _x0 _x1 _x2 ->
              ExObj (o#_Loc_t _x0) (o#patt _x1) (o#class_str_item _x2)
          | ExOlb _x0 _x1 _x2 ->
              ExOlb (o#_Loc_t _x0) (o#string _x1) (o#expr _x2)
          | ExOvr _x0 _x1 -> ExOvr (o#_Loc_t _x0) (o#binding _x1)
          | ExRec _x0 _x1 _x2 ->
              ExRec (o#_Loc_t _x0) (o#binding _x1) (o#expr _x2)
          | ExSeq _x0 _x1 -> ExSeq (o#_Loc_t _x0) (o#expr _x1)
          | ExSnd _x0 _x1 _x2 ->
              ExSnd (o#_Loc_t _x0) (o#expr _x1) (o#string _x2)
          | ExSte _x0 _x1 _x2 ->
              ExSte (o#_Loc_t _x0) (o#expr _x1) (o#expr _x2)
          | ExStr _x0 _x1 -> ExStr (o#_Loc_t _x0) (o#string _x1)
          | ExTry _x0 _x1 _x2 ->
              ExTry (o#_Loc_t _x0) (o#expr _x1) (o#assoc _x2)
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
          | BiSem _x0 _x1 _x2 ->
              BiSem (o#_Loc_t _x0) (o#binding _x1) (o#binding _x2)
          | BiEq _x0 _x1 _x2 -> BiEq (o#_Loc_t _x0) (o#patt _x1) (o#expr _x2)
          | BiAnt _x0 _x1 -> BiAnt (o#_Loc_t _x0) (o#string _x1) ];
        method assoc : assoc -> assoc =
          fun
          [ AsNil _x0 -> AsNil (o#_Loc_t _x0)
          | AsOr _x0 _x1 _x2 ->
              AsOr (o#_Loc_t _x0) (o#assoc _x1) (o#assoc _x2)
          | AsArr _x0 _x1 _x2 _x3 ->
              AsArr (o#_Loc_t _x0) (o#patt _x1) (o#expr _x2) (o#expr _x3)
          | AsAnt _x0 _x1 -> AsAnt (o#_Loc_t _x0) (o#string _x1) ];
      end;
    class c_expr f =
      object inherit map as super; method expr = fun x -> f (super#expr x);
      end;
    class c_patt f =
      object inherit map as super; method patt = fun x -> f (super#patt x);
      end;
    class c_ctyp f =
      object inherit map as super; method ctyp = fun x -> f (super#ctyp x);
      end;
    class c_str_item f =
      object inherit map as super;
        method str_item = fun x -> f (super#str_item x);
      end;
    class c_sig_item f =
      object inherit map as super;
        method sig_item = fun x -> f (super#sig_item x);
      end;
    class c_loc f =
      object inherit map as super;
        method _Loc_t = fun x -> f (super#_Loc_t x);
      end;
    value map_patt f ast = (new c_patt f)#patt ast;
    value map_loc f ast = (new c_loc f)#_Loc_t ast;
    value map_sig_item f ast = (new c_sig_item f)#sig_item ast;
    value map_str_item f ast = (new c_str_item f)#str_item ast;
    value map_ctyp f ast = (new c_ctyp f)#ctyp ast;
    value map_expr f ast = (new c_expr f)#expr ast;
    value ghost = Loc.ghost;
    value safe_string_escaped s =
      if ((String.length s) > 2) && ((s.[0] = '\\') && (s.[1] = '$'))
      then s
      else String.escaped s;
    value rec is_module_longident =
      fun
      [ Ast.IdAcc _ _ i -> is_module_longident i
      | Ast.IdApp _ i1 i2 ->
          (is_module_longident i1) && (is_module_longident i2)
      | Ast.IdUid _ _ -> True
      | _ -> False ];
    value ident_of_expr =
      let error () =
        invalid_arg "ident_of_expr: this expression is not an identifier"
      in
        let rec self =
          fun
          [ Ast.ExApp _loc e1 e2 -> Ast.IdApp _loc (self e1) (self e2)
          | Ast.ExAcc _loc e1 e2 -> Ast.IdAcc _loc (self e1) (self e2)
          | Ast.ExId _ (Ast.IdLid _ _) -> error ()
          | Ast.ExId _ i -> if is_module_longident i then i else error ()
          | _ -> error () ]
        in
          fun
          [ Ast.ExId _ i -> i
          | Ast.ExApp _loc e1 e2 -> error ()
          | t -> self t ];
    value ident_of_ctyp =
      let error () =
        invalid_arg "ident_of_ctyp: this type is not an identifier"
      in
        let rec self =
          fun
          [ Ast.TyApp _loc t1 t2 -> Ast.IdApp _loc (self t1) (self t2)
          | Ast.TyId _ (Ast.IdLid _ _) -> error ()
          | Ast.TyId _ i -> if is_module_longident i then i else error ()
          | _ -> error () ]
        in fun [ Ast.TyId _ i -> i | t -> self t ];
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
    value rec asOr_of_list =
      fun
      [ [] -> Ast.AsNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_assoc x in Ast.AsOr _loc x (asOr_of_list xs) ];
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
    value rec biSem_of_list =
      fun
      [ [] -> Ast.BiNil ghost
      | [ x ] -> x
      | [ x :: xs ] ->
          let _loc = loc_of_binding x in Ast.BiSem _loc x (biSem_of_list xs) ];
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
      | Ast.BiSem _ b1 b2 -> (pel_of_binding b1) @ (pel_of_binding b2)
      | t -> assert False ];
    value rec list_of_binding x acc =
      match x with
      [ Ast.BiAnd _ b1 b2 | Ast.BiSem _ b1 b2 ->
          list_of_binding b1 (list_of_binding b2 acc)
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
    value rec list_of_assoc x acc =
      match x with
      [ Ast.AsNil _ -> acc
      | Ast.AsOr _ x y -> list_of_assoc x (list_of_assoc y acc)
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
  end;

