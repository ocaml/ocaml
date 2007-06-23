open Camlp4;
open PreCast;
module MapTy = Map.Make String;

type t =
  { name : Ast.ident;
    type_decls : MapTy.t Ast.ctyp;
    acc : Ast.expr;
    app : Ast.expr;
    id  : Ast.expr;
    tup : Ast.expr;
    com : Ast.expr;
    str : Ast.expr;
    int : Ast.expr;
    flo : Ast.expr;
    chr : Ast.expr;
    ant : Ast.ident;
  };

value _loc = Loc.ghost;

value x i = <:ident< $lid:"x"^string_of_int i$ >>;

value meta_ s = <:ident< $lid:"meta_"^s$ >>;

value mf_ s = "mf_" ^ s;

value rec string_of_ident =
  fun
  [ <:ident< $lid:s$ >> -> s
  | <:ident< $uid:s$ >> -> s
  | <:ident< $i1$.$i2$ >> -> "acc_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | <:ident< $i1$ $i2$ >> -> "app_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | <:ident< $anti:_$ >> -> assert False ];

value fold_args ty f init =
  let (_, res) =
    List.fold_left begin fun (i, acc) ty ->
      (succ i, f ty i acc)
    end (0, init) ty
  in res;

value fold_data_ctors ty f init =
  let rec loop acc t =
    match t with
    [ <:ctyp< $uid:cons$ of $ty$ >> -> f cons (Ast.list_of_ctyp ty []) acc
    | <:ctyp< $uid:cons$ >> -> f cons [] acc
    | <:ctyp< $t1$ | $t2$ >> -> loop (loop acc t1) t2
    | <:ctyp<>> -> acc
    | _ -> assert False ] in
  loop init ty;

value fold_type_decls m f init =
  MapTy.fold f m.type_decls init;

value patt_of_data_ctor_decl cons tyargs =
  fold_args tyargs begin fun _ i acc ->
    <:patt< $acc$ $id:x i$ >>
  end <:patt< $id:cons$ >>;

value expr_of_data_ctor_decl cons tyargs =
  fold_args tyargs begin fun _ i acc ->
    <:expr< $acc$ $id:x i$ >>
  end <:expr< $id:cons$ >>;

value is_antiquot_data_ctor s =
  let ls = String.length s in
  ls > 3 && String.sub s (ls - 3) 3 = "Ant";

value rec meta_ident m =
  fun
  [ <:ident< $i1$.$i2$ >> -> <:expr< Ast.IdAcc _loc $meta_ident m i1$ $meta_ident m i2$ >>
  | <:ident< $i1$ $i2$ >> -> <:expr< Ast.IdApp _loc $meta_ident m i1$ $meta_ident m i2$ >>
  | <:ident< $anti:s$ >>  -> <:expr< $anti:s$ >>
  | <:ident< $lid:s$ >>   -> <:expr< Ast.IdLid _loc $str:s$ >>
  | <:ident< $uid:s$ >>   -> <:expr< Ast.IdUid _loc $str:s$ >> ];
value m_app m x y = <:expr< $m.app$ _loc $x$ $y$ >>;
value m_id m i = <:expr< $m.id$ _loc $i$ >>;
value m_uid m s = m_id m (meta_ident m <:ident< $uid:s$ >>);

value failure = <:expr< raise (Failure "MetaGenerator: cannot handle that kind of types") >>;

value mk_meta m =
  let m_name_uid x = <:ident< $m.name$.$uid:x$ >> in
  fold_type_decls m begin fun tyname tydcl binding_acc ->
    match tydcl with
    [ Ast.TyDcl _ _ tyvars <:ctyp< [$ty$] >> _ ->
      let match_case =
        fold_data_ctors ty begin fun cons tyargs acc ->
          let m_name_cons = m_name_uid cons in
          let init = m_id m (meta_ident m m_name_cons) in
          let p = patt_of_data_ctor_decl m_name_cons tyargs in
          let e =
            if cons = "BAnt" || cons = "OAnt" || cons = "LAnt" then
              <:expr< $id:m.ant$ _loc x0 >>
            else if is_antiquot_data_ctor cons then
              expr_of_data_ctor_decl m.ant tyargs
            else
              fold_args tyargs begin fun ty i acc ->
                let rec fcall_of_ctyp ty =
                  match ty with
                  [ <:ctyp< $id:id$ >> ->
                      <:expr< $id:meta_ (string_of_ident id)$ >>
                  | <:ctyp< ($t1$ * $t2$) >> ->
                      <:expr< fun _loc (x1, x2) ->
                                $m.tup$ _loc
                                  ($m.com$ _loc
                                    ($fcall_of_ctyp t1$ _loc x1)
                                    ($fcall_of_ctyp t2$ _loc x2)) >>
                  | <:ctyp< $t1$ $t2$ >> ->
                      <:expr< $fcall_of_ctyp t1$ $fcall_of_ctyp t2$ >>
                  | <:ctyp< '$s$ >> -> <:expr< $lid:mf_ s$ >>
                  | _ -> failure ]
                in m_app m acc <:expr< $fcall_of_ctyp ty$ _loc $id:x i$ >>
              end init
          in <:match_case< $p$ -> $e$ | $acc$ >>
        end <:match_case<>> in
        let funct =
          List.fold_right begin fun tyvar acc ->
            match tyvar with
            [ <:ctyp< +'$s$ >> | <:ctyp< -'$s$ >> | <:ctyp< '$s$ >> ->
                <:expr< fun $lid:mf_ s$ -> $acc$ >>
            | _ -> assert False ]
          end tyvars <:expr< fun _loc -> fun [ $match_case$ ] >>
        in <:binding< $binding_acc$ and $lid:"meta_"^tyname$ = $funct$ >>
    | Ast.TyDcl _ _ _ _ _ -> binding_acc
    | _ -> assert False ]
  end <:binding<>>;

value find_type_decls = object
  inherit Ast.fold as super;
  value accu = MapTy.empty;
  method get = accu;
  method ctyp =
    fun
    [ Ast.TyDcl _ name _ _ _ as t -> {< accu = MapTy.add name t accu >}
    | t -> super#ctyp t ];
end;

value filter st =
  let type_decls = lazy (find_type_decls#str_item st)#get in
  object
   inherit Ast.map as super;
   method module_expr me =
     let mk_meta_module m =
       let bi = mk_meta m in
       <:module_expr<
        struct
          value meta_string _loc s = $m.str$ _loc s;
          value meta_int _loc s = $m.int$ _loc s;
          value meta_float _loc s = $m.flo$ _loc s;
          value meta_char _loc s = $m.chr$ _loc s;
          value meta_bool _loc =
            fun   
            [ False -> $m_uid m "False"$
            | True  -> $m_uid m "True"$ ];
          value rec meta_list mf_a _loc =
            fun
            [ [] -> $m_uid m "[]"$
            | [x :: xs] -> $m_app m (m_app m (m_uid m "::") <:expr< mf_a _loc x >>) <:expr< meta_list mf_a _loc xs >>$ ];
          value rec $bi$;
        end >> in
     match super#module_expr me with
     [ <:module_expr< Camlp4Filters.MetaGeneratorExpr $id:i$ >> ->
         mk_meta_module
           { name = i;
             type_decls = Lazy.force type_decls;
             app = <:expr< Ast.ExApp >>;
             acc = <:expr< Ast.ExAcc >>;
             id  = <:expr< Ast.ExId  >>;
             tup = <:expr< Ast.ExTup >>;
             com = <:expr< Ast.ExCom >>;
             str = <:expr< Ast.ExStr >>;
             int = <:expr< Ast.ExInt >>;
             flo = <:expr< Ast.ExFlo >>;
             chr = <:expr< Ast.ExChr >>;
             ant = <:ident< Ast.ExAnt >>
           }
     | <:module_expr< Camlp4Filters.MetaGeneratorPatt $id:i$ >> ->
         mk_meta_module
           { name = i;
             type_decls = Lazy.force type_decls;
             app = <:expr< Ast.PaApp >>;
             acc = <:expr< Ast.PaAcc >>;
             id  = <:expr< Ast.PaId  >>;
             tup = <:expr< Ast.PaTup >>;
             com = <:expr< Ast.PaCom >>;
             str = <:expr< Ast.PaStr >>;
             int = <:expr< Ast.PaInt >>;
             flo = <:expr< Ast.PaFlo >>;
             chr = <:expr< Ast.PaChr >>;
             ant = <:ident< Ast.PaAnt >>
           }
     | me -> me ];
  end#str_item st;

AstFilters.register_str_item_filter filter;
