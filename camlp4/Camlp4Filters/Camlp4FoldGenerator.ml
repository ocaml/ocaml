(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2006-2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)


open Camlp4;

module Id = struct
  value name    = "Camlp4FoldGenerator";
  value version = Sys.ocaml_version;
end;

module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  module StringMap = Map.Make String;
  open Ast;

  value _loc = Loc.ghost;

  value sf = Printf.sprintf;

  value xik i k =
    let i =
      if i < 0 then assert False
      else if i = 0 then ""
      else sf "_i%d" i
    in
    let k =
      if k < 1 then assert False
      else if k = 1 then ""
      else sf "_k%d" k
    in
    sf "_x%s%s" i k;
  value exik i k = <:expr< $lid:xik i k$ >>;
  value pxik i k = <:patt< $lid:xik i k$ >>;
  value elidk y k = <:expr< $lid:sf "%s_%d" y k$ >>;
  value plidk y k = <:patt< $lid:sf "%s_%d" y k$ >>;

  value xs s = "_x_" ^ s;
  value xsk = sf "_x_%s_%d";
  value exsk s k = <:expr< $lid:xsk s k$>>;

  value rec apply_expr accu =
    fun
    [ [] -> accu
    | [x :: xs] ->
        let _loc = Ast.loc_of_expr x
        in apply_expr <:expr< $accu$ $x$ >> xs ];

  value rec apply_patt accu =
    fun
    [ [] -> accu
    | [x :: xs] ->
        let _loc = Ast.loc_of_patt x
        in apply_patt <:patt< $accu$ $x$ >> xs ];

  value rec apply_ctyp accu =
    fun
    [ [] -> accu
    | [x :: xs] ->
        let _loc = Ast.loc_of_ctyp x
        in apply_ctyp <:ctyp< $accu$ $x$ >> xs ];

  value opt_map f = fun [ Some x -> Some (f x) | None -> None ];

  value list_init f n =
    let rec self m =
      if m = n then []
      else [f m :: self (succ m)]
    in self 0;

  value rec lid_of_ident sep =
    fun
    [ <:ident< $lid:s$ >> | <:ident< $uid:s$ >> -> s
    | <:ident< $i1$.$i2$ >> -> lid_of_ident sep i1 ^ sep ^ lid_of_ident sep i2
    | _ -> assert False ];

  type type_decl = (string * Ast.ident * list Ast.ctyp * Ast.ctyp * bool);

  value builtin_types =
    let tyMap = StringMap.empty in
    let tyMap =
      let abstr = ["string"; "int"; "float"; "int32"; "int64"; "nativeint"; "char"] in
      List.fold_right
        (fun name -> StringMap.add name (name, <:ident< $lid:name$ >>, [], <:ctyp<>>, False))
        abstr tyMap
    in
    let tyMap =
      let concr =
        [("bool", <:ident<bool>>, [], <:ctyp< [ False | True ] >>, False);
         ("list", <:ident<list>>, [ <:ctyp< 'a >> ], <:ctyp< [ $uid:"[]"$ | $uid:"::"$ of 'a and list 'a ] >>, False);
         ("option", <:ident<option>>, [ <:ctyp< 'a >> ], <:ctyp< [ None | Some of 'a ] >>, False);
         ("ref", <:ident<ref>>, [ <:ctyp< 'a >> ], <:ctyp< { contents : 'a } >>, False)]
      in
      List.fold_right (fun ((name, _, _, _, _) as decl) -> StringMap.add name decl) concr tyMap
    in
    tyMap;

  value used_builtins = ref StringMap.empty;

  value store_if_builtin_type id =
    if StringMap.mem id builtin_types then
      used_builtins.val := StringMap.add id (StringMap.find id builtin_types) used_builtins.val
    else ();

  type mode = [ Fold | Map | Fold_map ];

  value string_of_mode = fun [ Fold -> "fold" | Map -> "map" | Fold_map -> "fold_map" ];

  module Gen (X :
    sig
      value size : int;
      value mode : mode;
    end) =
    struct

      value size = X.size;
      value mode = X.mode;

      value tuplify_expr f =
        if size <= 0 then assert False
        else if size = 1 then f 1
        else
          let rec loop k =
            if k = 2 then f 2
            else <:expr< $loop (k - 1)$, $f k$ >>
          in <:expr< ($f 1$, $loop size$) >>;

      value tuplify_patt f =
        if size <= 0 then assert False
        else if size = 1 then f 1
        else
          let rec loop k =
            if k = 2 then f 2
            else <:patt< $loop (k - 1)$, $f k$ >>
          in <:patt< ($f 1$, $loop size$) >>;

      value xiks i = tuplify_expr (exik i);

      value tuplify_type typ =
        if size <= 0 then assert False
        else if size = 1 then typ
        else
          let rec loop k =
            if k = 2 then typ
            else <:ctyp< $loop (k - 1)$ * $typ$ >>
          in <:ctyp< ($typ$ * $loop size$) >>;

      value tuplify_tycon tycon = tuplify_type <:ctyp< $lid:tycon$ >>;

      value rec patt_of_expr =
        fun
        [ <:expr<>> -> <:patt<>>
        | <:expr< $id:i$ >> -> <:patt< $id:i$ >>
        | <:expr< $e1$, $e2$ >> -> <:patt< $patt_of_expr e1$, $patt_of_expr e2$ >>
        | <:expr< $tup:e$ >> -> <:patt< $tup:patt_of_expr e$ >>
        | _ -> assert False ];

      value bind p e1 e2 =
        match mode with
        [ Fold_map -> <:expr< let (o, $p$) = $e1$ in $e2$ >>
        | Map      -> <:expr< let $p$ = $e1$ in $e2$ >>
        | Fold     -> <:expr< let o = $e1$ in $e2$ >> ];

      value return e =
        match mode with
        [ Fold_map -> <:expr< (o, $e$) >>
        | Map      -> e
        | Fold     -> <:expr<o>> ];

      value rec opt_bind opt_patt e1 mk_e2 =
        match e1 with
        [ <:expr< $id:_$ >> | <:expr< $lid:_$#$_$ >> -> mk_e2 e1
        | <:expr< let $p1$ = $e1$ in $e2$ >> ->
            <:expr< let $p1$ = $e1$ in $opt_bind None e2 mk_e2$ >>
        | _ ->
            let e2 = mk_e2 <:expr<o>> in
            match opt_patt with
            [ Some patt -> bind patt e1 e2
            | None -> <:expr< (fun o -> $e1$) $e2$ >> ] ];

        (* ts = [t1; ...; tN] *)
      value chain_tuple mkp mke expr_of_ty ts =
        (* exiks = [<<(x_i0_k1, ..., x_i0_kM)>>; ...; <<(x_iN_k1, ..., x_iN_kM)>>] *)
        let exiks = list_init (fun i -> tuplify_expr (exik i)) (List.length ts) in
        (* exi1s, pxi1s = [<<x_i0_k1>>; ...; <<x_iN_k1>>] *)
        let exi1s = list_init (fun i -> exik i 1) (List.length ts) in
        let pxi1s = list_init (fun i -> pxik i 1) (List.length ts) in
        let ps k = mkp (list_init (fun i -> pxik i k) (List.length ts)) in
        let p = tuplify_patt ps in
        let e1 = mke exi1s in
        let es = List.map2 (fun x -> expr_of_ty (Some x)) exiks ts in
        let e =
          List.fold_right2 begin fun pxi1 e acc ->
            bind pxi1 e acc
          end pxi1s es (return e1)
        in
        <:match_case< $p$ -> $e$ >>;

      value mk_tuple expr_of_ty t =
        let mc =
          chain_tuple
            (fun ps -> <:patt< ($tup:Ast.paCom_of_list ps$) >>)
            (fun es -> <:expr< ($tup:Ast.exCom_of_list es$) >>)
            expr_of_ty (Ast.list_of_ctyp t [])
        in <:expr< fun [ $mc$ ] >>;

      value default_match_case =
        let mk k = if k = 1 then <:patt< x >> else <:patt< _ >> in
        match mode with
        [ Fold_map -> <:match_case< $tuplify_patt mk$ -> (o, x) >>
        | Fold     -> <:match_case< _ -> o >>
        | Map      -> <:match_case< $tuplify_patt mk$ -> x >> ];

      value default_expr = <:expr< fun [ $default_match_case$ ] >>;

      value mkfuno e =
        match e with
        [ <:expr< $e$ o >> -> e
        | _ -> <:expr< fun o -> $e$ >> ];

      value is_unknown t =
        let rec loop t =
          match t with
          [ <:ctyp< $lid:_$ >> -> False
          | <:ctyp< $id:_$ >> -> True
          | <:ctyp< $t$ $_$ >> -> loop t
          | _ -> False ]
        in
        match t with
        [ <:ctyp< $uid:_$ >> -> False
        | t -> loop t ];

      value contains_unknown t =
        try
          let (_ : < .. >) =
            object
              inherit Ast.fold as super;
              method ctyp t = if is_unknown t then raise Exit else super#ctyp t;
            end#ctyp t
          in False
        with [ Exit -> True ];

      value opt_bind' ox e1 mk_e2 =
        let mk_e2 =
          match ox with
          [ Some x -> fun e1 -> <:expr< $mk_e2 e1$ $x$ >>
          | _      -> mk_e2 ]
        in
        opt_bind (opt_map patt_of_expr ox) e1 mk_e2;

    (* FIXME finish me
      value rec is_simple =
        fun
        [ <:expr< $id:_$ >> -> True
        | <:expr< $e$#$_$ >> | <:expr< $tup:e$ >> -> is_simple e
        | <:expr< $e1$ $e2$ >> | <:expr< $e1$, $e2$ >> -> is_simple e1 && is_simple e2
        | _ -> False ];

      value app e1 e2 =
        let is_e1_simple = is_simple e1 in
        let is_e2_simple = is_simple e2 in
        if is_e1_simple then
          if is_e2_simple then <:expr< $e1$ $e2$ >>
          else let x = fresh "y" in <:expr< let $lid:y$ = $e2$ in $e1$ $lid:y$ >>
        else
          if is_e2_simple then
            let x = fresh "y" in <:expr< let $lid:y$ = $e1$ in $lid:y$ $e2$ >>
          else ; *)

      value opt_app e ox =
        match ox with
        [ Some x -> <:expr< $e$ $x$ >> (* call app *)
        | _ -> e ];

      value rec expr_of_ty x ty =
        let rec self ?(arity=0) ox =
          fun
          [ t when is_unknown t ->
              self ox <:ctyp< unknown >>
          | <:ctyp< $lid:id$ >> ->
              let () = store_if_builtin_type id in
              opt_bind' ox <:expr<o>> (fun e1 -> <:expr< $e1$#$id$ >>)
          | <:ctyp@_loc< $t1$ $t2$ >> ->
              let e = opt_bind None
                               (self ~arity:(arity+1) None t1)
                               (fun e1 -> <:expr< $e1$ $mkfuno (self None t2)$ >>) in
              opt_app e ox
          | <:ctyp< ( $tup:t$ ) >> ->
              opt_app (mk_tuple (self ~arity:0) t) ox
          | <:ctyp< '$s$ >> ->
              opt_app <:expr< $lid:"_f_" ^ s$ o >> ox
          | _ ->
              self ox <:ctyp< unknown >> ]
        in self x ty

      and expr_of_ty' e t = expr_of_ty (Some e) t

      and out_constr_patt s =
        <:patt< $uid:s$ >>
        (* <:patt< `$s$ >>
        <:patt< M.$uid:s$ >> *)
      and out_constr_expr s =
        <:expr< $uid:s$ >>
        (* <:expr< `$s$ >>
        <:expr< M.$uid:s$ >> *)

    (* method term t =
        match t with
        | C(x1, ..., xn) ->
            let o, x1 = o#t1 x1 in
            let o, x2 = o#t2 x2 in
            ...
            let o, xn = o#tn xn in
            o, C(x1, ..., xn)
     *)

      (* s = C, t = t1 and ... and tN *)
      and match_case_of_constructor s t =
        chain_tuple
          (apply_patt (out_constr_patt s))
          (apply_expr (out_constr_expr s))
          expr_of_ty (Ast.list_of_ctyp t [])

      and match_case_of_sum_type =
        fun
        [ <:ctyp< $t1$ | $t2$ >> ->
             <:match_case< $match_case_of_sum_type t1$ | $match_case_of_sum_type t2$ >>
        | <:ctyp< $uid:s$ of $t$ >> -> match_case_of_constructor s t
        | <:ctyp< $uid:s$ >> -> match_case_of_constructor s <:ctyp<>>
        | _ -> assert False ]

      and match_case_of_poly_constructor s ts =
        chain_tuple
          (fun [ [] -> <:patt< `$s$ >> | [p] -> <:patt< `$s$ $p$ >> | ps -> <:patt< `$s$ ($tup:Ast.paCom_of_list ps$) >> ])
          (fun [ [] -> <:expr< `$s$ >> | [e] -> <:expr< `$s$ $e$ >> | es -> <:expr< `$s$ ($tup:Ast.exCom_of_list es$) >> ])
          expr_of_ty ts

      and match_case_of_poly_sum_type =
        fun
        [ <:ctyp< $t1$ | $t2$ >> ->
             <:match_case< $match_case_of_poly_sum_type t1$ | $match_case_of_poly_sum_type t2$ >>
        | <:ctyp< `$i$ of ($tup:t$) >> -> match_case_of_poly_constructor i (Ast.list_of_ctyp t [])
        | <:ctyp< `$i$ of $t$ >> -> match_case_of_poly_constructor i [t]
        | <:ctyp< `$i$ >> -> match_case_of_poly_constructor i []
        | _ -> assert False ]

      and record_patt_of_type k =
        fun
        [ <:ctyp< $lid:s$ : $_$ >> ->
            <:patt< $lid:s$ = $lid:xsk s k$ >>
        | <:ctyp< $t1$ ; $t2$ >> ->
            <:patt< $record_patt_of_type k t1$; $record_patt_of_type k t2$ >>
        | _ -> assert False ]

      and type_list_of_record_type t ((acc1, acc2) as acc) =
        match t with
        [ <:ctyp<>> -> acc
        | <:ctyp< $lid:s$ : mutable $t$ >> | <:ctyp< $lid:s$ : $t$ >> ->
              ([s :: acc1], [t :: acc2])
        | <:ctyp< $t1$ ; $t2$ >> ->
             type_list_of_record_type t1 (type_list_of_record_type t2 acc)
        | _ -> assert False ]

      and expr_of_record_type t =
        let (ls, ts) = type_list_of_record_type t ([], []) in
        let mkp ps = <:patt< { $list:List.map2 (fun l p -> <:patt< $lid:l$ = $p$ >>) ls ps$ } >> in
        let mke es = <:expr< { $list:List.map2 (fun l e -> <:rec_binding< $lid:l$ = $e$ >>) ls es$ } >> in
        chain_tuple mkp mke expr_of_ty ts

      and failure_match_case =
        <:match_case< $tuplify_patt (pxik 0)$ ->
                        o#$lid:sf "%s%d_failure" (string_of_mode mode) size$ $tuplify_expr (exik 0)$ >>

      and complete_match_case mk t =
        match t with
        [ <:ctyp< $_$ | $_$ >> when size > 1 ->
            <:match_case< $mk t$ | $failure_match_case$ >>
        | _ -> mk t ]

      and fun_of_ctyp tyid =
        fun
        [ <:ctyp< [ $t$ ] >> ->
            <:expr< fun [ $complete_match_case match_case_of_sum_type t$ ] >>
        | <:ctyp< { $t$ } >> ->
            <:expr< fun [ $expr_of_record_type t$ ] >>
        | <:ctyp< ( $tup:t$ ) >> -> mk_tuple expr_of_ty t
        | <:ctyp< $lid:i$ >> when i = tyid -> default_expr
        | <:ctyp< $_$ $_$ >> | <:ctyp< $_$ -> $_$ >> | <:ctyp< '$_$ >> | <:ctyp< $id:_$ >> as t ->
            expr_of_ty None t
        | <:ctyp<>> ->
            expr_of_ty None <:ctyp< unknown >>
        | <:ctyp< [ = $t$ ] >> | <:ctyp< [ < $t$ ] >> | <:ctyp< private [ < $t$ ] >> ->
            <:expr< fun [ $complete_match_case match_case_of_poly_sum_type t$ ] >>
        | <:ctyp< [ > $t$ ] >> | <:ctyp< private [ > $t$ ] >> ->
            if size > 1 then
              <:expr< fun [ $complete_match_case match_case_of_poly_sum_type t$ ] >>
            else
              <:expr< fun [ $match_case_of_poly_sum_type t$ | $default_match_case$ ] >>
        | _ -> assert False ]

      and string_of_type_param t =
        match t with
        [ <:ctyp< '$s$ >> | <:ctyp< +'$s$ >> | <:ctyp< -'$s$ >> -> s
        | _ -> assert False ]

      and method_of_type_decl _ ((id1, _, params, ctyp, priv) as type_decl) acc =
        let rec lambda acc =
          fun
          [ [] -> acc
          | [ x :: xs ] -> lambda <:expr< fun $lid:"_f_" ^ x$ -> $acc$ >> xs ] in
        let params' = List.map string_of_type_param params in
        let funs = lambda (fun_of_ctyp id1 ctyp) params' in
        let ty = method_type_of_type_decl type_decl in
        let priv = if priv then <:private_flag< private >> else <:private_flag<>> in
        <:class_str_item< method $private:priv$ $lid:id1$ : $ty$ = $funs$; $acc$ >>

      and ctyp_name_of_name_params name params =
        apply_ctyp <:ctyp< $id:name$ >> params

      and method_type_of_type_decl (_, name, params, ctyp, _) =
        let t = ctyp_name_of_name_params name params in
        if mode = Map && not (contains_unknown ctyp) then
          let out_params = List.map (fun [ <:ctyp< '$i$ >> -> <:ctyp< '$i^"_out"$ >> | _ -> assert False ]) params in
          let t_out = ctyp_name_of_name_params name out_params in
          method_type_of_type t t_out params out_params
        else
          method_type_of_type t t params []

      and method_type_of_type t_in t_out params_in params_out =
        let rt t =
          match mode with
          [ Fold_map -> <:ctyp< ('self_type * $t$) >>
          | Fold     -> <:ctyp< 'self_type >>
          | Map      -> t ]
        in
        match (params_in, params_out) with
        [ ([param_in], [param_out]) ->
            let alphas = tuplify_type param_in in
            <:ctyp< ! $param_in$ $param_out$ . ('self_type -> $alphas$ -> $rt param_out$) -> $tuplify_type t_in$ -> $rt t_out$ >>
        | ([param], []) ->
            let alphas = tuplify_type param in
            <:ctyp< ! $param$ . ('self_type -> $alphas$ -> $rt param$) -> $tuplify_type t_in$ -> $rt t_out$ >>
        | ([], []) ->
            <:ctyp< $tuplify_type t_in$ -> $rt t_out$ >>
        | _ ->
            let i = List.length params_in in
            failwith (Printf.sprintf
                  "Camlp4FoldGenerator: FIXME not implemented for types with %d parameters" i) ]

      and class_sig_item_of_type_decl _ ((name, _, _, t, _) as type_decl) acc =
        let (_ : < .. >) =
          object (self)
            inherit Ast.fold as super;
            method ctyp =
              fun
              [ <:ctyp< $lid:id$ >> -> let () = store_if_builtin_type id in self
              | t -> super#ctyp t ];
          end#ctyp t
        in
        <:class_sig_item<
           method $lid:name$ : $method_type_of_type_decl type_decl$;
           $acc$ >>

      and generate_structure tyMap =
        StringMap.fold method_of_type_decl used_builtins.val
          (StringMap.fold method_of_type_decl tyMap <:class_str_item<>>)

      and generate_signature tyMap =
        StringMap.fold class_sig_item_of_type_decl used_builtins.val
          (StringMap.fold class_sig_item_of_type_decl tyMap <:class_sig_item<>>);

  end;

  value rec tyMap_of_type_decls t acc =
    match t with
    [ <:ctyp<>> -> acc
    | <:ctyp< $t1$ and $t2$ >> ->
        tyMap_of_type_decls t1 (tyMap_of_type_decls t2 acc)
    | Ast.TyDcl _ name tl tk _ ->
        StringMap.add name (name, <:ident< $lid:name$ >>, tl, tk, False) acc
    | _ -> assert False ];

  value generate_class_implem ?(virtual_flag=False) mode c tydcl n =
    let tyMap = tyMap_of_type_decls tydcl StringMap.empty in
    let module M = Gen(struct value size = n; value mode = mode; end) in
    let generated = M.generate_structure tyMap in
    let gen_type =
      <:ctyp< ! 'a 'b . $M.method_type_of_type <:ctyp< 'a >> <:ctyp< 'b >> [] []$ >>
    in
    let failure =
      if n > 1 then
        let name = string_of_mode mode in
        <:class_str_item< method $lid:sf "%s%d_failure" name n$ : $gen_type$ =
                            fun $M.tuplify_patt (pxik 0)$ ->
                              failwith $`str:sf "%s%d_failure: default implementation" name n$ >>
      else <:class_str_item<>>
    in
    let gen_type =
      <:ctyp< ! 'a . $M.method_type_of_type <:ctyp< 'a >> <:ctyp< 'a >> [] []$ >>
    in
    let unknown =
      <:class_str_item< method unknown : $gen_type$ = $M.default_expr$ >> in
    if not virtual_flag then
      <:str_item< class $lid:c$ = object (o : 'self_type) $generated$; $failure$; $unknown$ end >>
    else 
      <:str_item< class virtual $lid:c$ = object (o : 'self_type) $generated$; $failure$; $unknown$ end >>;

  value generate_class_interf ?(virtual_flag=False) mode c tydcl n =
    let tyMap = tyMap_of_type_decls tydcl StringMap.empty in
    let module M = Gen(struct value size = n; value mode = mode; end) in
    let generated = M.generate_signature tyMap in
    let gen_type =
      <:ctyp< ! 'a 'b . $M.method_type_of_type <:ctyp< 'a >> <:ctyp< 'b >> [] []$ >>
    in
    let failure =
      if n > 1 then
        let name = string_of_mode mode in
        <:class_sig_item< method $lid:sf "%s%d_failure" name n$ : $gen_type$ >>
      else <:class_sig_item<>>
    in
    let gen_type =
      <:ctyp< ! 'a . $M.method_type_of_type <:ctyp< 'a >> <:ctyp< 'a >> [] []$ >>
    in
    let unknown =
      <:class_sig_item< method unknown : $gen_type$ >>
    in
    if not virtual_flag then 
      <:sig_item< class $lid:c$ : object ('self_type) $generated$; $failure$; $unknown$ end >>
    else
      <:sig_item< class virtual $lid:c$ : object ('self_type) $generated$; $failure$; $unknown$ end >> ;

  value processor =
    let last = ref <:ctyp<>> in
    let generate_class' generator default c s n =
      match s with
      [ "Fold"    -> generator Fold c last.val n
      | "Map"     -> generator Map c last.val n
      | "FoldMap" -> generator Fold_map c last.val n
      | _ -> default ]
    in
    let generate_class_from_module_name generator c default m =
      try Scanf.sscanf m "Camlp4%[^G]Generator" begin fun m' ->
        try Scanf.sscanf m' "%[^0-9]%d" (generate_class' generator default c)
        with [ End_of_file | Scanf.Scan_failure _ -> generate_class' generator default c m' 1 ]
      end with [ End_of_file | Scanf.Scan_failure _ -> default ]
    in
    object (self)
      inherit Ast.map as super;

      method str_item st =
        match st with
        [ <:str_item< type $t$ >> -> (last.val := t; st)

        (* backward compatibility *)
        | <:str_item@_loc< class $lid:c$ = Camlp4Filters.GenerateFold.generated >> ->
              generate_class_implem Fold c last.val 1
        | <:str_item@_loc< class virtual $lid:c$ = Camlp4Filters.GenerateFold.generated >> ->
              generate_class_implem ~virtual_flag:True Fold c last.val 1

        | <:str_item@_loc< class $lid:c$ = Camlp4Filters.GenerateMap.generated >> ->
              generate_class_implem Map c last.val 1
        | <:str_item@_loc< class virtual $lid:c$ = Camlp4Filters.GenerateMap.generated >> ->
              generate_class_implem ~virtual_flag:True Map c last.val 1

        (* Handle Camlp4(Fold|Map|FoldMap)\d*Generator *)
        | <:str_item@_loc< class $lid:c$ = $uid:m$.generated >> ->
              generate_class_from_module_name (generate_class_implem ~virtual_flag:False) c st m
        | <:str_item@_loc< class virtual $lid:c$ = $uid:m$.generated >> ->
              generate_class_from_module_name (generate_class_implem ~virtual_flag:True) c st m

        (* It's a hack to force to recurse on the left to right order *)
        | <:str_item< $st1$; $st2$ >> ->
             let st1 = self#str_item st1 in
              <:str_item< $st1$; $self#str_item st2$ >>

        | st -> super#str_item st ];

      method sig_item sg =
        match sg with
        [ <:sig_item< type $t$ >> -> (last.val := t; sg)

        (* backward compatibility *)
        | <:sig_item@_loc< class $lid:c$ : Camlp4Filters.GenerateFold.generated >> ->
             generate_class_interf Fold c last.val 1
        | <:sig_item@_loc< class virtual $lid:c$ : Camlp4Filters.GenerateFold.generated >> ->
             generate_class_interf ~virtual_flag:True Fold c last.val 1

        | <:sig_item@_loc< class $lid:c$ : Camlp4Filters.GenerateMap.generated >> ->
             generate_class_interf Map c last.val 1
        | <:sig_item@_loc< class virtual $lid:c$ : Camlp4Filters.GenerateMap.generated >> ->
             generate_class_interf ~virtual_flag:True Map c last.val 1

        (* Handle Camlp4(Fold|Map|FoldMap)\d*Generator *)
        | <:sig_item@_loc< class $lid:c$ : $uid:m$.generated >> ->
            generate_class_from_module_name (generate_class_interf ~virtual_flag:False) c sg m
        | <:sig_item@_loc< class virtual $lid:c$ : $uid:m$.generated >> ->
            generate_class_from_module_name (generate_class_interf ~virtual_flag:True) c sg m

        (* It's a hack to force to recurse on the left to right order *)
        | <:sig_item< $sg1$; $sg2$ >> ->
             let sg1 = self#sig_item sg1 in
              <:sig_item< $sg1$; $self#sig_item sg2$ >>

        | sg -> super#sig_item sg ];
    end;

  register_str_item_filter processor#str_item;
  register_sig_item_filter processor#sig_item;

end;

let module M = Camlp4.Register.AstFilter Id Make in ();
