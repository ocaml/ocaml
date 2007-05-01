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


open Camlp4;

module Id = struct
  value name    = "Camlp4FoldGenerator";
  value version = "$Id$";
end;

module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  module StringMap = Map.Make String;
  open Ast;

  value _loc = Loc.ghost;

  value xi i = "_x" ^ string_of_int i;

  value xs s = "_x_" ^ s;

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

  value list_mapi f =
    let rec self i =
      fun
      [ [] -> []
      | [ x :: xs ] -> [ f i x :: self (succ i) xs ] ]
    in self 0;

  value list_init f n =
    let rec self m =
      if m = n then []
      else [f m :: self (succ m)]
    in self 0;

  (* Yes this is a poor fresh function *)
  value fresh =
    let count = ref 0 in
    fun basename ->
      let res = basename ^ (string_of_int count.val)
      in do { incr count; res };

  value mk_tuple self t =
    let tl = Ast.list_of_ctyp t [] in
    let n = List.length tl in
    let exi i = <:expr< $lid:xi i$ >> in
    let pxi i = <:patt< $lid:xi i$ >> in
    let (e, _) =
      List.fold_left
        (fun (acc, i) t -> (self ?obj:(Some acc) (Some (exi i)) t, succ i))
        (<:expr<o>>, 0) tl in
    <:expr< fun ($tup:Ast.paCom_of_list (list_init pxi n)$) -> $e$ >>;

  value builtins =
    <:class_str_item<
      method string (_ : string) : 'self_type = o;
      method int (_ : int) : 'self_type = o;
      method float (_ : float) : 'self_type = o;
      method bool (_ : bool) : 'self_type = o;
      method list : ! 'a . ('self_type -> 'a -> 'self_type) -> list 'a -> 'self_type =
        fun f -> List.fold_left f o;
      method option : ! 'a . ('self_type -> 'a -> 'self_type) -> option 'a -> 'self_type =
        fun f -> fun [ None -> o | Some x -> f o x ];
      method array : ! 'a . ('self_type -> 'a -> 'self_type) -> array 'a -> 'self_type =
        fun f -> Array.fold_left f o;
      method ref : ! 'a . ('self_type -> 'a -> 'self_type) -> ref 'a -> 'self_type =
        fun f { val = x } -> f o x;
  >>;

  value rec lid_of_ident sep =
    fun
    [ <:ident< $lid:s$ >> | <:ident< $uid:s$ >> -> s
    | <:ident< $i1$.$i2$ >> -> lid_of_ident sep i1 ^ sep ^ lid_of_ident sep i2
    | _ -> assert False ];

  type type_decl = (string * Ast.ident * list Ast.ctyp * Ast.ctyp);

  value (unknown_type, fold_unknown_types) =
    let set = ref StringMap.empty in
    let add id1 id2 ty = set.val := StringMap.add id1 (id1, id2, [], ty) set.val
    and fold f = StringMap.fold f set.val in (add, fold);

  value rec expr_of_ty ?obj x ty =
    let rec self ?(obj = <:expr<o>>) ox =
      fun
      [ <:ctyp< $lid:id$ >> ->
          match ox with
          [ Some x -> <:expr< $obj$#$id$ $x$ >>
          | _ -> <:expr< $obj$#$id$ >> ]
      | <:ctyp< $t1$ $t2$ >> ->
          let e = <:expr< $self ~obj None t1$ (fun o -> $self None t2$) >> in
          match ox with
          [ Some x -> <:expr< $e$ $x$ >>
          | _   -> e ]
      | <:ctyp< $t1$ -> $t2$ >> ->
          let mk_fun x =
            let y = fresh "y" in
            let py = <:expr< $lid:y$ >> in
            let e = <:expr< $x$ $self (Some py) t1$ >>
            in <:expr< fun $lid:y$ -> $self ~obj (Some e) t2$ >> in
          match ox with
          [ Some x -> mk_fun x
          | _ ->
              let z = fresh "z" in
              let pz = <:expr< $lid:z$ >> in
              <:expr< fun $lid:z$ -> $mk_fun pz$ >> ]
      | <:ctyp< ( $tup:t$ ) >> ->
          let e = mk_tuple self t in
          match ox with
          [ Some x -> <:expr< $e$ $x$ >>
          | _ -> e ]
      | <:ctyp< '$s$ >> ->
          let id = "_f_" ^ s in
          match ox with
          [ Some x -> <:expr< $lid:id$ o $x$ >>
          | _   -> <:expr< $lid:id$ o >> ]
      | <:ctyp< $id:i$ >> ->
          let id1 = "_" ^ lid_of_ident "_" i in
          let ty = <:ctyp< $lid:id1$ >> in
          let () = unknown_type id1 i ty in
          self ox ty
      | _ ->
          match ox with
          [ Some x -> <:expr< $x$ >>
          | _   -> <:expr< fun _ -> o >> ] ]
    in self ?obj x ty

  and expr_of_constructor t (i, acc) =
    match t with
    [ <:ctyp< $t1$ and $t2$ >> ->
        expr_of_constructor t2 (expr_of_constructor t1 (i, acc))
    | _ -> (succ i, <:expr< $expr_of_ty ~obj:acc (Some <:expr< $lid:xi i$ >>) t$ >>) ]

(*   and expr_of_constructor_for_fold t (i, acc) =
    match t with
    [ <:ctyp< $t1$ and $t2$ >> ->
        expr_of_constructor_for_fold t2 (expr_of_constructor_for_fold t1 (i, acc))
    | _ -> (succ i, <:expr< $acc$ $expr_of_ty (Some <:expr< $lid:xi i$ >>) t$ >>) ]
 *)
  and patt_of_constructor t (i, acc) =
    match t with
    [ <:ctyp< $t1$ and $t2$ >> ->
        patt_of_constructor t2 (patt_of_constructor t1 (i, acc))
    | _ -> (succ i, <:patt< $acc$ $lid:xi i$ >>) ]

  and match_case_of_sum_type =
    fun
    [ <:ctyp< $t1$ | $t2$ >> ->
         <:match_case< $match_case_of_sum_type t1$ | $match_case_of_sum_type t2$ >>
    | <:ctyp< $uid:s$ of $t$ >> ->
         <:match_case< $pat:snd (patt_of_constructor t (0, <:patt< $uid:s$ >>))$
               -> $snd (expr_of_constructor t (0, <:expr< o >>))$ >>
    | <:ctyp< $uid:s$ >> ->
         <:match_case< $uid:s$ -> o >>
    | _ -> assert False ]

  and match_case_of_poly_sum_type =
    fun
    [ <:ctyp< $t1$ | $t2$ >> ->
         <:match_case< $match_case_of_poly_sum_type t1$ | $match_case_of_poly_sum_type t2$ >>
    | <:ctyp< `$i$ of $t$ >> ->
         <:match_case< `$i$ x -> $expr_of_ty ~obj:<:expr< o >> (Some <:expr< x >>) t$ >>
    | <:ctyp< `$i$ >> ->
         <:match_case< `$i$ -> o >>
    | _ -> assert False ]

  and record_patt_of_type =
    fun
    [ <:ctyp< $lid:s$ : $_$ >> ->
        <:patt< $lid:s$ = $lid:xs s$ >>
    | <:ctyp< $t1$ ; $t2$ >> ->
        <:patt< $record_patt_of_type t1$; $record_patt_of_type t2$ >>
    | _ -> assert False ]

  and record_binding_of_type =
    fun
    [ <:ctyp< $lid:s$ : mutable $t$ >> | <:ctyp< $lid:s$ : $t$ >> ->
         <:rec_binding< $lid:s$ = $expr_of_ty (Some <:expr< $lid:xs s$ >>) t$ >>
    | <:ctyp< $t1$ ; $t2$ >> ->
         <:rec_binding< $record_binding_of_type t1$; $record_binding_of_type t2$ >>
    | _ -> assert False ]

  and fun_of_ctyp tyid =
    fun
    [ <:ctyp< [ $t$ ] >> ->
        <:expr< fun [ $match_case_of_sum_type t$ ] >>
    | <:ctyp< { $t$ } >> ->
        <:expr< fun { $record_patt_of_type t$ } -> { $record_binding_of_type t$ } >>
    | <:ctyp< ( $tup:t$ ) >> -> mk_tuple expr_of_ty t
    | <:ctyp< $_$ $_$ >> | <:ctyp< $_$ -> $_$ >> | <:ctyp< '$_$ >> as t ->
        expr_of_ty None t
    | <:ctyp< $lid:i$ >> when i = tyid -> <:expr< fun _ -> o >>
    | <:ctyp< $id:i$ >> as t ->
        let id1 = "_" ^ lid_of_ident "_" i in
        if id1 = tyid then <:expr< fun _ -> o >>
        else expr_of_ty None t
    | <:ctyp< [ = $t$ ] >> | <:ctyp< [ < $t$ ] >> | <:ctyp< private [ < $t$ ] >> ->
        <:expr< fun [ $match_case_of_poly_sum_type t$ ] >>
    | <:ctyp< [ > $t$ ] >> | <:ctyp< private [ > $t$ ] >> ->
        <:expr< fun [ $match_case_of_poly_sum_type t$ | x -> x ] >>
    | _ -> assert False ]

  and string_of_type_param t =
    match t with
    [ <:ctyp< '$s$ >> | <:ctyp< +'$s$ >> | <:ctyp< -'$s$ >> -> s
    | _ -> assert False ]

  and method_of_type_decl ((id1, _, params, ctyp) as type_decl) =
    let rec lambda acc =
      fun
      [ [] -> acc
      | [ x :: xs ] -> lambda <:expr< fun $lid:"_f_" ^ x$ -> $acc$ >> xs ] in
    let params' = List.map string_of_type_param params in
    let funs = lambda (fun_of_ctyp id1 ctyp) params' in
    let ty = method_type_of_type_decl type_decl in
    <:class_str_item< method $lid:id1$ : $ty$ = $funs$ >>

  and ctyp_name_of_name_params name params = 
    apply_ctyp <:ctyp< $id:name$ >> params

  and method_type_of_type_decl (_, name, params, _) =
    let t = ctyp_name_of_name_params name [] (* FIXME params *) in
    match List.length params with
    [ 1 -> <:ctyp< ! 'a . ('self_type -> 'a -> 'self_type) -> $t$ 'a -> 'self_type >>
    | 0 -> <:ctyp< $t$ -> 'self_type >>
    | _ -> failwith "FIXME not implemented" ]

  and class_sig_item_of_type_decl _ ((name, _, _, _) as type_decl) acc =
    <:class_sig_item<
       method $lid:name$ : $method_type_of_type_decl type_decl$;
       $acc$ >>

  and tyMap_of_type_decls t acc =
    match t with
    [ <:ctyp< $t1$ and $t2$ >> ->
        tyMap_of_type_decls t1 (tyMap_of_type_decls t2 acc)
    | Ast.TyDcl _ name tl tk _ ->
        StringMap.add name (name, <:ident< $lid:name$ >>, tl, tk) acc
    | _ -> assert False ]

  and fold_types_in_str_item f =
    fun
    [ <:str_item< type $t$ >> -> f t
    | <:str_item< $st1$; $st2$ >> -> fun acc ->
        fold_types_in_str_item f st1 (fold_types_in_str_item f st2 acc)
    | <:str_item< module $_$ = struct $st$ end >> |
      <:str_item< module $_$ ($_$:$_$) = struct $st$ end >> ->
        fold_types_in_str_item f st
    | _ -> fun x -> x ]

  and fold_types_in_sig_item f =
    fun
    [ <:sig_item< type $t$ >> -> f t
    | <:sig_item< $sg1$; $sg2$ >> -> fun acc ->
        fold_types_in_sig_item f sg1 (fold_types_in_sig_item f sg2 acc)
    | <:sig_item< module $_$ : sig $sg$ end >> |
      <:sig_item< module $_$ ($_$:$_$) : sig $sg$ end >> ->
        fold_types_in_sig_item f sg
    | _ -> fun x -> x ]

  and collect_types_in_str_item str_item =
    fold_types_in_str_item tyMap_of_type_decls str_item StringMap.empty

  and collect_types_in_sig_item sig_item =
    fold_types_in_sig_item tyMap_of_type_decls sig_item StringMap.empty

  and generate_structure tyMap =
    let f x acc = <:class_str_item< $method_of_type_decl x$; $acc$ >> in
    let g _ ty = f ty in
    fold_unknown_types g (StringMap.fold g tyMap <:class_str_item<>>)

  and generate_signature tyMap =
    StringMap.fold class_sig_item_of_type_decl tyMap <:class_sig_item<>>

  and inject_structure_drop_trash generated =
    (Ast.map_str_item
      (fun
       [ <:str_item@_loc< class $lid:c$ = Camlp4Filters.GenerateFold.generated >> ->
            (* FIXME <:str_item< class $lid:c$ = object (o) $builtins$; $generated$ end >> *)
            let x = <:class_str_item< $builtins$; $generated$ >> in
            <:str_item< class $lid:c$ = object (o : 'self_type) $x$ end >>
       | s -> s ]))#str_item
  
  and inject_signature generated =
    (Ast.map_sig_item
      (fun
       [ <:sig_item@_loc< class $lid:c$ : Camlp4Filters.GenerateFold.generated >> ->
            <:sig_item< class $lid:c$ : object $generated$ end >>
       | s -> s ]))#sig_item

  and process_str_item str_item =
    let tyMap = collect_types_in_str_item str_item in
    let generated = generate_structure tyMap in
    inject_structure_drop_trash generated str_item

  and process_sig_item sig_item =
    let tyMap = collect_types_in_sig_item sig_item in
    let generated = generate_signature tyMap in
    inject_signature generated sig_item;

  register_str_item_filter process_str_item;
  register_sig_item_filter process_sig_item;

end;

let module M = Camlp4.Register.AstFilter Id Make in ();
