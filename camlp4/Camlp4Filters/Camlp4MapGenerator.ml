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
  value name    = "Camlp4MapGenerator";
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
    let el = list_mapi (fun i -> self (Some (exi i))) tl in
    <:expr< fun ($tup:Ast.paCom_of_list (list_init pxi n)$)
             -> ($tup:Ast.exCom_of_list el$) >>;

  value builtins =
    <:class_str_item<
      method string x : string = x;
      method int x : int = x;
      method float x : float = x;
      method bool x : bool = x;
      method list : ! 'a 'b . ('a -> 'b) -> list 'a -> list 'b =
        List.map;
      method option : ! 'a 'b . ('a -> 'b) -> option 'a -> option 'b =
        fun f -> fun [ None -> None | Some x -> Some (f x) ];
      method array : ! 'a 'b . ('a -> 'b) -> array 'a -> array 'b =
        Array.map;
      method ref : ! 'a 'b . ('a -> 'b) -> ref 'a -> ref 'b =
        fun f { val = x } -> { val = f x };
    >>;

  (* FIXME UNUSED *)
  value builtins_sig =
    <:sig_item<
      value string : string -> string;
      value int : int -> int;
      value float : float -> float;
      value bool : bool -> bool;
      value list : ('a -> 'b) -> list 'a -> list 'b;
      value array : ('a -> 'b) -> array 'a -> array 'b;
      value option : ('a -> 'b) -> option 'a -> option 'b;
      value ref : ('a -> 'b) -> ref 'a -> ref 'b;
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

  value rec expr_of_ty x ty =
    let rec self ox =
      fun
      [ <:ctyp< $lid:id$ >> ->
          match ox with
          [ Some x -> <:expr< o#$id$ $x$ >>
          | _ -> <:expr< o#$id$ >> ]
      | <:ctyp< $t1$ $t2$ >> ->
          let e = <:expr< $self None t1$ $self None t2$ >> in
          match ox with
          [ Some x -> <:expr< $e$ $x$ >>
          | _   -> e ]
      | <:ctyp< $t1$ -> $t2$ >> ->
          let mk_fun x =
            let y = fresh "y" in
            let py = <:expr< $lid:y$ >> in
            let e = <:expr< $x$ $self (Some py) t1$ >>
            in <:expr< fun $lid:y$ -> $self (Some e) t2$ >> in
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
          [ Some x -> <:expr< $lid:id$ $x$ >>
          | _   -> <:expr< $lid:id$ >> ]
      | <:ctyp< $id:i$ >> ->
          let id1 = "_" ^ lid_of_ident "_" i in
          let ty = <:ctyp< $lid:id1$ >> in
          let () = unknown_type id1 i ty in
          self ox ty
      | _ ->
          match ox with
          [ Some x -> <:expr< $x$ >>
          | _   -> <:expr< fun x -> x >> ] ]
    in self x ty

  and expr_of_constructor t (i, acc) =
    match t with
    [ <:ctyp< $t1$ and $t2$ >> ->
        expr_of_constructor t2 (expr_of_constructor t1 (i, acc))
    | _ -> (succ i, <:expr< $acc$ $expr_of_ty (Some <:expr< $lid:xi i$ >>) t$ >>) ]

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
               -> $snd (expr_of_constructor t (0, <:expr< $uid:s$ >>))$ >>
    | <:ctyp< $uid:s$ >> ->
         <:match_case< $uid:s$ -> $uid:s$ >>
    | _ -> assert False ]

  and match_case_of_poly_sum_type =
    fun
    [ <:ctyp< $t1$ | $t2$ >> ->
         <:match_case< $match_case_of_poly_sum_type t1$ | $match_case_of_poly_sum_type t2$ >>
    | <:ctyp< `$i$ of $t$ >> ->
         <:match_case< `$i$ x -> `$i$ $expr_of_ty (Some <:expr< x >>) t$ >>
    | <:ctyp< `$i$ >> ->
         <:match_case< `$i$ -> `$i$ >>
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
    | <:ctyp< $lid:i$ >> when i = tyid -> <:expr< fun x -> x >>
    | <:ctyp< $id:i$ >> as t ->
        let id1 = "_" ^ lid_of_ident "_" i in
        if id1 = tyid then <:expr< fun x -> x >>
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
    [ 1 -> <:ctyp< ! 'a 'b . ('a -> 'b) -> $t$ 'a -> $t$ 'b >>
    | 0 -> <:ctyp< $t$ -> $t$ >>
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
       [ <:str_item@_loc< class $lid:c$ = Camlp4Filters.GenerateMap.generated >> ->
            let x = <:class_str_item< $builtins$; $generated$ >> in
            <:str_item< class $lid:c$ = object (o) $x$ end >>
       | <:str_item@_loc< class $lid:c$ = Camlp4Filters.Camlp4MapGenerator.generated >> ->
            (* FIXME <:str_item< class $lid:c$ = object (o) $builtins$; $generated$ end >> *)
            let x = <:class_str_item< $builtins$; $generated$ >> in
            <:str_item< class $lid:c$ = object (o) $x$ end >>
       | s -> s ]))#str_item
  
  and inject_signature generated =
    (Ast.map_sig_item
      (fun
       [ <:sig_item@_loc< class $lid:c$ : Camlp4Filters.GenerateMap.generated >> ->
            <:sig_item< class $lid:c$ : object $generated$ end >>
       | <:sig_item@_loc< class $lid:c$ : Camlp4Filters.Camlp4MapGenerator.generated >> ->
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
