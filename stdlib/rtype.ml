(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module Ident = struct
  type t = string * int
end

module Path = struct
  type t =
      Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t

  let rec name = function
    | Pident (n,p) -> n ^ "_" ^ string_of_int p
    | Pdot (path,n,p) -> name path ^ "." ^ n ^ "_" ^ string_of_int p
    | Papply (p1,p2) -> name p1 ^ "(" ^ name p2 ^ ")"
end

type mutable_flag = Immutable | Mutable

type label = string

type private_flag = Private | Public

type record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

(* We need -rectypes! *)

type type_expr = (Path.t * type_declaration) raw_type_expr
and type_desc = (Path.t * type_declaration) raw_type_desc
and type_declaration = (Path.t * type_declaration) raw_type_declaration
and type_kind = (Path.t * type_declaration)  raw_type_kind

and 'a raw_type_expr =
  { (* mutable *) desc: 'a raw_type_desc; 
    (* mutable level: int; *)
    (* mutable id: int *) }

and 'a raw_type_desc =
    Tvar
  | Tarrow of label * 'a raw_type_expr * 'a raw_type_expr (* * commutable *)
  | Ttuple of 'a raw_type_expr list
  | Tconstr of 'a * 'a raw_type_expr list (* * abbrev_memo ref *)
(*
  | Tobject of 'a raw_type_expr * (Path.t * 'a raw_type_expr list) option ref
  | Tfield of string * field_kind * 'a raw_type_expr * 'a raw_type_expr
  | Tnil
  | Tlink of 'a raw_type_expr
  | Tsubst of 'a raw_type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar
  | Tpoly of 'a raw_type_expr * 'a raw_type_expr list
*)

(* Type definitions *)

and 'a raw_type_declaration =
  { type_name: string;
    type_params: 'a raw_type_expr list;
    type_arity: int;
    type_kind: 'a raw_type_kind;
    type_manifest: 'a raw_type_expr option;
    type_variance: (bool * bool * bool) list;
    type_defined_with: 'a list
  }
            (* covariant, contravariant, weakly contravariant *)

and 'a raw_type_kind =
    Type_abstract
  | Type_variant of (string * 'a raw_type_expr list) list * private_flag
  | Type_record of (string * mutable_flag * 'a raw_type_expr) list
                 * record_representation * private_flag

let mk_type desc = { desc= desc }

(* type equality *)
let rec raw_equal f t1 t2 =
  if t1 == t2 then true
  else equal_desc f t1.desc t2.desc
and equal_desc f d1 d2 =
  match d1, d2 with
  | Tarrow (l1, t11, t12), Tarrow (l2, t21, t22) ->
      l1 = l2 && raw_equal f t11 t21 && raw_equal f t12 t22
  | Ttuple ts1, Ttuple ts2 when List.length ts1 = List.length ts2 ->
      List.for_all2 (raw_equal f) ts1 ts2
  | Tconstr (v1, ts1), Tconstr (v2, ts2) ->
      if f v1 v2 then List.for_all2 (raw_equal f) ts1 ts2 else false
  | _ -> false

let equal = raw_equal (fun (p1,d1) (p2,d2) -> (* p1 = p2 && *) d1 == d2)
  (* Paths may be different. ex:
       module M = struct
         type t 
         let t = Gcaml.type_of_dyn (Gcaml.dyn 1) (* "t" *)
       end
       [: M.t :] (* "M.t" *)
   *)

(* substitution *)

let rec raw_subst s t = 
  try
    List.assq t s
  with
  | Not_found -> 
      match t.desc with
      | Tvar -> t
      | Tarrow (l,t1,t2) -> 
	  let t1' = raw_subst s t1
	  and t2' = raw_subst s t2
	  in
	  if t1 == t1' && t2 == t2' then t 
	  else {desc= Tarrow(l,t1',t2')}
      | Ttuple ts ->
	  let ts' = List.map (raw_subst s) ts in
	  if List.for_all2 (==) ts ts' then t
	  else {desc= Ttuple ts'}
      | Tconstr (v, ts) ->
	  let ts'= List.map (raw_subst s) ts in
	  if List.for_all2 (==) ts ts' then t
	  else {desc= Tconstr (v, ts')}

let subst = raw_subst

(* extraction of attached information (i.e. paths) *)
let attached_info t = 
  let lst = ref [] in
  let rec aux t =
    match t.desc with
    | Tvar -> ()
    | Tarrow (_,t1,t2) -> aux t1; aux t2
    | Ttuple ts -> List.iter aux ts
    | Tconstr (p, ts) -> 
	if not (List.mem p !lst) then lst := p :: !lst;
	List.iter aux ts
  in
  aux t;
  !lst

(* type iterator *)

let iter_type_expr f ty =
  match ty.desc with
    Tvar                -> ()
  | Tarrow (_, ty1, ty2) -> f ty1; f ty2
  | Ttuple l            -> List.iter f l
  | Tconstr (_, l)      -> List.iter f l
(*
  | Tobject(ty, {contents = Some (_, p)})
                        -> f ty; List.iter f p
  | Tobject (ty, _)     -> f ty
  | Tvariant row        -> iter_row f row; f (row_more row)
  | Tfield (_, _, ty1, ty2) -> f ty1; f ty2
  | Tnil                -> ()
  | Tlink ty            -> f ty
  | Tsubst ty           -> f ty
  | Tunivar             -> ()
  | Tpoly (ty, tyl)     -> f ty; List.iter f tyl
  | Tkonst (konst, ty)  -> 
      List.iter (fun kelem -> 
	f kelem.ktype; 
	Misc.may f kelem.kdepend) konst;
      f ty
  | Toverload odesc     -> f odesc.over_aunif; List.iter f odesc.over_cases
  | Tpath _             -> ()
*)

(* built-in types *)

let int = (Obj.magic Builtintypes.int : type_declaration)
let char = (Obj.magic Builtintypes.char : type_declaration)
let string = (Obj.magic Builtintypes.string : type_declaration)
let float = (Obj.magic Builtintypes.float : type_declaration)
let bool = (Obj.magic Builtintypes.bool : type_declaration)
let unit = (Obj.magic Builtintypes.unit : type_declaration)
let exn = (Obj.magic Builtintypes.exn : type_declaration)
let array = (Obj.magic Builtintypes.array : type_declaration)
let list = (Obj.magic Builtintypes.list : type_declaration)
let format4 = (Obj.magic Builtintypes.format4 : type_declaration)
let option = (Obj.magic Builtintypes.option : type_declaration)
let nativeint = (Obj.magic Builtintypes.nativeint : type_declaration)
let int32 = (Obj.magic Builtintypes.int32 : type_declaration)
let int64 = (Obj.magic Builtintypes.int64 : type_declaration)
let lazy_t = (Obj.magic Builtintypes.lazy_t : type_declaration)

let builtin_types = [int; char; string; float; bool; unit; exn; array; list; format4; option; nativeint; int32; int64; lazy_t]

(* Print a type expression *)

open Format

(* From: Printyp *)
let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter)) 
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26) in
  incr name_counter;
  name

let name_of_type (t : 'a raw_type_expr) =
  (* A bit dirty hack with Obj.magic... *)
  let t = (Obj.magic t : type_expr) in
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let rec print_path ppf = function
  | Path.Pident (name,pos) -> fprintf ppf "%s(*%d*)" name pos
  | Path.Pdot (p, name, n) -> fprintf ppf "%a.%s(*%d*)" print_path p name n
  | Path.Papply (p1, p2) -> fprintf ppf "%a(%a)" print_path p1 print_path p2

(* From: Oprint.print_out_type *)
let rec raw_print f ppf ty = raw_print1 f ppf ty 

and raw_print1 f ppf ty = 
  match ty.desc with
  | Tarrow (lab, ty1, ty2) ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        (raw_print2 f) ty1 (raw_print1 f) ty2
  | _ -> raw_print2 f ppf ty

and raw_print2 f ppf ty =
  match ty.desc with
  | Ttuple tyl -> 
      fprintf ppf "@[<0>%a@]" 
	(raw_print_typlist (raw_print_simple f) " *") tyl
  | _ -> raw_print_simple f ppf ty

and raw_print_simple f ppf ty =
  match ty.desc with
  | Tconstr (v, tyl) ->
      fprintf ppf "@[%a%a@]" (raw_print_typargs f) tyl f v
  | Tvar -> 
      fprintf ppf "'%s" (name_of_type ty)
  | Tarrow (_,_,_) | Ttuple _ ->
      fprintf ppf "@[<1>(%a)@]" (raw_print f) ty

and raw_print_typlist raw_print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> raw_print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a" 
	raw_print_elem ty sep (raw_print_typlist raw_print_elem sep) tyl

and raw_print_typargs f ppf =
  function
    [] -> ()
  | [ty1] -> fprintf ppf "%a@ " (raw_print_simple f) ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " 
	(raw_print_typlist (raw_print f) ",") tyl

let rec print_list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

(* type declaration printing *)

let raw_print_type_declaration f ppf (kwdp, decl) =
  let rec print_type_declaration ppf (kwdp, decl) =
    (* We do not try to print something parsable. *) 
    let name = decl.type_name in
    let args = decl.type_params in (* it is not quite same *)
    let constraints = [] in
    let print_constraints ppf params =
  (*
      List.iter
        (fun (ty1, ty2) ->
           fprintf ppf "@ @[<2>constraint %a =@ %a@]" (raw_print f) ty1
             (raw_print f) ty2)
        params
  *)
      (* do nothing *) ()
    in
    let type_parameter ppf (ty, (co, cn, ct)) =
      fprintf ppf "%s%s%s%a" 
        (if not cn then "+" else "")
        (if not co then "-" else "")
        (if not ct then "*" else "")
        (raw_print f) ty
    in
    let type_defined ppf =
      let args = List.combine args decl.type_variance in 
      match args with
        [] -> fprintf ppf "%s" name
      | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
      | _ ->
          fprintf ppf "@[(@[%a)@]@ %s@]"
            (print_list type_parameter (fun ppf -> fprintf ppf ",@ ")) args name
    in
    let print_manifest ppf =
      function
        Some ty -> fprintf ppf " =@ %a" (raw_print f) ty
      | None -> ()
    in
    let print_name_args ppf =
      fprintf ppf "%t %t%a" kwdp type_defined print_manifest decl.type_manifest
    in
    let print_private ppf = function
      Private -> fprintf ppf "private "
    | Public -> () in
    let rec print_out_tkind = function
    | Type_abstract ->
        fprintf ppf "@[<2>@[<hv 2>%t@]%a@]" print_name_args print_constraints
          constraints
    | Type_record (lbls, _(*fixme*), priv) ->
        fprintf ppf "@[<2>@[<hv 2>%t = %a{%a@;<1 -2>}@]%a@]" print_name_args
          print_private priv
          (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
          print_constraints constraints
    | Type_variant (constrs, priv) ->
        fprintf ppf "@[<2>@[<hv 2>%t =@;<1 2>%a%a@]%a@]" print_name_args
          print_private priv
          (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
          print_constraints constraints
  (*
    | ty ->
        fprintf ppf "@[<2>@[<hv 2>%t =@ %a@]%a@]" print_name_args (raw_print f)
          ty print_constraints constraints
  *)
    in
    print_out_tkind decl.type_kind
  
  and print_out_constr ppf (name, tyl) =
    match tyl with
      [] -> fprintf ppf "%s" name
    | _ ->
        fprintf ppf "@[<2>%s of@ %a@]" name
          (raw_print_typlist (raw_print_simple f) " *") tyl
  
  and print_out_label ppf (name, mut, arg) =
    fprintf ppf "@[<2>%s%s :@ %a@];" (if mut = Mutable then "mutable " else "") name
      (raw_print f) arg
in
  print_type_declaration ppf (kwdp, decl)

let raw_print_type_declarations f ppf typedecls =
  fprintf ppf "@[";
  raw_print_type_declaration f ppf 
    ((fun ppf -> fprintf ppf "type"), List.hd typedecls);
  List.iter (fun decl ->
    raw_print_type_declaration f ppf 
      ((fun ppf -> fprintf ppf "@ and"), decl)) (List.tl typedecls);
  fprintf ppf "@]"

let print = raw_print (fun ppf (p,_) -> print_path ppf p)
let print_type_declarations = 
  raw_print_type_declarations (fun ppf (p,_) -> print_path ppf p)

let print_related_type_declarations ppf decl =
  raw_print_type_declarations (fun ppf (p,_) -> print_path ppf p) ppf
    (decl :: (List.map snd decl.type_defined_with))

