(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Gilles Peskine, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for dynamic typing *)

open Asttypes
open Types
open Typedynt
open Typedtree
open Longident
open Lambda

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IntMap = Map.Make(OrderedInt)

(* Get dynamics primitives identifiers *)
(* Adapted from Translobj.oo_prim. *)
let dynamics_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Dynamics", name)) Env.empty))
  with Not_found ->
    Misc.fatal_error ("Primitive Dynamics." ^ name ^ " not found.")

let dynamics_type loc name =
  let sty =
    { Parsetree.ptyp_desc =
        Parsetree.Ptyp_constr (Ldot (Lident "Dynamics", "type_data"), []);
      Parsetree.ptyp_loc = loc }
  in
  Typetexp.transl_simple_type Env.empty true sty


exception Unimplemented of string


let var_path_name path =
  let rec aux acc = function
    | Path.Pident ident ->
        Ident.unique_name ident :: acc
    | Path.Pdot (path, string, _) ->
        aux (string :: acc) path
    | Path.Papply _ -> assert false
  in
  String.concat ":" (aux [] path)

let extract_type_definitions whole_env ty0 =
  let r_env = ref Env.empty and r_sig = ref [] in
  let rec all ty =
    let ty1 = Ctype.correct_levels ty in
    Ctype.normalize_type whole_env ty1;
    Btype.iter_type_paths one ty1;
    ty1
  and one path =
    if Predef.is_predef_type_path path then path else
    let name = Path.unique_name path in
    let id = Ident.create_persistent name in
    let path' = Path.Pident id in
    begin try
      let _ = Env.find_type path' !r_env in ()
        ; Printf.eprintf "  (saw         %s)\n" (var_path_name path); flush stderr;
    with Not_found ->
      Printf.eprintf "  (encountered %s, munged as %s)\n" (var_path_name path) (var_path_name path'); flush stderr;
      let decl =
        Env.find_type path whole_env
      in
      r_env := Env.add_type id decl !r_env;
      let decl' =
        { type_params = decl.type_params;
          type_arity = decl.type_arity;
          type_kind = Type_abstract;
          type_manifest =
            begin match decl.type_manifest with
            | Some ty -> Some (all ty)
            | None ->
                match decl.type_kind with
                | Type_abstract ->
                    raise (Unimplemented
                             ("Dynamicisation involving an abstract type: " ^
                              name))
                | Type_variant cases ->
                    let constructor_names = List.map fst cases in
                    let prefix = String.concat "+" constructor_names ^ "-" in
                    let row = {
                      row_fields = polymorphise_variants prefix 0 [] cases;
                      row_more = Btype.newgenvar ();
                      row_bound = [];
                      row_closed = true;
                      row_name = None;
                    } in
                    Some (Btype.newgenty (Tvariant row))
                | Type_record (record_fields, _) ->
                    let get_name (name, mutabl, _) =
                      match mutabl with
                      | Asttypes.Mutable -> "!" ^ name
                      | Asttypes.Immutable -> name
                    in
                    let field_names = List.map get_name record_fields in
                    let prefix = String.concat "+" field_names ^ "-" in
                    let empty = Btype.newgenty Tnil in
                    let object_fields =
                      object_of_record prefix 0 empty record_fields
                    in
                    Some (Btype.newgenty (Tobject (object_fields, ref None)))
            end;
          type_variance = decl.type_variance }
      in
      r_sig := Tsig_type (id, decl') :: !r_sig
    end;
    path'
  and object_of_record prefix n object_type = function
    | [] -> object_type
    | (_, _, ty) :: tail ->
        let ty' = all ty in
        let new_object_type =
          Btype.newgenty (Tfield (prefix ^ string_of_int n,
                                  Fpresent,
                                  ty',
                                  object_type))
        in
        object_of_record prefix (n + 1) new_object_type tail
  and polymorphise_variants prefix n acc = function
    | [] -> acc
    | (_, args_list) :: tail ->
        let args_desc = Ttuple (List.map all args_list) in
        let row_field =
          (prefix ^ string_of_int n,
           Rpresent (Some (Btype.newgenty args_desc)))
        in
        polymorphise_variants prefix (n + 1) (row_field :: acc) tail
  in
  let ty1 = all ty0 in
  let clean = true in
  let vars = if clean then Ctype.free_type_variables ty1 else [] in
  if vars <> [] then raise (Unimplemented "dynamic of a polymorphic value cannot be typed in core Caml");
  (* TODO: check that each of the [vars] is generalisable (i.e., not '_a) *)
  let decl =
    { type_params = if clean then vars else [];
      type_arity = if clean then 0 else List.length vars;
      type_kind = Type_abstract;
      type_manifest = Some ty1;
      type_variance = if clean then List.map (fun _ -> true, true) vars (*??*) else [] }
  in
  Tsig_type (interesting_ident, decl) :: !r_sig


(* From a type expression, produce code that builds a value that describes
   this type expression. Said value has the type Dynamics.type_bytes. *)
let make_type_repr_code whole_env ty0 =
  Printf.eprintf "<Transldyn.make_type_repr_code>\n"; flush stderr;
  let sg = extract_type_definitions whole_env ty0 in
  let bytes =
    Marshal.to_string (sg : reified_type_data) []
  in
  Printf.eprintf "</Transldyn.make_type_repr_code>\n"; flush stderr;
  Lconst (Const_base (Const_string bytes))


(* From a module type expression, produce code that builds a value that
   describes this module type expression. Said value has the type
   Dynamics.module_type_repr. *)
let make_sig_repr_code env mty0 =
  (*TODO*)
  Lconst (Const_pointer 0)
