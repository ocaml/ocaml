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

open Rtype
open Path

(* Simplification of types and type declaration to obtain 
   data representation *)

(* What we ignore:
     * function labels
     * manifest definition (when definition is available)
     * variance
     * constructor and record labels
     * private flags
     * mutable flags

   Expansion
     * manifest types
*)

(* FIXME
     * cyclic data types are not supported 
*)

type fingerprint = 
  | BuiltinBase of string 
  | Digest of Digest.t 
  | Recursive of string

module H = struct
  type t = string * fingerprint
  let equal v1 v2 = fst (v1 : t) == fst (v2 : t)
  let hash v = Hashtbl.hash (fst v)
end

module D = Weak.Make(H)

let fingerprint_tbl = D.create 107

type fingerprint_type_expr = fingerprint raw_type_expr

type type_signature = fingerprint raw_type_declaration list

let rec type_expr t =
  match t.desc with
  | Tvar -> (Obj.magic t : fingerprint_type_expr)
  | Tarrow (l,t1,t2) -> {desc= Tarrow("",type_expr t1, type_expr t2)}
  | Ttuple ts -> {desc= Ttuple (List.map type_expr ts)}
  | Tconstr ((_,d), ts) ->
      match d.type_manifest with
      | Some t -> 
	  begin try
	    type_expr (subst (List.combine d.type_params ts) t)
	  with Invalid_argument _ -> 
	    (* It must be a bug. call Jun *)
	    assert false
	  end
      | None -> {desc= Tconstr(fingerprint d, List.map type_expr ts)}

and fingerprint decl =
  try
    snd (D.find fingerprint_tbl (decl.type_name, Digest ""))
  with
  | Not_found ->
      let fingerprint = make_fingerprint decl in
      D.add fingerprint_tbl (decl.type_name, fingerprint);
      fingerprint
  
and make_fingerprint decl =
  if List.memq decl Rtype.builtin_types && decl.type_kind = Type_abstract then
    BuiltinBase decl.type_name
  else
    Digest (type_digest decl)

and type_digest decl = 
  Digest.string (string_of_type_signature decl)

and type_signature (decl : type_declaration) =
  let decls = decl :: List.map snd decl.type_defined_with in
  let recrefs =
    let pos = ref 0 in
    List.map (fun decl -> 
      incr pos;
      decl.type_name, Recursive ("#" ^ string_of_int !pos)) decls
  in
  List.iter (D.add fingerprint_tbl) recrefs; (* to avoid loops *)
  let decls = 
    let pos = ref 0 in
    List.map (fun decl -> 
      incr pos;
      type_declaration ("#" ^ string_of_int !pos) decl) decls
  in
  List.iter (D.remove fingerprint_tbl) recrefs;
  decls

and print_type_signature ppf decl = 
  let decls = type_signature decl in
  let printer ppf = function
    | BuiltinBase n -> Format.fprintf ppf "base(%s)" n
    | Digest d -> Format.fprintf ppf "%s" (Digest.to_hex d)
    | Recursive n -> Format.fprintf ppf "%s" n
  in
  Rtype.raw_print_type_declarations printer ppf decls

and string_of_type_signature decl =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  let out, flush, _, spaces = Format.pp_get_all_formatter_output_functions ppf () in
  Format.pp_set_all_formatter_output_functions
    ppf ~out ~flush ~newline:(fun () -> ()) ~spaces:(fun n -> spaces 1);
  print_type_signature ppf decl;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

and type_declaration name td =
  { type_name= name;
    type_params= List.map type_expr td.type_params;
    type_arity= td.type_arity;
    type_kind= type_kind td.type_kind;
    type_manifest= begin
      if td.type_kind = Type_abstract then 
	match td.type_manifest with
	| Some t -> Some (type_expr t)
	| None -> None
      else None
    end;
    type_variance= List.map (fun x -> (true,true,true)) td.type_params;
    type_defined_with= []
  }

and type_kind = function
  | Type_abstract -> Type_abstract
  | Type_variant (sargss, priv) -> 
      Type_variant 
	(List.map (fun (n,ts) -> "_", List.map type_expr ts) sargss,
	 Public)
  | Type_record (lmts, repr, priv) ->
      Type_record 
	(List.map (fun (l,m,t) -> ("_", Immutable, type_expr t)) lmts,
	 repr,
	 Public)

generic val output_value : {'a} => out_channel -> 'a -> unit =
  fun ty oc v -> 
    let ty = type_expr ty in
    Pervasives.output_value oc (ty,v)

generic val input_value : {'a} => in_channel -> 'a =
  fun ty ic ->
    (* We suppose that the data is written by output_value.
       Otherwise, it is not safe at all! *)
    let (ty',v) = Pervasives.input_value ic in
    let ty = type_expr ty in
    (* check compatibility between ty and ty' *)
    if raw_equal (=) ty ty' then v else raise Exit
