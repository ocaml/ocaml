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

module H = struct
  type t = Path.t * string
  let equal v1 v2 = fst (v1 : t) = fst (v2 : t)
  let hash v = Hashtbl.hash (fst v)
end

module D = Weak.Make(H)

let digest_tbl = D.create 107

type digest_type_expr = string raw_type_expr

let rec type_expr t =
  match t.desc with
  | Tvar -> (Obj.magic t : digest_type_expr)
  | Tarrow (l,t1,t2) -> {desc= Tarrow("",type_expr t1, type_expr t2)}
  | Ttuple ts -> {desc= Ttuple (List.map type_expr ts)}
  | Tconstr ((p,d), ts) ->
      match d.type_manifest with
      | Some t -> type_expr (subst (List.combine d.type_params ts) t)
      | None -> {desc= Tconstr(digest p d, List.map type_expr ts)}

and digest path decl =
  try
    snd (D.find digest_tbl (path, ""))
  with
  | Not_found ->
      let recref = path, Path.name path in
      D.add digest_tbl recref; (* to avoid loops *)
      let decl = type_declaration decl in
      prerr_endline ("CALC " ^ Path.name path);
      let md5 = "" in
      D.remove digest_tbl recref;
      D.add digest_tbl (path, md5);
      md5
  
and type_declaration td =
  { type_params= List.map type_expr td.type_params;
    type_arity= td.type_arity;
    type_kind= type_kind td.type_kind;
    type_manifest= begin
      if td.type_kind = Type_abstract then 
	match td.type_manifest with
	| Some t -> Some (type_expr t)
	| None -> None
      else None
    end;
    type_variance= [] }

and type_kind = function
  | Type_abstract -> Type_abstract
  | Type_variant (sargss, priv) -> 
      Type_variant 
	(List.map (fun (n,ts) -> "", List.map type_expr ts) sargss,
	 Public)
  | Type_record (lmts, repr, priv) ->
      Type_record 
	(List.map (fun (l,m,t) -> ("", Immutable, type_expr t)) lmts,
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

