open Types
open Btype
open Ctype
open Longident

let split t =
  let t = repr t in
  match t.desc with
  | Tkonst (konst, ty) -> konst, ty
  | _ -> [], t

let konst t = fst (split t)
let body t = snd (split t) 

let type_variables tyl =
  let tvars = ref [] in
  let rec f ty =
    match ty.desc with
    | Tvar -> if not (List.memq ty !tvars) then tvars := ty :: !tvars
    | _ -> iter_type_expr f ty
  in
  List.iter f tyl;
  List.rev !tvars

(* normalize the konstraints *)
let normalize_konstraint konst ty =
  let tvars = type_variables [ty] in
  List.filter (fun t -> List.memq t konst) tvars

let type_of_generic_primitive_compilation env konst ty =
  let obj_path, _ = Env.lookup_type (Ldot (Lident "Obj", "t")) env in
  let type_path, _ = Env.lookup_type (Ldot (Lident "Rtype", "type_expr")) env in 
  let obj_t = Ctype.newty (Tconstr (obj_path, [], ref Mnil)) in
  let type_t = Ctype.newty (Tconstr (type_path, [], ref Mnil)) in
  begin_def ();
  let ty', konst' = 
    let ts = Ctype.instance_list (ty :: konst) in
    List.hd ts, List.tl ts
  in
  List.iter (fun v -> Ctype.unify env v obj_t) konst';
  let ty'' = List.fold_left (fun t var ->
    Ctype.newty (Tarrow ("", type_t, t, Cok)))
      ty' konst'
  in
  end_def ();
  generalize ty'';
  ty''

(* type abstraction *)
let type_abstraction konst t =
  let t = repr t in
  let konst = normalize_konstraint konst t in
  konst

let type_abstraction_of_value vdesc =
  match vdesc.val_type.desc with
  | Tkonst (konst, ty) -> type_abstraction konst ty
  | _ -> []

(* association from type variables to identifiers *)

module TVAR = struct
  type t = type_expr * Ident.t
  let equal (t1,_) (t2,_) = t1 == t2
  let hash (t,_) = t.id
end

let ident_of_type_variable, find_ident_of_type_variable = 
  let dummyid = Ident.create "dummy" in
  let module TBL = Weak.Make(TVAR) in
  let tbl = TBL.create 127 in
  let find_ident_of_type_variable tvar =
    let data = tvar, dummyid in
    let _, id = TBL.find tbl data in 
    id
  in
  (fun tvar ->
    assert (tvar.desc = Tvar);
    try
      find_ident_of_type_variable tvar
    with
    | Not_found ->
	let name = "*t" ^ string_of_int tvar.id ^ "*" in
	let id = Ident.create name in
	TBL.add tbl (tvar, id);
	id),
  find_ident_of_type_variable
