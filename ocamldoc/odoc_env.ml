(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(** Environment for finding complete names from relative names. *)

let print_DEBUG s = print_string s ; print_newline ();;

module Name = Odoc_name

(** relative name * complete name *)
type env_element = Name.t * Name.t

type env = {
    env_values : env_element list ;
    env_types : env_element list ;
    env_class_types : env_element list ;
    env_classes : env_element list ;
    env_modules : env_element list ;
    env_module_types : env_element list ;
    env_exceptions : env_element list ;
  } 

let empty = {
  env_values = [] ; 
  env_types = [] ; 
  env_class_types = [] ; 
  env_classes = [] ; 
  env_modules = [] ; 
  env_module_types = [] ; 
  env_exceptions = [] ; 
  } 

(** Add a signature to an environment.  *)
let rec add_signature env root ?rel signat =
  let qualify id = Name.concat root (Name.from_ident id) in
  let rel_name id = 
    let n = Name.from_ident id in
    match rel with
      None -> n
    | Some r -> Name.concat r n
  in
  let f env item =
    match item with
      Types.Tsig_value (ident, _) -> { env with env_values = (rel_name ident, qualify ident) :: env.env_values }
    | Types.Tsig_type (ident,_ ) -> { env with env_types = (rel_name ident, qualify ident) :: env.env_types }
    | Types.Tsig_exception (ident, _) -> { env with env_exceptions = (rel_name ident, qualify ident) :: env.env_exceptions }
    | Types.Tsig_module (ident, modtype) -> 
	let env2 = 
	  match modtype with (* A VOIR : le cas où c'est un identificateur, dans ce cas on n'a pas de signature *)
	    Types.Tmty_signature s -> add_signature env (qualify ident) ~rel: (rel_name ident) s
	  |  _ -> env
	in
	{ env2 with env_modules = (rel_name ident, qualify ident) :: env2.env_modules }
    | Types.Tsig_modtype (ident, modtype_decl) -> 
	let env2 =
	  match modtype_decl with
	    Types.Tmodtype_abstract ->
	      env 
	  | Types.Tmodtype_manifest modtype ->
	      match modtype with
		 (* A VOIR : le cas où c'est un identificateur, dans ce cas on n'a pas de signature *)
		Types.Tmty_signature s -> add_signature env (qualify ident) ~rel: (rel_name ident) s
	      |  _ -> env
	in
	{ env2 with env_module_types = (rel_name ident, qualify ident) :: env2.env_module_types }
    | Types.Tsig_class (ident, _) -> { env with env_classes = (rel_name ident, qualify ident) :: env.env_classes }
    | Types.Tsig_cltype (ident, _) -> { env with env_class_types = (rel_name ident, qualify ident) :: env.env_class_types }
  in
  List.fold_left f env signat 

let add_exception env full_name =
  let simple_name = Name.simple full_name in
  { env with env_exceptions = (simple_name, full_name) :: env.env_exceptions }

let add_type env full_name =
  let simple_name = Name.simple full_name in
  { env with env_types = (simple_name, full_name) :: env.env_types }

let add_value env full_name =
  let simple_name = Name.simple full_name in
  { env with env_values = (simple_name, full_name) :: env.env_values }

let add_module env full_name =
  let simple_name = Name.simple full_name in
  { env with env_modules = (simple_name, full_name) :: env.env_modules }

let add_module_type env full_name =
  let simple_name = Name.simple full_name in
  { env with env_module_types = (simple_name, full_name) :: env.env_module_types }

let add_class env full_name =
  let simple_name = Name.simple full_name in
  { env with 
    env_classes = (simple_name, full_name) :: env.env_classes ;
    (* we also add a type 'cause the class name may appear as a type *)
    env_types = (simple_name, full_name) :: env.env_types
  }
    
let add_class_type env full_name =
  let simple_name = Name.simple full_name in
  { env with 
    env_class_types = (simple_name, full_name) :: env.env_class_types ; 
    (* we also add a type 'cause the class type name may appear as a type *)
    env_types = (simple_name, full_name) :: env.env_types
  }
    
let full_module_name env n =
  try List.assoc n env.env_modules
  with Not_found ->
    print_DEBUG ("Module "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_modules;
    n

let full_module_type_name env n =
  try List.assoc n env.env_module_types
  with Not_found -> 
    print_DEBUG ("Module "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_modules;
    n

let full_module_or_module_type_name env n =
  try List.assoc n env.env_modules
  with Not_found -> full_module_type_name env n

let full_type_name env n =
  try 
    let full = List.assoc n env.env_types in
(**    print_string ("type "^n^" is "^full);
    print_newline ();*)
    full
  with Not_found -> 
(**    print_string ("type "^n^" not found");
    print_newline ();*)
    n
      
let full_value_name env n =
  try List.assoc n env.env_values
  with Not_found -> n

let full_exception_name env n =
  try List.assoc n env.env_exceptions
  with Not_found ->
    print_DEBUG ("Exception "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_exceptions;
    n

let full_class_name env n =
  try List.assoc n env.env_classes
  with Not_found -> 
    print_DEBUG ("Class "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_classes;
    n

let full_class_type_name env n =
  try List.assoc n env.env_class_types
  with Not_found -> 
    print_DEBUG ("Class type "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_class_types;
    n

let full_class_or_class_type_name env n =
  try List.assoc n env.env_classes
  with Not_found -> full_class_type_name env n

let subst_type env t =
(**  print_string "Odoc_env.subst_type";
   print_newline ();
*)
  Printtyp.mark_loops t;
  let rec iter deja_vu t =
    let (new_desc, new_deja_vu) = 
      if List.memq t deja_vu then
	(t.Types.desc, deja_vu)
      else
	let dv = t :: deja_vu in
	match t.Types.desc with
	| Types.Tvar -> 
	    (Types.Tvar, dv)

	| Types.Tarrow (l, t1, t2, c) ->
	    let (t1', dv1) = iter dv t1 in
	    let (t2', dv2) = iter dv1 t2 in
	    (Types.Tarrow (l, t1', t2', c), dv2)

	| Types.Ttuple l -> 
	    let (l', dv') =
	      List.fold_left 
		(fun (acc_t, acc_dv) -> fun t ->
		  let (new_t, new_dv) = iter acc_dv t in
		  (acc_t @ [new_t], new_dv)
		)
		([], dv)
		l
	    in
	    (Types.Ttuple l', dv')

	| Types.Tconstr (p, [ty], a) when Path.same p Predef.path_option ->
	    let (ty', dv') = iter dv ty in
	    (Types.Tconstr (p, [ty'], a), dv')

	| Types.Tconstr (p, l, a) ->
	    let new_p = Odoc_name.to_path (full_type_name env (Odoc_name.from_path p)) in
	    let (l', dv') =
	      List.fold_left 
		(fun (acc_t, acc_dv) -> fun t ->
		  let (new_t, new_dv) = iter acc_dv t in
		  (acc_t @ [new_t], new_dv)
		)
		([], dv)
		l
	    in
	    (Types.Tconstr (new_p, l', a), dv')

	| Types.Tobject (t2, r) ->
	    (* A VOIR : descendre dans r ? *)
	    let (t2', dv') = iter dv t2 in
	    (Types.Tobject (t2', r), dv')

	| Types.Tfield (s, fk, t1, t2) ->
	    let (t1', dv1) = iter dv t1 in
	    let (t2', dv2) = iter dv1 t2 in
	    (Types.Tfield (s, fk, t1', t2'), dv2)

	| Types.Tnil ->
	    (Types.Tnil, dv)

	| Types.Tlink t2 -> 
	    let (t2', dv') = iter dv t2 in
	    (Types.Tlink t2', dv')

	| Types.Tsubst t2 ->
	    let (t2', dv') = iter dv t2 in
	    (Types.Tsubst t2', dv')

	| Types.Tvariant rd ->
	    (* A VOIR : est-ce la peine de descendre dans rd ? *)
	    (Types.Tvariant rd, dv)
    in
    t.Types.desc <- new_desc;
    (t, new_deja_vu)
  in
  let (res, _) = iter [] t in
(**  print_string "Odoc_env.subst_type fini";
   print_newline ();
*)
  res

let subst_module_type env t =
  let rec iter t =
    match t with
      Types.Tmty_ident p ->
	let new_p = Odoc_name.to_path (full_module_type_name env (Odoc_name.from_path p)) in
	Types.Tmty_ident new_p
    | Types.Tmty_signature _ ->
	t
    | Types.Tmty_functor (id, mt1, mt2) ->
	Types.Tmty_functor (id, iter mt1, iter mt2)
  in
  iter t


    
