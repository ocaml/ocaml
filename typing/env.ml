(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Environment handling *)

open Format
open Config
open Misc
open Asttypes
open Longident
open Path
open Types


type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string

exception Error of error

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_exception of summary * Ident.t * exception_declaration
  | Env_module of summary * Ident.t * module_type
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_type
  | Env_open of summary * Path.t

type t = {
  values: (Path.t * value_description) Ident.tbl;
  constrs: constructor_description Ident.tbl;
  labels: label_description Ident.tbl;
  types: (Path.t * type_declaration) Ident.tbl;
  modules: (Path.t * module_type) Ident.tbl;
  modtypes: (Path.t * modtype_declaration) Ident.tbl;
  components: (Path.t * module_components) Ident.tbl;
  classes: (Path.t * class_type) Ident.tbl;
  summary: summary
}

and module_components =
    Structure_comps of structure_components
  | Functor_comps of functor_components

and structure_components = {
  mutable comp_values: (string, (value_description * int)) Tbl.t;
  mutable comp_constrs: (string, (constructor_description * int)) Tbl.t;
  mutable comp_labels: (string, (label_description * int)) Tbl.t;
  mutable comp_types: (string, (type_declaration * int)) Tbl.t;
  mutable comp_modules: (string, (module_type * int)) Tbl.t;
  mutable comp_modtypes: (string, (modtype_declaration * int)) Tbl.t;
  mutable comp_components: (string, (module_components * int)) Tbl.t;
  mutable comp_classes: (string, (class_type * int)) Tbl.t
}

and functor_components = {
  fcomp_param: Ident.t;
  fcomp_arg: module_type;
  fcomp_res: module_type;
  fcomp_env: t
}

let empty = {
  values = Ident.empty; constrs = Ident.empty;
  labels = Ident.empty; types = Ident.empty;
  modules = Ident.empty; modtypes = Ident.empty;
  components = Ident.empty; classes = Ident.empty;
  summary = Env_empty }

(* Persistent structure descriptions *)

type pers_struct =
  { ps_name: string;
    ps_sig: signature;
    ps_comps: module_components }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

let imported_units = ref ([] : (string * Digest.t) list)

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmi_magic_number) in
    really_input ic buffer 0 (String.length cmi_magic_number);
    if buffer <> cmi_magic_number then begin
      close_in ic;
      raise(Error(Not_an_interface filename))
    end;
    let ps = (input_value ic : pers_struct) in
    let crc = Digest.input ic in
    close_in ic;
    if ps.ps_name <> modname then
      raise(Error(Illegal_renaming(ps.ps_name, filename)));
    (ps, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_interface(filename)))

let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    let (ps, crc) =
      read_pers_struct name
        (find_in_path !load_path (String.uncapitalize name ^ ".cmi")) in
    Hashtbl.add persistent_structures name ps;
    imported_units := (name, crc) :: !imported_units;
    ps

let reset_cache() =
  Hashtbl.clear persistent_structures;
  imported_units := []

(* Forward declarations *)

let components_of_functor_appl =
  ref ((fun f p1 p2 -> fatal_error "Env.components_of_functor_appl") :
       functor_components -> Path.t -> Path.t -> module_components)

let check_modtype_inclusion =
  (* to be filled with includemod.check_modtype_inclusion *)
  ref ((fun env mty1 mty2 -> fatal_error "Env.include_modtypes") :
       t -> module_type -> module_type -> unit)

(* Lookup by identifier *)

let rec find_module_descr path env =
  match path with
    Pident id ->
      begin try
        let (p, desc) = Ident.find_same id env.components
        in desc
      with Not_found ->
        if Ident.persistent id
        then (find_pers_struct (Ident.name id)).ps_comps
        else raise Not_found
      end
  | Pdot(p, s, pos) ->
      begin match find_module_descr p env with
      	Structure_comps c ->
	  let (descr, pos) = Tbl.find s c.comp_components in
          descr
      | Functor_comps f ->
      	 raise Not_found
      end
  | Papply(p1, p2) ->
      begin match find_module_descr p1 env with
      	Functor_comps f ->
          !components_of_functor_appl f p1 p2
      | Structure_comps c ->
      	  raise Not_found
      end

let find proj1 proj2 path env =
  match path with
    Pident id ->
      let (p, data) = Ident.find_same id (proj1 env)
      in data
  | Pdot(p, s, pos) ->
      begin match find_module_descr p env with
        Structure_comps c ->
          let (data, pos) = Tbl.find s (proj2 c) in data
      | Functor_comps f ->
          raise Not_found
      end
  | Papply(p1, p2) ->
      raise Not_found

let find_value = find (fun env -> env.values) (fun sc -> sc.comp_values)
and find_type = find (fun env -> env.types) (fun sc -> sc.comp_types)
and find_modtype = find (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)
and find_class = find (fun env -> env.classes) (fun sc -> sc.comp_classes)

let find_module path env =
  match path with
    Pident id ->
      begin try
        let (p, data) = Ident.find_same id env.modules
        in data
      with Not_found ->
        if Ident.persistent id
        then Tmty_signature((find_pers_struct (Ident.name id)).ps_sig)
        else raise Not_found
      end
  | Pdot(p, s, pos) ->
      begin match find_module_descr p env with
        Structure_comps c ->
          let (data, pos) = Tbl.find s c.comp_modules in data
      | Functor_comps f ->
          raise Not_found
      end
  | Papply(p1, p2) ->
      raise Not_found (* not right *)

(* Lookup by name *)

let rec lookup_module_descr lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.components
      with Not_found ->
        (Pident(Ident.create_persistent s), (find_pers_struct s).ps_comps)
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr l env in
      begin match descr with
      	Structure_comps c ->
	  let (descr, pos) = Tbl.find s c.comp_components in
          (Pdot(p, s, pos), descr)
      | Functor_comps f ->
      	  raise Not_found
      end
  | Lapply(l1, l2) ->
      let (p1, desc1) = lookup_module_descr l1 env in
      let (p2, mty2) = lookup_module l2 env in
      begin match desc1 with
      	Functor_comps f ->
          !check_modtype_inclusion env mty2 f.fcomp_arg;
          (Papply(p1, p2), !components_of_functor_appl f p1 p2)
      | Structure_comps c ->
      	  raise Not_found
      end

and lookup_module lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.modules
      with Not_found ->
        (Pident(Ident.create_persistent s), 
         Tmty_signature(find_pers_struct s).ps_sig)
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr l env in
      begin match descr with
      	Structure_comps c ->
          let (data, pos) = Tbl.find s c.comp_modules in
          (Pdot(p, s, pos), data)
      | Functor_comps f ->
      	  raise Not_found
      end
  | Lapply(l1, l2) ->
      let (p1, desc1) = lookup_module_descr l1 env in
      let (p2, mty2) = lookup_module l2 env in
      let p = Papply(p1, p2) in
      begin match desc1 with
      	Functor_comps f ->
          !check_modtype_inclusion env mty2 f.fcomp_arg;
          (p, Subst.modtype (Subst.add_module f.fcomp_param p2 Subst.identity)
      	                    f.fcomp_res)
      | Structure_comps c ->
      	  raise Not_found
      end

let lookup proj1 proj2 lid env =
  match lid with
    Lident s ->
      Ident.find_name s (proj1 env)
  | Ldot(l, s) ->
      begin match lookup_module_descr l env with
      	(p, Structure_comps c) ->
	  let (data, pos) = Tbl.find s (proj2 c) in
          (Pdot(p, s, pos), data)
      | (p, Functor_comps f) ->
      	  raise Not_found
      end
  | Lapply(l1, l2) ->
      raise Not_found

let lookup_simple proj1 proj2 lid env =
  match lid with
    Lident s ->
      Ident.find_name s (proj1 env)
  | Ldot(l, s) ->
      begin match lookup_module_descr l env with
      	(p, Structure_comps c) ->
	  let (data, pos) = Tbl.find s (proj2 c) in
          data
      | (p, Functor_comps f) ->
      	  raise Not_found
      end
  | Lapply(l1, l2) ->
      raise Not_found

let lookup_value =
  lookup (fun env -> env.values) (fun sc -> sc.comp_values)
and lookup_constructor =
  lookup_simple (fun env -> env.constrs) (fun sc -> sc.comp_constrs)
and lookup_label =
  lookup_simple (fun env -> env.labels) (fun sc -> sc.comp_labels)
and lookup_type =
  lookup (fun env -> env.types) (fun sc -> sc.comp_types)
and lookup_modtype =
  lookup (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)
and lookup_class =
  lookup (fun env -> env.classes) (fun sc -> sc.comp_classes)

(* Scrape a module type *)

let rec scrape_modtype mty env =
  match mty with
    Tmty_ident path ->
      begin match find_modtype path env with
          Tmodtype_manifest mty -> scrape_modtype mty env
        | Tmodtype_abstract -> mty
      end
  | _ -> mty

(* Compute constructor descriptions *)

let constructors_of_type ty_path decl =
  match decl.type_kind with
    Type_variant cstrs ->
      Datarepr.constructor_descrs
        {desc = Tconstr(ty_path, decl.type_params, ref Mnil);
      	 level = -1 (* generic_level *)}
        cstrs
  | _ -> []

(* Compute label descriptions *)

let labels_of_type ty_path decl =
  match decl.type_kind with
    Type_record labels ->
      Datarepr.label_descrs
        {desc = Tconstr(ty_path, decl.type_params, ref Mnil);
         level = -1 (* generic_level *)}
        labels
  | _ -> []

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let rec prefix_idents root pos sub = function
    [] -> ([], sub)
  | Tsig_value(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let nextpos = match decl.val_kind with Val_prim _ -> pos | _ -> pos+1 in
      let (pl, final_sub) = prefix_idents root nextpos sub rem in
      (p::pl, final_sub)
  | Tsig_type(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | Tsig_exception(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) = prefix_idents root (pos+1) sub rem in
      (p::pl, final_sub)
  | Tsig_module(id, mty) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos+1) (Subst.add_module id p sub) rem in
      (p::pl, final_sub)
  | Tsig_modtype(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos
                      (Subst.add_modtype id (Tmty_ident p) sub) rem in
      (p::pl, final_sub)
  | Tsig_class(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos + 1) sub rem
      in
      (p::pl, final_sub)

(* Compute structure descriptions *)

let rec components_of_module env sub path mty =
  match scrape_modtype mty env with
    Tmty_signature sg ->
      let c =
        { comp_values = Tbl.empty; comp_constrs = Tbl.empty;
          comp_labels = Tbl.empty; comp_types = Tbl.empty;
          comp_modules = Tbl.empty; comp_modtypes = Tbl.empty;
          comp_components = Tbl.empty; comp_classes = Tbl.empty } in
      let (pl, sub) = prefix_idents path 0 sub sg in
      let env = ref env in
      let pos = ref 0 in
      List.iter2 (fun item path ->
        match item with
          Tsig_value(id, decl) ->
            let decl' = Subst.value_description sub decl in
            c.comp_values <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
            begin match decl.val_kind with
              Val_prim _ -> () | _ -> incr pos
            end
        | Tsig_type(id, decl) ->
            let decl' = Subst.type_declaration sub decl in
            c.comp_types <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_types;
            List.iter
              (fun (name, descr) ->
                c.comp_constrs <- Tbl.add name (descr, nopos) c.comp_constrs)
              (constructors_of_type path decl');
            List.iter
              (fun (name, descr) ->
                c.comp_labels <- Tbl.add name (descr, nopos) c.comp_labels)
              (labels_of_type path decl')
        | Tsig_exception(id, decl) ->
            let decl' = Subst.exception_declaration sub decl in
            let cstr = Datarepr.exception_descr path decl' in
            c.comp_constrs <-
              Tbl.add (Ident.name id) (cstr, !pos) c.comp_constrs;
            incr pos
        | Tsig_module(id, mty) ->
            let mty' = Subst.modtype sub mty in
            c.comp_modules <-
              Tbl.add (Ident.name id) (mty', !pos) c.comp_modules;
            let comps = components_of_module !env sub path mty in
            c.comp_components <-
              Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
            env := store_components id path comps !env;
            incr pos
        | Tsig_modtype(id, decl) ->
            let decl' = Subst.modtype_declaration sub decl in
            c.comp_modtypes <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_modtypes;
            env := store_modtype id path decl' !env
	| Tsig_class(id, decl) ->
	    let decl' = Subst.class_type sub decl in
            c.comp_classes <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_classes;
	    incr pos)
        sg pl;
	Structure_comps c
  | Tmty_functor(param, ty_arg, ty_res) ->
      	Functor_comps {
	  fcomp_param = param;
	  fcomp_arg = Subst.modtype sub ty_arg;
	  fcomp_res = Subst.modtype sub ty_res;
	  fcomp_env = env }
  | Tmty_ident p ->
      	Structure_comps {
      	  comp_values = Tbl.empty; comp_constrs = Tbl.empty;
          comp_labels = Tbl.empty; comp_types = Tbl.empty;
          comp_modules = Tbl.empty; comp_modtypes = Tbl.empty;
          comp_components = Tbl.empty; comp_classes = Tbl.empty }

(* Insertion of bindings by identifier + path *)

and store_value id path decl env =
  { values = Ident.add id (path, decl) env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components;
    classes = env.classes;
    summary = Env_value(env.summary, id, decl) }

and store_type id path info env =
  { values = env.values;
    constrs =
      List.fold_right
        (fun (name, descr) constrs ->
          Ident.add (Ident.create name) descr constrs)
        (constructors_of_type path info)
        env.constrs;
    labels =
      List.fold_right
        (fun (name, descr) labels ->
          Ident.add (Ident.create name) descr labels)
        (labels_of_type path info)
        env.labels;
    types = Ident.add id (path, info) env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components;
    classes = env.classes;
    summary = Env_type(env.summary, id, info) }

and store_exception id path decl env =
  { values = env.values;
    constrs = Ident.add id (Datarepr.exception_descr path decl) env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components;
    classes = env.classes;
    summary = Env_exception(env.summary, id, decl) }

and store_module id path mty env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = Ident.add id (path, mty) env.modules;
    modtypes = env.modtypes;
    components =
      Ident.add id (path, components_of_module env Subst.identity path mty)
                   env.components;
    classes = env.classes;
    summary = Env_module(env.summary, id, mty) }

and store_modtype id path info env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = Ident.add id (path, info) env.modtypes;
    components = env.components;
    classes = env.classes;
    summary = Env_modtype(env.summary, id, info) }

and store_components id path comps env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = Ident.add id (path, comps) env.components;
    classes = env.classes;
    summary = env.summary }

and store_class id path desc env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components;
    classes = Ident.add id (path, desc) env.classes;
    summary = Env_class(env.summary, id, desc) }

(* Memoized function to compute the components of a functor application
   in a path. *)

let funappl_memo =
  (Hashtbl.create 17 : (Path.t, module_components) Hashtbl.t)

let _ =
  components_of_functor_appl :=
    (fun f p1 p2 ->
      let p = Papply(p1, p2) in
      try
        Hashtbl.find funappl_memo p
      with Not_found ->
        let mty = 
          Subst.modtype (Subst.add_module f.fcomp_param p2 Subst.identity)
                        f.fcomp_res in
        let comps = components_of_module f.fcomp_env Subst.identity p mty in
        Hashtbl.add funappl_memo p comps;
        comps)

(* Insertion of bindings by identifier *)

let add_value id desc env =
  store_value id (Pident id) desc env

and add_type id info env =
  store_type id (Pident id) info env

and add_exception id decl env =
  store_exception id (Pident id) decl env

and add_module id mty env =
  store_module id (Pident id) mty env

and add_modtype id info env =
  store_modtype id (Pident id) info env

and add_class id ty env =
  store_class id (Pident id) ty env

(* Insertion of bindings by name *)

let enter store_fun name data env =
  let id = Ident.create name in (id, store_fun id (Pident id) data env)

let enter_value = enter store_value
and enter_type = enter store_type
and enter_exception = enter store_exception
and enter_module = enter store_module
and enter_modtype = enter store_modtype
and enter_class = enter store_class

(* Insertion of all components of a signature *)

let add_item comp env =
  match comp with
    Tsig_value(id, decl) -> add_value id decl env
  | Tsig_type(id, decl) -> add_type id decl env
  | Tsig_exception(id, decl) -> add_exception id decl env
  | Tsig_module(id, mty) -> add_module id mty env
  | Tsig_modtype(id, decl) -> add_modtype id decl env
  | Tsig_class(id, decl) -> add_class id decl env

let rec add_signature sg env =
  match sg with
    [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)

(* Open a signature path *)

let open_signature root sg env =
  (* First build the paths and substitution *)
  let (pl, sub) = prefix_idents root 0 Subst.identity sg in
  (* Then enter the components in the environment after substitution *)
  let newenv =
    List.fold_left2
      (fun env item p ->
        match item with
          Tsig_value(id, decl) ->
            store_value (Ident.hide id) p
                        (Subst.value_description sub decl) env
        | Tsig_type(id, decl) ->
            store_type (Ident.hide id) p
                       (Subst.type_declaration sub decl) env
        | Tsig_exception(id, decl) ->
            store_exception (Ident.hide id) p
                            (Subst.exception_declaration sub decl) env
        | Tsig_module(id, mty) ->
            store_module (Ident.hide id) p (Subst.modtype sub mty) env
        | Tsig_modtype(id, decl) ->
            store_modtype (Ident.hide id) p
                          (Subst.modtype_declaration sub decl) env
        | Tsig_class(id, decl) ->
            store_class (Ident.hide id) p
                        (Subst.class_type sub decl) env)
      env sg pl in
  { values = newenv.values;
    constrs = newenv.constrs;
    labels = newenv.labels;
    types = newenv.types;
    modules = newenv.modules;
    modtypes = newenv.modtypes;
    components = newenv.components;
    classes = newenv.classes;
    summary = Env_open(env.summary, root) }
  
(* Open a signature from a file *)

let open_pers_signature name env =
  let ps = find_pers_struct name in
  open_signature (Pident(Ident.create_persistent name)) ps.ps_sig env

(* Read a signature from a file *)

let read_signature modname filename =
  let (ps, crc) = read_pers_struct modname filename in (ps.ps_sig, crc)

(* Save a signature to a file *)

let save_signature sg modname filename =
  Types.cleanup_abbrev ();
  let ps =
    { ps_name = modname;
      ps_sig = sg;
      ps_comps =
        components_of_module empty Subst.identity
	    (Pident(Ident.create_persistent modname)) (Tmty_signature sg) } in
  let oc = open_out_bin filename in
  output_string oc cmi_magic_number;
  output_value oc ps;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc;
  crc

(* Make the initial environment *)

let initial = Predef.build_initial_env add_type add_exception empty

(* Return the list of imported interfaces with their CRCs *)

let imported_units() = !imported_units

(* Return the environment summary *)

let summary env = env.summary

(* Error report *)

let report_error = function
    Not_an_interface filename ->
      print_string filename; print_space();
      print_string "is not a compiled interface."
  | Corrupted_interface filename ->
      print_string "Corrupted compiled interface"; print_space();
      print_string filename
  | Illegal_renaming(modname, filename) ->
      print_string "Wrong file naming:"; print_space();
      print_string filename; print_space();
      print_string "contains the compiled interface for"; print_space();
      print_string modname

