(* Environment handling *)

open Format
open Config
open Misc
open Asttypes
open Longident
open Path
open Typedtree


type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string

exception Error of error

type t = {
  values: (Path.t * value_description) Ident.tbl;
  constrs: constructor_description Ident.tbl;
  labels: label_description Ident.tbl;
  types: (Path.t * type_declaration) Ident.tbl;
  modules: (Path.t * module_type) Ident.tbl;
  modtypes: (Path.t * modtype_declaration) Ident.tbl;
  components: (Path.t * structure_components) Ident.tbl
}

and structure_components = {
  mutable comp_values: (string, (value_description * int)) Tbl.t;
  mutable comp_constrs: (string, (constructor_description * int)) Tbl.t;
  mutable comp_labels: (string, (label_description * int)) Tbl.t;
  mutable comp_types: (string, (type_declaration * int)) Tbl.t;
  mutable comp_modules: (string, (module_type * int)) Tbl.t;
  mutable comp_modtypes: (string, (modtype_declaration * int)) Tbl.t;
  mutable comp_components: (string, (structure_components * int)) Tbl.t
}

let empty = {
  values = Ident.empty; constrs = Ident.empty;
  labels = Ident.empty; types = Ident.empty;
  modules = Ident.empty; modtypes = Ident.empty;
  components = Ident.empty }

(* Persistent structure descriptions *)

type pers_struct =
  { ps_name: string;
    ps_crc: int;
    ps_sig: signature;
    ps_comps: structure_components }

let persistent_structures =
  (Hashtbl.new 17 : (string, pers_struct) Hashtbl.t)

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmi_magic_number) in
    really_input ic buffer 0 (String.length cmi_magic_number);
    if buffer <> cmi_magic_number then raise(Error(Not_an_interface filename));
    let ps = (input_value ic : pers_struct) in
    if ps.ps_name <> modname then
      raise(Error(Illegal_renaming(modname, filename)));
    ps
  with End_of_file | Failure _ ->
    raise(Error(Corrupted_interface(filename)))

let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    let ps =
      read_pers_struct name
        (find_in_path !load_path (lowercase name ^ ".cmi")) in
    Hashtbl.add persistent_structures name ps;
    ps

let reset_cache() =
  Hashtbl.clear persistent_structures

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
      let descr_p = find_module_descr p env in
      let (descr, pos) = Tbl.find s descr_p.comp_components in
      descr

let find proj1 proj2 path env =
  try
    match path with
      Pident id ->
        let (p, data) = Ident.find_same id (proj1 env)
        in data
    | Pdot(p, s, pos) ->
        let (data, pos) = Tbl.find s (proj2 (find_module_descr p env))
        in data
  with Not_found ->
    fatal_error "Env.find"

let find_value = find (fun env -> env.values) (fun sc -> sc.comp_values)
and find_type = find (fun env -> env.types) (fun sc -> sc.comp_types)
and find_modtype = find (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)

(* Lookup by name *)

let rec lookup_module_descr lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.components
      with Not_found ->
        (Pident(Ident.new_persistent s), (find_pers_struct s).ps_comps)
      end
  | Ldot(p, s) ->
      let (path, descr_p) = lookup_module_descr p env in
      let (descr, pos) = Tbl.find s descr_p.comp_components in
      (Pdot(path, s, pos), descr)

let lookup proj1 proj2 lid env =
  match lid with
    Lident s ->
      Ident.find_name s (proj1 env)
  | Ldot(p, s) ->
      let (path, descr) = lookup_module_descr p env in
      let (data, pos) = Tbl.find s (proj2 descr) in
      (Pdot(path, s, pos), data)

let lookup_simple proj1 proj2 lid env =
  match lid with
    Lident s ->
      Ident.find_name s (proj1 env)
  | Ldot(p, s) ->
      let (path, descr) = lookup_module_descr p env in
      let (data, pos) = Tbl.find s (proj2 descr) in
      data

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

let lookup_module lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.modules
      with Not_found ->
        (Pident(Ident.new_persistent s), 
         Tmty_signature(find_pers_struct s).ps_sig)
      end
    | Ldot(p, s) ->
        let (path, descr) = lookup_module_descr p env in
        let (data, pos) = Tbl.find s descr.comp_modules in
        (Pdot(path, s, pos), data)

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
      let ty_res = Tconstr(ty_path, decl.type_params) in
      let num_consts = ref 0 and num_nonconsts = ref 0 in
      List.iter
        (function (name, []) -> incr num_consts
                | (name, _)  -> incr num_nonconsts)
        cstrs;
      let rec describe_constructors idx_const idx_nonconst = function
          [] -> []
        | (name, ty_args) :: rem ->
            let (tag, descr_rem) =
              match ty_args with
                [] -> (Cstr_constant idx_const,
                       describe_constructors (idx_const+1) idx_nonconst rem)
              | _  -> (Cstr_block idx_nonconst,
                       describe_constructors idx_const (idx_nonconst+1) rem) in
            let cstr =
              { cstr_res = ty_res;
                cstr_args = ty_args;
                cstr_arity = List.length ty_args;
                cstr_tag = tag;
                cstr_consts = !num_consts;
                cstr_nonconsts = !num_nonconsts } in
            (name, cstr) :: descr_rem in
      describe_constructors 0 0 cstrs
  | _ -> []

(* Compute a constructor description for an exception *)

let constructor_exception path_exc decl =
  { cstr_res = Predef.type_exn;
    cstr_args = decl;
    cstr_arity = List.length decl;
    cstr_tag = Cstr_exception path_exc;
    cstr_consts = -1;
    cstr_nonconsts = -1 }

(* Compute label descriptions *)

let dummy_label =
  { lbl_res = Ttuple []; lbl_arg = Ttuple []; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||] }

let labels_of_type ty_path decl =
  match decl.type_kind with
    Type_record labels ->
      let ty_res = Tconstr(ty_path, decl.type_params) in
      let all_labels = Array.new (List.length labels) dummy_label in
      let rec describe_labels num = function
          [] -> []
        | (name, mut_flag, ty_arg) :: rest ->
            let lbl =
              { lbl_res = ty_res;
                lbl_arg = ty_arg;
                lbl_mut = mut_flag;
                lbl_pos = num;
                lbl_all = all_labels } in
            all_labels.(num) <- lbl;
            (name, lbl) :: describe_labels (num+1) rest in
      describe_labels 0 labels
  | _ -> []

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let rec prefix_idents root pos sub = function
    [] -> ([], sub)
  | Tsig_value(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) = prefix_idents root (pos+1) sub rem in
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

(* Compute structure descriptions *)

let rec components_of_module env path mty =
  let c =
    { comp_values = Tbl.empty; comp_constrs = Tbl.empty;
      comp_labels = Tbl.empty; comp_types = Tbl.empty;
      comp_modules = Tbl.empty; comp_modtypes = Tbl.empty;
      comp_components = Tbl.empty } in
  begin match scrape_modtype mty env with
    Tmty_signature sg ->
      let (pl, sub) = prefix_idents path 0 Subst.identity sg in
      let env = ref env in
      let pos = ref 0 in
      List.iter2 (fun item path ->
        match item with
          Tsig_value(id, decl) ->
            let decl' = Subst.value_description sub decl in
            c.comp_values <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
            incr pos
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
            let cstr = constructor_exception path decl' in
            c.comp_constrs <-
              Tbl.add (Ident.name id) (cstr, !pos) c.comp_constrs;
            incr pos
        | Tsig_module(id, mty) ->
            let mty' = Subst.modtype sub mty in
            c.comp_modules <-
              Tbl.add (Ident.name id) (mty', !pos) c.comp_modules;
            let comps = components_of_module !env path mty' in
            c.comp_components <-
              Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
            env := store_components id path comps !env;
            incr pos
        | Tsig_modtype(id, decl) ->
            let decl' = Subst.modtype_declaration sub decl in
            c.comp_modtypes <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_modtypes;
            env := store_modtype id path decl' !env)
        sg pl
  | _ -> ()
  end;
  c

(* Insertion of bindings by identifier + path *)

and store_value id path decl env =
  { values = Ident.add id (path, decl) env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components }

and store_type id path info env =
  { values = env.values;
    constrs =
      List.fold_right
        (fun (name, descr) constrs ->
          Ident.add (Ident.new name) descr constrs)
        (constructors_of_type path info)
        env.constrs;
    labels =
      List.fold_right
        (fun (name, descr) labels ->
          Ident.add (Ident.new name) descr labels)
        (labels_of_type path info)
        env.labels;
    types = Ident.add id (path, info) env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components }

and store_exception id path decl env =
  { values = env.values;
    constrs = Ident.add id (constructor_exception path decl) env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = env.components }

and store_module id path mty env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = Ident.add id (path, mty) env.modules;
    modtypes = env.modtypes;
    components = Ident.add id (path, components_of_module env path mty)
                 env.components }

and store_modtype id path info env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = Ident.add id (path, info) env.modtypes;
    components = env.components }

and store_components id path comps env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    modtypes = env.modtypes;
    components = Ident.add id (path, comps) env.components }

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

(* Insertion of bindings by name *)

let enter store_fun name data env =
  let id = Ident.new name in (id, store_fun id (Pident id) data env)

let enter_value = enter store_value
and enter_type = enter store_type
and enter_exception = enter store_exception
and enter_module = enter store_module
and enter_modtype = enter store_modtype

(* Insertion of all components of a signature *)

let add_signature_component comp env =
  match comp with
    Tsig_value(id, decl) -> add_value id decl env
  | Tsig_type(id, decl) -> add_type id decl env
  | Tsig_exception(id, decl) -> add_exception id decl env
  | Tsig_module(id, mty) -> add_module id mty env
  | Tsig_modtype(id, decl) -> add_modtype id decl env

let add_signature = List.fold_right add_signature_component

(* Open a signature path *)

let open_signature root sg env =
  (* First build the paths and substitution *)
  let (pl, sub) = prefix_idents root 0 Subst.identity sg in
  (* Then enter the components in the environment after substitution *)
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
                        (Subst.modtype_declaration sub decl) env)
    env sg pl

(* Open a signature from a file *)

let open_pers_signature name env =
  let ps = find_pers_struct name in
  open_signature (Pident(Ident.new_persistent name)) ps.ps_sig env

(* Read a signature from a file *)

let read_signature modname filename =
  let ps = read_pers_struct modname filename in (ps.ps_sig, ps.ps_crc)

(* Save a signature to a file *)

let save_signature sg modname crc filename =
  let ps =
    { ps_name = modname;
      ps_crc = crc;
      ps_sig = sg;
      ps_comps =
        components_of_module empty (Pident(Ident.new_persistent modname))
                                   (Tmty_signature sg) } in
  let oc = open_out_bin filename in
  output_string oc cmi_magic_number;
  output_value oc ps;
  close_out oc

(* Make the initial environment *)

let initial = Predef.build_initial_env add_type add_exception empty

(* Return the list of imported interfaces with their CRCs *)

let imported_units() =
  let l = ref [] in
  Hashtbl.iter
    (fun name ps -> l := (ps.ps_name, ps.ps_crc) :: !l) persistent_structures;
  !l

(* Error report *)

let report_error = function
    Not_an_interface filename ->
      print_string filename; print_space();
      print_string "is not a compiled interface."
  | Corrupted_interface filename ->
      print_string "Corrupted compiled interface"; print_space();
      print_string filename
  | Illegal_renaming(modname, filename) ->
      print_string filename; print_space();
      print_string "contains the compiled interface for"; print_space();
      print_string modname

