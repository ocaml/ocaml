(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Environment handling *)

open Cmi_format
open Misc
open Asttypes
open Longident
open Path
open Types
open Btype

open Local_store

module String = Misc.Stdlib.String

let add_delayed_check_forward = ref (fun _ -> assert false)

type 'a usage_tbl = ('a -> unit) Types.Uid.Tbl.t
(** This table is used to track usage of value declarations.
    A declaration is identified by its uid.
    The callback attached to a declaration is called whenever the value (or
    type, or ...) is used explicitly (lookup_value, ...) or implicitly
    (inclusion test between signatures, cf Includemod.value_descriptions, ...).
*)

let value_declarations  : unit usage_tbl ref = s_table Types.Uid.Tbl.create 16
let type_declarations   : unit usage_tbl ref = s_table Types.Uid.Tbl.create 16
let module_declarations : unit usage_tbl ref = s_table Types.Uid.Tbl.create 16

type constructor_usage = Positive | Pattern | Privatize
type constructor_usages =
    {
     mutable cu_positive: bool;
     mutable cu_pattern: bool;
     mutable cu_privatize: bool;
    }
let add_constructor_usage ~rebind priv cu usage =
  let private_or_rebind =
    match priv with
    | Asttypes.Private -> true
    | Asttypes.Public -> rebind
  in
  if private_or_rebind then begin
    cu.cu_positive <- true
  end else begin
    match usage with
    | Positive -> cu.cu_positive <- true
    | Pattern -> cu.cu_pattern <- true
    | Privatize -> cu.cu_privatize <- true
  end

let constructor_usages () =
  {cu_positive = false; cu_pattern = false; cu_privatize = false}

let used_constructors : constructor_usage usage_tbl ref =
  s_table Types.Uid.Tbl.create 16

(** Map indexed by the name of module components. *)
module NameMap = String.Map

type value_unbound_reason =
  | Val_unbound_instance_variable
  | Val_unbound_self
  | Val_unbound_ancestor
  | Val_unbound_ghost_recursive of Location.t

type module_unbound_reason =
  | Mod_unbound_illegal_recursion

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_presence * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration Path.Map.t
  | Env_copy_types of summary
  | Env_persistent of summary * Ident.t
  | Env_value_unbound of summary * string * value_unbound_reason
  | Env_module_unbound of summary * string * module_unbound_reason

type address =
  | Aident of Ident.t
  | Adot of address * int

module TycompTbl =
  struct
    (** This module is used to store components of types (i.e. labels
        and constructors).  We keep a representation of each nested
        "open" and the set of local bindings between each of them. *)

    type 'a t = {
      current: 'a Ident.tbl;
      (** Local bindings since the last open. *)

      opened: 'a opened option;
      (** Symbolic representation of the last (innermost) open, if any. *)
    }

    and 'a opened = {
      components: ('a list) NameMap.t;
      (** Components from the opened module. We keep a list of
          bindings for each name, as in comp_labels and
          comp_constrs. *)

      using: (string -> ('a * 'a) option -> unit) option;
      (** A callback to be applied when a component is used from this
          "open".  This is used to detect unused "opens".  The
          arguments are used to detect shadowing. *)

      next: 'a t;
      (** The table before opening the module. *)
    }

    let empty = { current = Ident.empty; opened = None }

    let add id x tbl =
      {tbl with current = Ident.add id x tbl.current}

    let add_open slot wrap components next =
      let using =
        match slot with
        | None -> None
        | Some f -> Some (fun s x -> f s (wrap x))
      in
      {
        current = Ident.empty;
        opened = Some {using; components; next};
      }

    let rec find_same id tbl =
      try Ident.find_same id tbl.current
      with Not_found as exn ->
        begin match tbl.opened with
        | Some {next; _} -> find_same id next
        | None -> raise exn
        end

    let nothing = fun () -> ()

    let mk_callback rest name desc using =
      match using with
      | None -> nothing
      | Some f ->
          (fun () ->
             match rest with
             | [] -> f name None
             | (hidden, _) :: _ -> f name (Some (desc, hidden)))

    let rec find_all ~mark name tbl =
      List.map (fun (_id, desc) -> desc, nothing)
        (Ident.find_all name tbl.current) @
      match tbl.opened with
      | None -> []
      | Some {using; next; components} ->
          let rest = find_all ~mark name next in
          let using = if mark then using else None in
          match NameMap.find name components with
          | exception Not_found -> rest
          | opened ->
              List.map
                (fun desc -> desc, mk_callback rest name desc using)
                opened
              @ rest

    let rec fold_name f tbl acc =
      let acc = Ident.fold_name (fun _id d -> f d) tbl.current acc in
      match tbl.opened with
      | Some {using = _; next; components} ->
          acc
          |> NameMap.fold
            (fun _name -> List.fold_right f)
            components
          |> fold_name f next
      | None ->
          acc

    let rec local_keys tbl acc =
      let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
      match tbl.opened with
      | Some o -> local_keys o.next acc
      | None -> acc

    let diff_keys is_local tbl1 tbl2 =
      let keys2 = local_keys tbl2 [] in
      List.filter
        (fun id ->
           is_local (find_same id tbl2) &&
           try ignore (find_same id tbl1); false
           with Not_found -> true)
        keys2

  end


module IdTbl =
  struct
    (** This module is used to store all kinds of components except
        (labels and constructors) in environments.  We keep a
        representation of each nested "open" and the set of local
        bindings between each of them. *)


    type ('a, 'b) t = {
      current: 'a Ident.tbl;
      (** Local bindings since the last open *)

      layer: ('a, 'b) layer;
      (** Symbolic representation of the last (innermost) open, if any. *)
    }

    and ('a, 'b) layer =
      | Open of {
          root: Path.t;
          (** The path of the opened module, to be prefixed in front of
              its local names to produce a valid path in the current
              environment. *)

          components: 'b NameMap.t;
          (** Components from the opened module. *)

          using: (string -> ('a * 'a) option -> unit) option;
          (** A callback to be applied when a component is used from this
              "open".  This is used to detect unused "opens".  The
              arguments are used to detect shadowing. *)

          next: ('a, 'b) t;
          (** The table before opening the module. *)
        }

      | Map of {
          f: ('a -> 'a);
          next: ('a, 'b) t;
        }

      | Nothing

    let empty = { current = Ident.empty; layer = Nothing }

    let add id x tbl =
      {tbl with current = Ident.add id x tbl.current}

    let remove id tbl =
      {tbl with current = Ident.remove id tbl.current}

    let add_open slot wrap root components next =
      let using =
        match slot with
        | None -> None
        | Some f -> Some (fun s x -> f s (wrap x))
      in
      {
        current = Ident.empty;
        layer = Open {using; root; components; next};
      }

    let map f next =
      {
        current = Ident.empty;
        layer = Map {f; next}
      }

    let rec find_same id tbl =
      try Ident.find_same id tbl.current
      with Not_found as exn ->
        begin match tbl.layer with
        | Open {next; _} -> find_same id next
        | Map {f; next} -> f (find_same id next)
        | Nothing -> raise exn
        end

    let rec find_name wrap ~mark name tbl =
      try
        let (id, desc) = Ident.find_name name tbl.current in
        Pident id, desc
      with Not_found as exn ->
        begin match tbl.layer with
        | Open {using; root; next; components} ->
            begin try
              let descr = wrap (NameMap.find name components) in
              let res = Pdot (root, name), descr in
              if mark then begin match using with
              | None -> ()
              | Some f -> begin
                  match find_name wrap ~mark:false name next with
                  | exception Not_found -> f name None
                  | _, descr' -> f name (Some (descr', descr))
                end
              end;
              res
            with Not_found ->
              find_name wrap ~mark name next
            end
        | Map {f; next} ->
            let (p, desc) =  find_name wrap ~mark name next in
            p, f desc
        | Nothing ->
            raise exn
        end

    let rec find_all wrap name tbl =
      List.map
        (fun (id, desc) -> Pident id, desc)
        (Ident.find_all name tbl.current) @
      match tbl.layer with
      | Nothing -> []
      | Open {root; using = _; next; components} ->
          begin try
            let desc = wrap (NameMap.find name components) in
            (Pdot (root, name), desc) :: find_all wrap name next
          with Not_found ->
            find_all wrap name next
          end
      | Map {f; next} ->
          List.map (fun (p, desc) -> (p, f desc))
            (find_all wrap name next)

    let rec fold_name wrap f tbl acc =
      let acc =
        Ident.fold_name
          (fun id d -> f (Ident.name id) (Pident id, d))
          tbl.current acc
      in
      match tbl.layer with
      | Open {root; using = _; next; components} ->
          acc
          |> NameMap.fold
            (fun name desc -> f name (Pdot (root, name), wrap desc))
            components
          |> fold_name wrap f next
      | Nothing ->
          acc
      | Map {f=g; next} ->
          acc
          |> fold_name wrap
               (fun name (path, desc) -> f name (path, g desc))
               next

    let rec local_keys tbl acc =
      let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
      match tbl.layer with
      | Open {next; _ } | Map {next; _} -> local_keys next acc
      | Nothing -> acc


    let rec iter wrap f tbl =
      Ident.iter (fun id desc -> f id (Pident id, desc)) tbl.current;
      match tbl.layer with
      | Open {root; using = _; next; components} ->
          NameMap.iter
            (fun s x ->
               let root_scope = Path.scope root in
              f (Ident.create_scoped ~scope:root_scope s)
                (Pdot (root, s), wrap x))
            components;
          iter wrap f next
      | Map {f=g; next} ->
          iter wrap (fun id (path, desc) -> f id (path, g desc)) next
      | Nothing -> ()

    let diff_keys tbl1 tbl2 =
      let keys2 = local_keys tbl2 [] in
      List.filter
        (fun id ->
           try ignore (find_same id tbl1); false
           with Not_found -> true)
        keys2


  end

type type_descriptions =
    constructor_description list * label_description list

let in_signature_flag = 0x01

type t = {
  values: (value_entry, value_data) IdTbl.t;
  constrs: constructor_data TycompTbl.t;
  labels: label_data TycompTbl.t;
  types: (type_data, type_data) IdTbl.t;
  modules: (module_entry, module_data) IdTbl.t;
  modtypes: (modtype_data, modtype_data) IdTbl.t;
  classes: (class_data, class_data) IdTbl.t;
  cltypes: (cltype_data, cltype_data) IdTbl.t;
  functor_args: unit Ident.tbl;
  summary: summary;
  local_constraints: type_declaration Path.Map.t;
  flags: int;
}

and module_declaration_lazy =
  (Subst.t * Subst.scoping * module_declaration, module_declaration) EnvLazy.t

and module_components =
  {
    alerts: alerts;
    uid: Uid.t;
    comps:
      (components_maker,
       (module_components_repr, module_components_failure) result)
        EnvLazy.t;
  }

and components_maker = {
  cm_env: t;
  cm_freshening_subst: Subst.t option;
  cm_prefixing_subst: Subst.t;
  cm_path: Path.t;
  cm_addr: address_lazy;
  cm_mty: Types.module_type;
}

and module_components_repr =
    Structure_comps of structure_components
  | Functor_comps of functor_components

and module_components_failure =
  | No_components_abstract
  | No_components_alias of Path.t

and structure_components = {
  mutable comp_values: value_data NameMap.t;
  mutable comp_constrs: constructor_data list NameMap.t;
  mutable comp_labels: label_data list NameMap.t;
  mutable comp_types: type_data NameMap.t;
  mutable comp_modules: module_data NameMap.t;
  mutable comp_modtypes: modtype_data NameMap.t;
  mutable comp_classes: class_data NameMap.t;
  mutable comp_cltypes: cltype_data NameMap.t;
}

and functor_components = {
  fcomp_arg: functor_parameter;
  (* Formal parameter and argument signature *)
  fcomp_res: module_type;               (* Result signature *)
  fcomp_cache: (Path.t, module_components) Hashtbl.t;  (* For memoization *)
  fcomp_subst_cache: (Path.t, module_type) Hashtbl.t
}

and address_unforced =
  | Projection of { parent : address_lazy; pos : int; }
  | ModAlias of { env : t; path : Path.t; }

and address_lazy = (address_unforced, address) EnvLazy.t

and value_data =
  { vda_description : value_description;
    vda_address : address_lazy }

and value_entry =
  | Val_bound of value_data
  | Val_unbound of value_unbound_reason

and constructor_data =
  { cda_description : constructor_description;
    cda_address : address_lazy option; }

and label_data = label_description

and type_data =
  { tda_declaration : type_declaration;
    tda_descriptions : type_descriptions; }

and module_data =
  { mda_declaration : module_declaration_lazy;
    mda_components : module_components;
    mda_address : address_lazy; }

and module_entry =
  | Mod_local of module_data
  | Mod_persistent
  | Mod_unbound of module_unbound_reason

and modtype_data = modtype_declaration

and class_data =
  { clda_declaration : class_declaration;
    clda_address : address_lazy }

and cltype_data = class_type_declaration

let empty_structure =
  Structure_comps {
    comp_values = NameMap.empty;
    comp_constrs = NameMap.empty;
    comp_labels = NameMap.empty;
    comp_types = NameMap.empty;
    comp_modules = NameMap.empty; comp_modtypes = NameMap.empty;
    comp_classes = NameMap.empty;
    comp_cltypes = NameMap.empty }

type unbound_value_hint =
  | No_hint
  | Missing_rec of Location.t

type lookup_error =
  | Unbound_value of Longident.t * unbound_value_hint
  | Unbound_type of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of Longident.t
  | Unbound_class of Longident.t
  | Unbound_modtype of Longident.t
  | Unbound_cltype of Longident.t
  | Unbound_instance_variable of string
  | Not_an_instance_variable of string
  | Masked_instance_variable of Longident.t
  | Masked_self_variable of Longident.t
  | Masked_ancestor_variable of Longident.t
  | Structure_used_as_functor of Longident.t
  | Abstract_used_as_functor of Longident.t
  | Functor_used_as_structure of Longident.t
  | Abstract_used_as_structure of Longident.t
  | Generative_used_as_applicative of Longident.t
  | Illegal_reference_to_recursive_module
  | Cannot_scrape_alias of Longident.t * Path.t

type error =
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string
  | Lookup_error of Location.t * t * lookup_error

exception Error of error

let error err = raise (Error err)

let lookup_error loc env err =
  error (Lookup_error(loc, env, err))

let copy_local ~from env =
  { env with
    local_constraints = from.local_constraints;
    flags = from.flags }

let same_constr = ref (fun _ _ _ -> assert false)

let check_well_formed_module = ref (fun _ -> assert false)

(* Helper to decide whether to report an identifier shadowing
   by some 'open'. For labels and constructors, we do not report
   if the two elements are from the same re-exported declaration.

   Later, one could also interpret some attributes on value and
   type declarations to silence the shadowing warnings. *)

let check_shadowing env = function
  | `Constructor (Some (cda1, cda2))
    when not (!same_constr env
                cda1.cda_description.cstr_res
                cda2.cda_description.cstr_res) ->
      Some "constructor"
  | `Label (Some (l1, l2))
    when not (!same_constr env l1.lbl_res l2.lbl_res) ->
      Some "label"
  | `Value (Some _) -> Some "value"
  | `Type (Some _) -> Some "type"
  | `Module (Some _) | `Component (Some _) -> Some "module"
  | `Module_type (Some _) -> Some "module type"
  | `Class (Some _) -> Some "class"
  | `Class_type (Some _) -> Some "class type"
  | `Constructor _ | `Label _
  | `Value None | `Type None | `Module None | `Module_type None
  | `Class None | `Class_type None | `Component None ->
      None

let subst_modtype_maker (subst, scoping, md) =
  {md with md_type = Subst.modtype scoping subst md.md_type}

let empty = {
  values = IdTbl.empty; constrs = TycompTbl.empty;
  labels = TycompTbl.empty; types = IdTbl.empty;
  modules = IdTbl.empty; modtypes = IdTbl.empty;
  classes = IdTbl.empty; cltypes = IdTbl.empty;
  summary = Env_empty; local_constraints = Path.Map.empty;
  flags = 0;
  functor_args = Ident.empty;
 }

let in_signature b env =
  let flags =
    if b then env.flags lor in_signature_flag
    else env.flags land (lnot in_signature_flag)
  in
  {env with flags}

let is_in_signature env = env.flags land in_signature_flag <> 0

let has_local_constraints env =
  not (Path.Map.is_empty env.local_constraints)

let is_ident = function
    Pident _ -> true
  | Pdot _ | Papply _ -> false

let is_ext cda =
  match cda.cda_description with
  | {cstr_tag = Cstr_extension _} -> true
  | _ -> false

let is_local_ext cda =
  match cda.cda_description with
  | {cstr_tag = Cstr_extension(p, _)} -> is_ident p
  | _ -> false

let diff env1 env2 =
  IdTbl.diff_keys env1.values env2.values @
  TycompTbl.diff_keys is_local_ext env1.constrs env2.constrs @
  IdTbl.diff_keys env1.modules env2.modules @
  IdTbl.diff_keys env1.classes env2.classes

(* Functions for use in "wrap" parameters in IdTbl *)
let wrap_identity x = x
let wrap_value vda = Val_bound vda
let wrap_module mda = Mod_local mda

(* Forward declarations *)

let components_of_module_maker' =
  ref ((fun _ -> assert false) :
          components_maker ->
            (module_components_repr, module_components_failure) result)

let components_of_functor_appl' =
  ref ((fun ~loc:_ _f _env _p1 _p2 -> assert false) :
          loc:Location.t -> functor_components -> t ->
            Path.t -> Path.t -> module_components)
let check_functor_application =
  (* to be filled by Includemod *)
  ref ((fun ~errors:_ ~loc:_ _env _mty1 _path1 _mty2 _path2 -> assert false) :
          errors:bool -> loc:Location.t -> t -> module_type ->
            Path.t -> module_type -> Path.t -> unit)
let strengthen =
  (* to be filled with Mtype.strengthen *)
  ref ((fun ~aliasable:_ _env _mty _path -> assert false) :
         aliasable:bool -> t -> module_type -> Path.t -> module_type)

let md md_type =
  {md_type; md_attributes=[]; md_loc=Location.none
  ;md_uid = Uid.internal_not_actually_unique}

(* Print addresses *)

let rec print_address ppf = function
  | Aident id -> Format.fprintf ppf "%s" (Ident.name id)
  | Adot(a, pos) -> Format.fprintf ppf "%a.[%i]" print_address a pos

(* The name of the compilation unit currently compiled.
   "" if outside a compilation unit. *)
module Current_unit_name : sig
  val get : unit -> modname
  val set : modname -> unit
  val is : modname -> bool
  val is_ident : Ident.t -> bool
  val is_path : Path.t -> bool
end = struct
  let current_unit =
    ref ""
  let get () =
    !current_unit
  let set name =
    current_unit := name
  let is name =
    !current_unit = name
  let is_ident id =
    Ident.persistent id && is (Ident.name id)
  let is_path = function
  | Pident id -> is_ident id
  | Pdot _ | Papply _ -> false
end

let set_unit_name = Current_unit_name.set
let get_unit_name = Current_unit_name.get

let find_same_module id tbl =
  match IdTbl.find_same id tbl with
  | x -> x
  | exception Not_found
    when Ident.persistent id && not (Current_unit_name.is_ident id) ->
      Mod_persistent

let find_name_module ~mark name tbl =
  match IdTbl.find_name wrap_module ~mark name tbl with
  | x -> x
  | exception Not_found when not (Current_unit_name.is name) ->
      let path = Pident(Ident.create_persistent name) in
      path, Mod_persistent

let add_persistent_structure id env =
  if not (Ident.persistent id) then invalid_arg "Env.add_persistent_structure";
  if Current_unit_name.is_ident id then env
  else begin
    let material =
      (* This addition only observably changes the environment if it shadows a
         non-persistent module already in the environment.
         (See PR#9345) *)
      match
        IdTbl.find_name wrap_module ~mark:false (Ident.name id) env.modules
      with
      | exception Not_found | _, Mod_persistent -> false
      | _ -> true
    in
    let summary =
      if material then Env_persistent (env.summary, id)
      else env.summary
    in
    let modules =
      (* With [-no-alias-deps], non-material additions should not
         affect the environment at all. We should only observe the
         existence of a cmi when accessing components of the module.
         (See #9991). *)
      if material || not !Clflags.transparent_modules then
        IdTbl.add id Mod_persistent env.modules
      else
        env.modules
    in
    { env with modules; summary }
  end

let components_of_module ~alerts ~uid env fs ps path addr mty =
  {
    alerts;
    uid;
    comps = EnvLazy.create {
      cm_env = env;
      cm_freshening_subst = fs;
      cm_prefixing_subst = ps;
      cm_path = path;
      cm_addr = addr;
      cm_mty = mty
    }
  }

let sign_of_cmi ~freshen { Persistent_env.Persistent_signature.cmi; _ } =
  let name = cmi.cmi_name in
  let sign = cmi.cmi_sign in
  let flags = cmi.cmi_flags in
  let id = Ident.create_persistent name in
  let path = Pident id in
  let alerts =
    List.fold_left (fun acc -> function Alerts s -> s | _ -> acc)
      Misc.Stdlib.String.Map.empty
      flags
  in
  let md =
    { md_type =  Mty_signature sign;
      md_loc = Location.none;
      md_attributes = [];
      md_uid = Uid.of_compilation_unit_id id;
    }
  in
  let mda_address = EnvLazy.create_forced (Aident id) in
  let mda_declaration =
    EnvLazy.create (Subst.identity, Subst.Make_local, md)
  in
  let mda_components =
    let freshening_subst =
      if freshen then (Some Subst.identity) else None
    in
    components_of_module ~alerts ~uid:md.md_uid
      empty freshening_subst Subst.identity
      path mda_address (Mty_signature sign)
  in
  {
    mda_declaration;
    mda_components;
    mda_address;
  }

let read_sign_of_cmi = sign_of_cmi ~freshen:true

let save_sign_of_cmi = sign_of_cmi ~freshen:false

let persistent_env : module_data Persistent_env.t ref =
  s_table Persistent_env.empty ()

let without_cmis f x =
  Persistent_env.without_cmis !persistent_env f x

let imports () = Persistent_env.imports !persistent_env

let import_crcs ~source crcs =
  Persistent_env.import_crcs !persistent_env ~source crcs

let read_pers_mod modname filename =
  Persistent_env.read !persistent_env read_sign_of_cmi modname filename

let find_pers_mod name =
  Persistent_env.find !persistent_env read_sign_of_cmi name

let check_pers_mod ~loc name =
  Persistent_env.check !persistent_env read_sign_of_cmi ~loc name

let crc_of_unit name =
  Persistent_env.crc_of_unit !persistent_env read_sign_of_cmi name

let is_imported_opaque modname =
  Persistent_env.is_imported_opaque !persistent_env modname

let register_import_as_opaque modname =
  Persistent_env.register_import_as_opaque !persistent_env modname

let reset_declaration_caches () =
  Types.Uid.Tbl.clear !value_declarations;
  Types.Uid.Tbl.clear !type_declarations;
  Types.Uid.Tbl.clear !module_declarations;
  Types.Uid.Tbl.clear !used_constructors;
  ()

let reset_cache () =
  Current_unit_name.set "";
  Persistent_env.clear !persistent_env;
  reset_declaration_caches ();
  ()

let reset_cache_toplevel () =
  Persistent_env.clear_missing !persistent_env;
  reset_declaration_caches ();
  ()

(* get_components *)

let get_components_res c =
  match Persistent_env.can_load_cmis !persistent_env with
  | Persistent_env.Can_load_cmis ->
    EnvLazy.force !components_of_module_maker' c.comps
  | Persistent_env.Cannot_load_cmis log ->
    EnvLazy.force_logged log !components_of_module_maker' c.comps

let get_components c =
  match get_components_res c with
  | Error _ -> empty_structure
  | Ok c -> c

(* Module type of functor application *)

let modtype_of_functor_appl fcomp p1 p2 =
  match fcomp.fcomp_res with
  | Mty_alias _ as mty -> mty
  | mty ->
      try
        Hashtbl.find fcomp.fcomp_subst_cache p2
      with Not_found ->
        let scope = Path.scope (Papply(p1, p2)) in
        let mty =
          let subst =
            match fcomp.fcomp_arg with
            | Unit
            | Named (None, _) -> Subst.identity
            | Named (Some param, _) -> Subst.add_module param p2 Subst.identity
          in
          Subst.modtype (Rescope scope) subst mty
        in
        Hashtbl.add fcomp.fcomp_subst_cache p2 mty;
        mty

let check_functor_appl ~errors ~loc env p1 f arg p2 md =
  if not (Hashtbl.mem f.fcomp_cache p2) then
    !check_functor_application ~errors ~loc env md.md_type p2 arg p1

(* Lookup by identifier *)

let find_ident_module id env =
  match find_same_module id env.modules with
  | Mod_local data -> data
  | Mod_unbound _ -> raise Not_found
  | Mod_persistent -> find_pers_mod (Ident.name id)

let rec find_module_components path env =
  match path with
  | Pident id -> (find_ident_module id env).mda_components
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      (NameMap.find s sc.comp_modules).mda_components
  | Papply(p1, p2) ->
      let fc = find_functor_components p1 env in
      let loc = Location.(in_file !input_name) in
      !components_of_functor_appl' ~loc fc env p1 p2

and find_structure_components path env =
  match get_components (find_module_components path env) with
  | Structure_comps c -> c
  | Functor_comps _ -> raise Not_found

and find_functor_components path env =
  match get_components (find_module_components path env) with
  | Functor_comps f -> f
  | Structure_comps _ -> raise Not_found

let find_module ~alias path env =
  match path with
  | Pident id ->
      let data = find_ident_module id env in
      EnvLazy.force subst_modtype_maker data.mda_declaration
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      let data = NameMap.find s sc.comp_modules in
      EnvLazy.force subst_modtype_maker data.mda_declaration
  | Papply(p1, p2) ->
      let fc = find_functor_components p1 env in
      if alias then md (fc.fcomp_res)
      else md (modtype_of_functor_appl fc p1 p2)

let find_value_full path env =
  match path with
  | Pident id -> begin
      match IdTbl.find_same id env.values with
      | Val_bound data -> data
      | Val_unbound _ -> raise Not_found
    end
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_values
  | Papply _ -> raise Not_found

let find_type_full path env =
  match path with
  | Pident id -> IdTbl.find_same id env.types
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_types
  | Papply _ -> raise Not_found

let find_modtype path env =
  match path with
  | Pident id -> IdTbl.find_same id env.modtypes
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_modtypes
  | Papply _ -> raise Not_found

let find_class_full path env =
  match path with
  | Pident id -> IdTbl.find_same id env.classes
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_classes
  | Papply _ -> raise Not_found

let find_cltype path env =
  match path with
  | Pident id -> IdTbl.find_same id env.cltypes
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_cltypes
  | Papply _ -> raise Not_found

let find_value path env =
  (find_value_full path env).vda_description

let find_class path env =
  (find_class_full path env).clda_declaration

let find_ident_constructor id env =
  (TycompTbl.find_same id env.constrs).cda_description

let find_ident_label id env =
  TycompTbl.find_same id env.labels

let type_of_cstr path = function
  | {cstr_inlined = Some decl; _} ->
      let labels =
        List.map snd (Datarepr.labels_of_type path decl)
      in
      { tda_declaration = decl; tda_descriptions = ([], labels) }
  | _ ->
      assert false

let find_type_full path env =
  match Path.constructor_typath path with
  | Regular p -> begin
      match Path.Map.find p env.local_constraints with
      | decl ->
          { tda_declaration = decl; tda_descriptions = [], [] }
      | exception Not_found -> find_type_full p env
    end
  | Cstr (ty_path, s) ->
      let tda =
        try find_type_full ty_path env
        with Not_found -> assert false
      in
      let (cstrs, _) = tda.tda_descriptions in
      let cstr =
        try List.find (fun cstr -> cstr.cstr_name = s) cstrs
        with Not_found -> assert false
      in
      type_of_cstr path cstr
  | LocalExt id ->
      let cstr =
        try (TycompTbl.find_same id env.constrs).cda_description
        with Not_found -> assert false
      in
      type_of_cstr path cstr
  | Ext (mod_path, s) ->
      let comps =
        try find_structure_components mod_path env
        with Not_found -> assert false
      in
      let cstrs =
        try NameMap.find s comps.comp_constrs
        with Not_found -> assert false
      in
      let exts = List.filter is_ext cstrs in
      match exts with
      | [cda] -> type_of_cstr path cda.cda_description
      | _ -> assert false

let find_type p env =
  (find_type_full p env).tda_declaration
let find_type_descrs p env =
  (find_type_full p env).tda_descriptions

let rec find_module_address path env =
  match path with
  | Pident id -> get_address (find_ident_module id env).mda_address
  | Pdot(p, s) ->
      let c = find_structure_components p env in
      get_address (NameMap.find s c.comp_modules).mda_address
  | Papply _ -> raise Not_found

and force_address = function
  | Projection { parent; pos } -> Adot(get_address parent, pos)
  | ModAlias { env; path } -> find_module_address path env

and get_address a =
  EnvLazy.force force_address a

let find_value_address path env =
  get_address (find_value_full path env).vda_address

let find_class_address path env =
  get_address (find_class_full path env).clda_address

let rec get_constrs_address = function
  | [] -> raise Not_found
  | cda :: rest ->
    match cda.cda_address with
    | None -> get_constrs_address rest
    | Some a -> get_address a

let find_constructor_address path env =
  match path with
  | Pident id -> begin
      let cda = TycompTbl.find_same id env.constrs in
      match cda.cda_address with
      | None -> raise Not_found
      | Some addr -> get_address addr
    end
  | Pdot(p, s) ->
      let c = find_structure_components p env in
      get_constrs_address (NameMap.find s c.comp_constrs)
  | Papply _ ->
      raise Not_found

let find_hash_type path env =
  match path with
  | Pident id ->
      let name = "#" ^ Ident.name id in
      let _, tda =
        IdTbl.find_name wrap_identity ~mark:false name env.types
      in
      tda.tda_declaration
  | Pdot(p, s) ->
      let c = find_structure_components p env in
      let name = "#" ^ s in
      let tda = NameMap.find name c.comp_types in
      tda.tda_declaration
  | Papply _ ->
      raise Not_found

let required_globals = s_ref []
let reset_required_globals () = required_globals := []
let get_required_globals () = !required_globals
let add_required_global id =
  if Ident.global id && not !Clflags.transparent_modules
  && not (List.exists (Ident.same id) !required_globals)
  then required_globals := id :: !required_globals

let rec normalize_module_path lax env = function
  | Pident id as path when lax && Ident.persistent id ->
      path (* fast path (avoids lookup) *)
  | Pdot (p, s) as path ->
      let p' = normalize_module_path lax env p in
      if p == p' then expand_module_path lax env path
      else expand_module_path lax env (Pdot(p', s))
  | Papply (p1, p2) as path ->
      let p1' = normalize_module_path lax env p1 in
      let p2' = normalize_module_path true env p2 in
      if p1 == p1' && p2 == p2' then expand_module_path lax env path
      else expand_module_path lax env (Papply(p1', p2'))
  | Pident _ as path ->
      expand_module_path lax env path

and expand_module_path lax env path =
  try match find_module ~alias:true path env with
    {md_type=Mty_alias path1} ->
      let path' = normalize_module_path lax env path1 in
      if lax || !Clflags.transparent_modules then path' else
      let id = Path.head path in
      if Ident.global id && not (Ident.same id (Path.head path'))
      then add_required_global id;
      path'
  | _ -> path
  with Not_found when lax
  || (match path with Pident id -> not (Ident.persistent id) | _ -> true) ->
      path

let normalize_module_path oloc env path =
  try normalize_module_path (oloc = None) env path
  with Not_found ->
    match oloc with None -> assert false
    | Some loc ->
        error (Missing_module(loc, path,
                              normalize_module_path true env path))

let normalize_path_prefix oloc env path =
  match path with
    Pdot(p, s) ->
      let p2 = normalize_module_path oloc env p in
      if p == p2 then path else Pdot(p2, s)
  | Pident _ ->
      path
  | Papply _ ->
      assert false

let normalize_type_path oloc env path =
  (* Inlined version of Path.is_constructor_typath:
     constructor type paths (i.e. path pointing to an inline
     record argument of a constructpr) are built as a regular
     type path followed by a capitalized constructor name. *)
  match path with
  | Pident _ ->
      path
  | Pdot(p, s) ->
      let p2 =
        if Path.is_uident s && not (Path.is_uident (Path.last p)) then
          (* Cstr M.t.C *)
          normalize_path_prefix oloc env p
        else
          (* Regular M.t, Ext M.C *)
          normalize_module_path oloc env p
      in
      if p == p2 then path else Pdot (p2, s)
  | Papply _ ->
      assert false

let rec normalize_modtype_path env path =
  let path = normalize_path_prefix None env path in
  expand_modtype_path env path

and expand_modtype_path env path =
  match (find_modtype path env).mtd_type with
  | Some (Mty_ident path) -> normalize_modtype_path env path
  | _ | exception Not_found -> path

let find_module path env =
  find_module ~alias:false path env

(* Find the manifest type associated to a type when appropriate:
   - the type should be public or should have a private row,
   - the type should have an associated manifest type. *)
let find_type_expansion path env =
  let decl = find_type path env in
  match decl.type_manifest with
  | Some body when decl.type_private = Public
              || decl.type_kind <> Type_abstract
              || Btype.has_constr_row body ->
      (decl.type_params, body, decl.type_expansion_scope)
  (* The manifest type of Private abstract data types without
     private row are still considered unknown to the type system.
     Hence, this case is caught by the following clause that also handles
     purely abstract data types without manifest type definition. *)
  | _ -> raise Not_found

(* Find the manifest type information associated to a type, i.e.
   the necessary information for the compiler's type-based optimisations.
   In particular, the manifest type associated to a private abstract type
   is revealed for the sake of compiler's type-based optimisations. *)
let find_type_expansion_opt path env =
  let decl = find_type path env in
  match decl.type_manifest with
  (* The manifest type of Private abstract data types can still get
     an approximation using their manifest type. *)
  | Some body ->
      (decl.type_params, body, decl.type_expansion_scope)
  | _ -> raise Not_found

let find_modtype_expansion path env =
  match (find_modtype path env).mtd_type with
  | None -> raise Not_found
  | Some mty -> mty

let rec is_functor_arg path env =
  match path with
    Pident id ->
      begin try Ident.find_same id env.functor_args; true
      with Not_found -> false
      end
  | Pdot (p, _s) -> is_functor_arg p env
  | Papply _ -> true

(* Copying types associated with values *)

let make_copy_of_types env0 =
  let memo = Hashtbl.create 16 in
  let copy t =
    try
      Hashtbl.find memo t.id
    with Not_found ->
      let t2 = Subst.type_expr Subst.identity t in
      Hashtbl.add memo t.id t2;
      t2
  in
  let f = function
    | Val_unbound _ as entry -> entry
    | Val_bound vda ->
        let desc = vda.vda_description in
        let desc = { desc with val_type = copy desc.val_type } in
        Val_bound { vda with vda_description = desc }
  in
  let values =
    IdTbl.map f env0.values
  in
  (fun env ->
     if env.values != env0.values then fatal_error "Env.make_copy_of_types";
     {env with values; summary = Env_copy_types env.summary}
  )

(* Helper to handle optional substitutions. *)

let may_subst subst_f sub x =
  match sub with
  | None -> x
  | Some sub -> subst_f sub x

(* Iter on an environment (ignoring the body of functors and
   not yet evaluated structures) *)

type iter_cont = unit -> unit
let iter_env_cont = ref []

let rec scrape_alias_for_visit env (sub : Subst.t option) mty =
  match mty with
  | Mty_alias path ->
      begin match may_subst Subst.module_path sub path with
      | Pident id
        when Ident.persistent id
          && not (Persistent_env.looked_up !persistent_env (Ident.name id)) ->
          false
      | path -> (* PR#6600: find_module may raise Not_found *)
          try scrape_alias_for_visit env sub (find_module path env).md_type
          with Not_found -> false
      end
  | _ -> true

let iter_env wrap proj1 proj2 f env () =
  IdTbl.iter wrap (fun id x -> f (Pident id) x) (proj1 env);
  let rec iter_components path path' mcomps =
    let cont () =
      let visit =
        match EnvLazy.get_arg mcomps.comps with
        | None -> true
        | Some { cm_mty; cm_freshening_subst; _ } ->
            scrape_alias_for_visit env cm_freshening_subst cm_mty
      in
      if not visit then () else
      match get_components mcomps with
        Structure_comps comps ->
          NameMap.iter
            (fun s d -> f (Pdot (path, s)) (Pdot (path', s), d))
            (proj2 comps);
          NameMap.iter
            (fun s mda ->
              iter_components
                (Pdot (path, s)) (Pdot (path', s)) mda.mda_components)
            comps.comp_modules
      | Functor_comps _ -> ()
    in iter_env_cont := (path, cont) :: !iter_env_cont
  in
  IdTbl.iter wrap_module
    (fun id (path, entry) ->
       match entry with
       | Mod_unbound _ -> ()
       | Mod_local data ->
           iter_components (Pident id) path data.mda_components
       | Mod_persistent ->
           let modname = Ident.name id in
           match Persistent_env.find_in_cache !persistent_env modname with
           | None -> ()
           | Some data ->
               iter_components (Pident id) path data.mda_components)
    env.modules

let run_iter_cont l =
  iter_env_cont := [];
  List.iter (fun c -> c ()) l;
  let cont = List.rev !iter_env_cont in
  iter_env_cont := [];
  cont

let iter_types f =
  iter_env wrap_identity (fun env -> env.types) (fun sc -> sc.comp_types)
    (fun p1 (p2, tda) -> f p1 (p2, tda.tda_declaration))

let same_types env1 env2 =
  env1.types == env2.types && env1.modules == env2.modules

let used_persistent () =
  Persistent_env.fold !persistent_env
    (fun s _m r -> Concr.add s r)
    Concr.empty

let find_all_comps wrap proj s (p, mda) =
  match get_components mda.mda_components with
    Functor_comps _ -> []
  | Structure_comps comps ->
      try
        let c = NameMap.find s (proj comps) in
        [Pdot(p,s), wrap c]
      with Not_found -> []

let rec find_shadowed_comps path env =
  match path with
  | Pident id ->
      List.filter_map
        (fun (p, data) ->
           match data with
           | Mod_local x -> Some (p, x)
           | Mod_unbound _ | Mod_persistent -> None)
        (IdTbl.find_all wrap_module (Ident.name id) env.modules)
  | Pdot (p, s) ->
      let l = find_shadowed_comps p env in
      let l' =
        List.map
          (find_all_comps wrap_identity
             (fun comps -> comps.comp_modules) s) l
      in
      List.flatten l'
  | Papply _ -> []

let find_shadowed wrap proj1 proj2 path env =
  match path with
    Pident id ->
      IdTbl.find_all wrap (Ident.name id) (proj1 env)
  | Pdot (p, s) ->
      let l = find_shadowed_comps p env in
      let l' = List.map (find_all_comps wrap proj2 s) l in
      List.flatten l'
  | Papply _ -> []

let find_shadowed_types path env =
  List.map fst
    (find_shadowed wrap_identity
       (fun env -> env.types) (fun comps -> comps.comp_types) path env)

(* Expand manifest module type names at the top of the given module type *)

let rec scrape_alias env sub ?path mty =
  match mty, path with
    Mty_ident _, _ ->
      let p =
        match may_subst (Subst.modtype Keep) sub mty with
        | Mty_ident p -> p
        | _ -> assert false (* only [Mty_ident]s in [sub] *)
      in
      begin try
        scrape_alias env sub (find_modtype_expansion p env) ?path
      with Not_found ->
        mty
      end
  | Mty_alias path, _ ->
      let path = may_subst Subst.module_path sub path in
      begin try
        scrape_alias env sub (find_module path env).md_type ~path
      with Not_found ->
        (*Location.prerr_warning Location.none
          (Warnings.No_cmi_file (Path.name path));*)
        mty
      end
  | mty, Some path ->
      !strengthen ~aliasable:true env mty path
  | _ -> mty

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let prefix_idents root freshening_sub prefixing_sub sg =
  let refresh id add_fn = function
    | None -> id, None
    | Some sub ->
      let id' = Ident.rename id in
      id', Some (add_fn id (Pident id') sub)
  in
  let rec prefix_idents root items_and_paths freshening_sub prefixing_sub =
    function
    | [] -> (List.rev items_and_paths, freshening_sub, prefixing_sub)
    | Sig_value(id, _, _) as item :: rem ->
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((item, p) :: items_and_paths) freshening_sub prefixing_sub rem
    | Sig_type(id, td, rs, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      let id', freshening_sub = refresh id Subst.add_type freshening_sub in
      prefix_idents root
        ((Sig_type(id', td, rs, vis), p) :: items_and_paths)
        freshening_sub
        (Subst.add_type id' p prefixing_sub)
        rem
    | Sig_typext(id, ec, es, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      let id', freshening_sub = refresh id Subst.add_type freshening_sub in
      (* we extend the substitution in case of an inlined record *)
      prefix_idents root
        ((Sig_typext(id', ec, es, vis), p) :: items_and_paths)
        freshening_sub
        (Subst.add_type id' p prefixing_sub)
        rem
    | Sig_module(id, pres, md, rs, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      let id', freshening_sub = refresh id Subst.add_module freshening_sub in
      prefix_idents root
        ((Sig_module(id', pres, md, rs, vis), p) :: items_and_paths)
        freshening_sub
        (Subst.add_module id' p prefixing_sub)
        rem
    | Sig_modtype(id, mtd, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      let id', freshening_sub =
        refresh id (fun i p s -> Subst.add_modtype i (Mty_ident p) s)
          freshening_sub
      in
      prefix_idents root
        ((Sig_modtype(id', mtd, vis), p) :: items_and_paths)
        freshening_sub
        (Subst.add_modtype id' (Mty_ident p) prefixing_sub)
        rem
    | Sig_class(id, cd, rs, vis) :: rem ->
      (* pretend this is a type, cf. PR#6650 *)
      let p = Pdot(root, Ident.name id) in
      let id', freshening_sub = refresh id Subst.add_type freshening_sub in
      prefix_idents root
        ((Sig_class(id', cd, rs, vis), p) :: items_and_paths)
        freshening_sub
        (Subst.add_type id' p prefixing_sub)
        rem
    | Sig_class_type(id, ctd, rs, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      let id', freshening_sub = refresh id Subst.add_type freshening_sub in
      prefix_idents root
        ((Sig_class_type(id', ctd, rs, vis), p) :: items_and_paths)
        freshening_sub
        (Subst.add_type id' p prefixing_sub)
        rem
  in
  prefix_idents root [] freshening_sub prefixing_sub sg

(* Compute structure descriptions *)

let add_to_tbl id decl tbl =
  let decls = try NameMap.find id tbl with Not_found -> [] in
  NameMap.add id (decl :: decls) tbl

let value_declaration_address (_ : t) id decl =
  match decl.val_kind with
  | Val_prim _ -> EnvLazy.create_failed Not_found
  | _ -> EnvLazy.create_forced (Aident id)

let extension_declaration_address (_ : t) id (_ : extension_constructor) =
  EnvLazy.create_forced (Aident id)

let class_declaration_address (_ : t) id (_ : class_declaration) =
  EnvLazy.create_forced (Aident id)

let module_declaration_address env id presence md =
  match presence with
  | Mp_absent -> begin
      match md.md_type with
      | Mty_alias path -> EnvLazy.create (ModAlias {env; path})
      | _ -> assert false
    end
  | Mp_present ->
      EnvLazy.create_forced (Aident id)

let is_identchar c =
  (* This should be kept in sync with the [identchar_latin1] character class
     in [lexer.mll] *)
  match c with
  | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214'
  | '\216'..'\246' | '\248'..'\255' | '\'' | '0'..'9' ->
    true
  | _ ->
    false

let rec components_of_module_maker
          {cm_env; cm_freshening_subst; cm_prefixing_subst;
           cm_path; cm_addr; cm_mty} : _ result =
  match scrape_alias cm_env cm_freshening_subst cm_mty with
    Mty_signature sg ->
      let c =
        { comp_values = NameMap.empty;
          comp_constrs = NameMap.empty;
          comp_labels = NameMap.empty; comp_types = NameMap.empty;
          comp_modules = NameMap.empty; comp_modtypes = NameMap.empty;
          comp_classes = NameMap.empty; comp_cltypes = NameMap.empty }
      in
      let items_and_paths, freshening_sub, prefixing_sub =
        prefix_idents cm_path cm_freshening_subst cm_prefixing_subst sg
      in
      let env = ref cm_env in
      let pos = ref 0 in
      let next_address () =
        let addr : address_unforced =
          Projection { parent = cm_addr; pos = !pos }
        in
        incr pos;
        EnvLazy.create addr
      in
      let sub = may_subst Subst.compose freshening_sub prefixing_sub in
      List.iter (fun (item, path) ->
        match item with
          Sig_value(id, decl, _) ->
            let decl' = Subst.value_description sub decl in
            let addr =
              match decl.val_kind with
              | Val_prim _ -> EnvLazy.create_failed Not_found
              | _ -> next_address ()
            in
            let vda = { vda_description = decl'; vda_address = addr } in
            c.comp_values <- NameMap.add (Ident.name id) vda c.comp_values;
        | Sig_type(id, decl, _, _) ->
            let fresh_decl =
              may_subst Subst.type_declaration freshening_sub decl
            in
            let final_decl = Subst.type_declaration prefixing_sub fresh_decl in
            Datarepr.set_row_name final_decl
              (Subst.type_path prefixing_sub (Path.Pident id));
            let constructors =
              List.map snd
                (Datarepr.constructors_of_type ~current_unit:(get_unit_name ())
                   path final_decl)
            in
            let labels =
              List.map snd (Datarepr.labels_of_type path final_decl) in
            let tda =
              { tda_declaration = final_decl;
                tda_descriptions = (constructors, labels); }
            in
            c.comp_types <- NameMap.add (Ident.name id) tda c.comp_types;
            List.iter
              (fun descr ->
                 let cda = { cda_description = descr; cda_address = None } in
                 c.comp_constrs <-
                   add_to_tbl descr.cstr_name cda c.comp_constrs)
              constructors;
            List.iter
              (fun descr ->
                c.comp_labels <-
                  add_to_tbl descr.lbl_name descr c.comp_labels)
              labels;
            env := store_type_infos id fresh_decl !env
        | Sig_typext(id, ext, _, _) ->
            let ext' = Subst.extension_constructor sub ext in
            let descr =
              Datarepr.extension_descr ~current_unit:(get_unit_name ()) path
                ext'
            in
            let addr = next_address () in
            let cda = { cda_description = descr; cda_address = Some addr } in
            c.comp_constrs <- add_to_tbl (Ident.name id) cda c.comp_constrs
        | Sig_module(id, pres, md, _, _) ->
            let md' =
              (* The prefixed items get the same scope as [cm_path], which is
                 the prefix. *)
              EnvLazy.create (sub, Subst.Rescope (Path.scope cm_path), md)
            in
            let addr =
              match pres with
              | Mp_absent -> begin
                  match md.md_type with
                  | Mty_alias p ->
                      let path = may_subst Subst.module_path freshening_sub p in
                      EnvLazy.create (ModAlias {env = !env; path})
                  | _ -> assert false
                end
              | Mp_present -> next_address ()
            in
            let alerts =
              Builtin_attributes.alerts_of_attrs md.md_attributes
            in
            let comps =
              components_of_module ~alerts ~uid:md.md_uid !env freshening_sub
                prefixing_sub path addr md.md_type
            in
            let mda =
              { mda_declaration = md';
                mda_components = comps;
                mda_address = addr }
            in
            c.comp_modules <-
              NameMap.add (Ident.name id) mda c.comp_modules;
            env :=
              store_module ~freshening_sub ~check:None id addr pres md !env
        | Sig_modtype(id, decl, _) ->
            let fresh_decl =
              (* the fresh_decl is only going in the local temporary env, and
                 shouldn't be used for anything. So we make the items local. *)
              may_subst (Subst.modtype_declaration Make_local) freshening_sub
                decl
            in
            let final_decl =
              (* The prefixed items get the same scope as [cm_path], which is
                 the prefix. *)
              Subst.modtype_declaration (Rescope (Path.scope cm_path))
                prefixing_sub fresh_decl
            in
            c.comp_modtypes <-
              NameMap.add (Ident.name id) final_decl c.comp_modtypes;
            env := store_modtype id fresh_decl !env
        | Sig_class(id, decl, _, _) ->
            let decl' = Subst.class_declaration sub decl in
            let addr = next_address () in
            let clda = { clda_declaration = decl'; clda_address = addr } in
            c.comp_classes <- NameMap.add (Ident.name id) clda c.comp_classes
        | Sig_class_type(id, decl, _, _) ->
            let decl' = Subst.cltype_declaration sub decl in
            c.comp_cltypes <-
              NameMap.add (Ident.name id) decl' c.comp_cltypes)
        items_and_paths;
        Ok (Structure_comps c)
  | Mty_functor(arg, ty_res) ->
      let sub =
        may_subst Subst.compose cm_freshening_subst cm_prefixing_subst
      in
      let scoping = Subst.Rescope (Path.scope cm_path) in
        Ok (Functor_comps {
          (* fcomp_arg and fcomp_res must be prefixed eagerly, because
             they are interpreted in the outer environment *)
          fcomp_arg =
            (match arg with
            | Unit -> Unit
            | Named (param, ty_arg) ->
              Named (param, Subst.modtype scoping sub ty_arg));
          fcomp_res = Subst.modtype scoping sub ty_res;
          fcomp_cache = Hashtbl.create 17;
          fcomp_subst_cache = Hashtbl.create 17 })
  | Mty_ident _ -> Error No_components_abstract
  | Mty_alias p -> Error (No_components_alias p)

(* Insertion of bindings by identifier + path *)

and check_usage loc id uid warn tbl =
  if not loc.Location.loc_ghost &&
     Uid.for_actual_declaration uid &&
     Warnings.is_active (warn "")
  then begin
    let name = Ident.name id in
    if Types.Uid.Tbl.mem tbl uid then ()
    else let used = ref false in
    Types.Uid.Tbl.add tbl uid (fun () -> used := true);
    if not (name = "" || name.[0] = '_' || name.[0] = '#')
    then
      !add_delayed_check_forward
        (fun () -> if not !used then Location.prerr_warning loc (warn name))
  end;

and check_value_name name loc =
  (* Note: we could also check here general validity of the
     identifier, to protect against bad identifiers forged by -pp or
     -ppx preprocessors. *)
  if String.length name > 0 && not (is_identchar name.[0]) then
    for i = 1 to String.length name - 1 do
      if name.[i] = '#' then
        error (Illegal_value_name(loc, name))
    done

and store_value ?check id addr decl env =
  check_value_name (Ident.name id) decl.val_loc;
  Option.iter
    (fun f -> check_usage decl.val_loc id decl.val_uid f !value_declarations)
    check;
  let vda = { vda_description = decl; vda_address = addr } in
  { env with
    values = IdTbl.add id (Val_bound vda) env.values;
    summary = Env_value(env.summary, id, decl) }

and store_type ~check id info env =
  let loc = info.type_loc in
  if check then
    check_usage loc id info.type_uid
      (fun s -> Warnings.Unused_type_declaration s)
      !type_declarations;
  let path = Pident id in
  let constructors =
    Datarepr.constructors_of_type path info
      ~current_unit:(get_unit_name ())
  in
  let labels = Datarepr.labels_of_type path info in
  let descrs = (List.map snd constructors, List.map snd labels) in
  let tda = { tda_declaration = info; tda_descriptions = descrs } in
  if check && not loc.Location.loc_ghost &&
    Warnings.is_active (Warnings.Unused_constructor ("", false, false))
  then begin
    let ty_name = Ident.name id in
    let priv = info.type_private in
    List.iter
      begin fun (_, cstr) ->
        let name = cstr.cstr_name in
        let loc = cstr.cstr_loc in
        let k = cstr.cstr_uid in
        if not (Types.Uid.Tbl.mem !used_constructors k) then
          let used = constructor_usages () in
          Types.Uid.Tbl.add !used_constructors k
            (add_constructor_usage ~rebind:false priv used);
          if not (ty_name = "" || ty_name.[0] = '_')
          then !add_delayed_check_forward
              (fun () ->
                if not (is_in_signature env) && not used.cu_positive then
                  Location.prerr_warning loc
                    (Warnings.Unused_constructor
                       (name, used.cu_pattern, used.cu_privatize)))
      end
      constructors
  end;
  { env with
    constrs =
      List.fold_right
        (fun (id, descr) constrs ->
           let cda = { cda_description = descr; cda_address = None } in
           TycompTbl.add id cda constrs)
        constructors env.constrs;
    labels =
      List.fold_right
        (fun (id, descr) labels -> TycompTbl.add id descr labels)
        labels env.labels;
    types = IdTbl.add id tda env.types;
    summary = Env_type(env.summary, id, info) }

and store_type_infos id info env =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  let tda = { tda_declaration = info; tda_descriptions = [], [] } in
  { env with
    types = IdTbl.add id tda env.types;
    summary = Env_type(env.summary, id, info) }

and store_extension ~check ~rebind id addr ext env =
  let loc = ext.ext_loc in
  let cstr =
    Datarepr.extension_descr ~current_unit:(get_unit_name ()) (Pident id) ext
  in
  let cda = { cda_description = cstr; cda_address = Some addr } in
  if check && not loc.Location.loc_ghost &&
    Warnings.is_active (Warnings.Unused_extension ("", false, false, false))
  then begin
    let priv = ext.ext_private in
    let is_exception = Path.same ext.ext_type_path Predef.path_exn in
    let name = cstr.cstr_name in
    let k = cstr.cstr_uid in
    if not (Types.Uid.Tbl.mem !used_constructors k) then begin
      let used = constructor_usages () in
      Types.Uid.Tbl.add !used_constructors k
        (add_constructor_usage ~rebind priv used);
      !add_delayed_check_forward
        (fun () ->
          if not (is_in_signature env) && not used.cu_positive then
            Location.prerr_warning loc
              (Warnings.Unused_extension
                 (name, is_exception, used.cu_pattern, used.cu_privatize)
              )
        )
    end;
  end;
  { env with
    constrs = TycompTbl.add id cda env.constrs;
    summary = Env_extension(env.summary, id, ext) }

and store_module ~check ~freshening_sub id addr presence md env =
  let loc = md.md_loc in
  Option.iter
    (fun f -> check_usage loc id md.md_uid f !module_declarations) check;
  let alerts = Builtin_attributes.alerts_of_attrs md.md_attributes in
  let module_decl_lazy =
    match freshening_sub with
    | None -> EnvLazy.create_forced md
    | Some s -> EnvLazy.create (s, Subst.Rescope (Ident.scope id), md)
  in
  let comps =
    components_of_module ~alerts ~uid:md.md_uid
      env freshening_sub Subst.identity (Pident id) addr md.md_type
  in
  let mda =
    { mda_declaration = module_decl_lazy;
      mda_components = comps;
      mda_address = addr }
  in
  { env with
    modules = IdTbl.add id (Mod_local mda) env.modules;
    summary = Env_module(env.summary, id, presence, md) }

and store_modtype id info env =
  { env with
    modtypes = IdTbl.add id info env.modtypes;
    summary = Env_modtype(env.summary, id, info) }

and store_class id addr desc env =
  let clda = { clda_declaration = desc; clda_address = addr } in
  { env with
    classes = IdTbl.add id clda env.classes;
    summary = Env_class(env.summary, id, desc) }

and store_cltype id desc env =
  { env with
    cltypes = IdTbl.add id desc env.cltypes;
    summary = Env_cltype(env.summary, id, desc) }

let scrape_alias env mty = scrape_alias env None mty

(* Compute the components of a functor application in a path. *)

let components_of_functor_appl ~loc f env p1 p2 =
  try
    Hashtbl.find f.fcomp_cache p2
  with Not_found ->
    let p = Papply(p1, p2) in
    let sub =
      match f.fcomp_arg with
      | Unit
      | Named (None, _) -> Subst.identity
      | Named (Some param, _) -> Subst.add_module param p2 Subst.identity
    in
    (* we have to apply eagerly instead of passing sub to [components_of_module]
       because of the call to [check_well_formed_module]. *)
    let mty = Subst.modtype (Rescope (Path.scope p)) sub f.fcomp_res in
    let addr = EnvLazy.create_failed Not_found in
    !check_well_formed_module env loc
      ("the signature of " ^ Path.name p) mty;
    let comps =
      components_of_module ~alerts:Misc.Stdlib.String.Map.empty
        ~uid:Uid.internal_not_actually_unique
        (*???*)
        env None Subst.identity p addr mty
    in
    Hashtbl.add f.fcomp_cache p2 comps;
    comps

(* Define forward functions *)

let _ =
  components_of_functor_appl' := components_of_functor_appl;
  components_of_module_maker' := components_of_module_maker

(* Insertion of bindings by identifier *)

let add_functor_arg id env =
  {env with
   functor_args = Ident.add id () env.functor_args;
   summary = Env_functor_arg (env.summary, id)}

let add_value ?check id desc env =
  let addr = value_declaration_address env id desc in
  store_value ?check id addr desc env

let add_type ~check id info env =
  store_type ~check id info env

and add_extension ~check ~rebind id ext env =
  let addr = extension_declaration_address env id ext in
  store_extension ~check ~rebind id addr ext env

and add_module_declaration ?(arg=false) ~check id presence md env =
  let check =
    if not check then
      None
    else if arg && is_in_signature env then
      Some (fun s -> Warnings.Unused_functor_parameter s)
    else
      Some (fun s -> Warnings.Unused_module s)
  in
  let addr = module_declaration_address env id presence md in
  let env = store_module ~freshening_sub:None ~check id addr presence md env in
  if arg then add_functor_arg id env else env

and add_modtype id info env =
  store_modtype id info env

and add_class id ty env =
  let addr = class_declaration_address env id ty in
  store_class id addr ty env

and add_cltype id ty env =
  store_cltype id ty env

let add_module ?arg id presence mty env =
  add_module_declaration ~check:false ?arg id presence (md mty) env

let add_local_type path info env =
  { env with
    local_constraints = Path.Map.add path info env.local_constraints }


(* Insertion of bindings by name *)

let enter_value ?check name desc env =
  let id = Ident.create_local name in
  let addr = value_declaration_address env id desc in
  let env = store_value ?check id addr desc env in
  (id, env)

let enter_type ~scope name info env =
  let id = Ident.create_scoped ~scope name in
  let env = store_type ~check:true id info env in
  (id, env)

let enter_extension ~scope ~rebind name ext env =
  let id = Ident.create_scoped ~scope name in
  let addr = extension_declaration_address env id ext in
  let env = store_extension ~check:true ~rebind id addr ext env in
  (id, env)

let enter_module_declaration ~scope ?arg s presence md env =
  let id = Ident.create_scoped ~scope s in
  (id, add_module_declaration ?arg ~check:true id presence md env)

let enter_modtype ~scope name mtd env =
  let id = Ident.create_scoped ~scope name in
  let env = store_modtype id mtd env in
  (id, env)

let enter_class ~scope name desc env =
  let id = Ident.create_scoped ~scope name in
  let addr = class_declaration_address env id desc in
  let env = store_class id addr desc env in
  (id, env)

let enter_cltype ~scope name desc env =
  let id = Ident.create_scoped ~scope name in
  let env = store_cltype id desc env in
  (id, env)

let enter_module ~scope ?arg s presence mty env =
  enter_module_declaration ~scope ?arg s presence (md mty) env

(* Insertion of all components of a signature *)

let add_item comp env =
  match comp with
    Sig_value(id, decl, _)    -> add_value id decl env
  | Sig_type(id, decl, _, _)  -> add_type ~check:false id decl env
  | Sig_typext(id, ext, _, _) ->
      add_extension ~check:false ~rebind:false id ext env
  | Sig_module(id, presence, md, _, _) ->
      add_module_declaration ~check:false id presence md env
  | Sig_modtype(id, decl, _)  -> add_modtype id decl env
  | Sig_class(id, decl, _, _) -> add_class id decl env
  | Sig_class_type(id, decl, _, _) -> add_cltype id decl env

let rec add_signature sg env =
  match sg with
    [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)

let enter_signature ~scope sg env =
  let sg = Subst.signature (Rescope scope) Subst.identity sg in
  sg, add_signature sg env

(* Add "unbound" bindings *)

let enter_unbound_value name reason env =
  let id = Ident.create_local name in
  { env with
    values = IdTbl.add id (Val_unbound reason) env.values;
    summary = Env_value_unbound(env.summary, name, reason) }

let enter_unbound_module name reason env =
  let id = Ident.create_local name in
  { env with
    modules = IdTbl.add id (Mod_unbound reason) env.modules;
    summary = Env_module_unbound(env.summary, name, reason) }

(* Open a signature path *)

let add_components slot root env0 comps =
  let add_l w comps env0 =
    TycompTbl.add_open slot w comps env0
  in
  let add w comps env0 = IdTbl.add_open slot w root comps env0 in
  let constrs =
    add_l (fun x -> `Constructor x) comps.comp_constrs env0.constrs
  in
  let labels =
    add_l (fun x -> `Label x) comps.comp_labels env0.labels
  in
  let values =
    add (fun x -> `Value x) comps.comp_values env0.values
  in
  let types =
    add (fun x -> `Type x) comps.comp_types env0.types
  in
  let modtypes =
    add (fun x -> `Module_type x) comps.comp_modtypes env0.modtypes
  in
  let classes =
    add (fun x -> `Class x) comps.comp_classes env0.classes
  in
  let cltypes =
    add (fun x -> `Class_type x) comps.comp_cltypes env0.cltypes
  in
  let modules =
    add (fun x -> `Module x) comps.comp_modules env0.modules
  in
  { env0 with
    summary = Env_open(env0.summary, root);
    constrs;
    labels;
    values;
    types;
    modtypes;
    classes;
    cltypes;
    modules;
  }

let open_signature slot root env0 : (_,_) result =
  match get_components_res (find_module_components root env0) with
  | Error _ -> Error `Not_found
  | exception Not_found -> Error `Not_found
  | Ok (Functor_comps _) -> Error `Functor
  | Ok (Structure_comps comps) ->
    Ok (add_components slot root env0 comps)


(* Open a signature from a file *)

let open_pers_signature name env =
  match open_signature None (Pident(Ident.create_persistent name)) env with
  | (Ok _ | Error `Not_found as res) -> res
  | Error `Functor -> assert false
        (* a compilation unit cannot refer to a functor *)

let open_signature
    ?(used_slot = ref false)
    ?(loc = Location.none) ?(toplevel = false)
    ovf root env =
  let unused =
    match ovf with
    | Asttypes.Fresh -> Warnings.Unused_open (Path.name root)
    | Asttypes.Override -> Warnings.Unused_open_bang (Path.name root)
  in
  let warn_unused =
    Warnings.is_active unused
  and warn_shadow_id =
    Warnings.is_active (Warnings.Open_shadow_identifier ("", ""))
  and warn_shadow_lc =
    Warnings.is_active (Warnings.Open_shadow_label_constructor ("",""))
  in
  if not toplevel && not loc.Location.loc_ghost
     && (warn_unused || warn_shadow_id || warn_shadow_lc)
  then begin
    let used = used_slot in
    if warn_unused then
      !add_delayed_check_forward
        (fun () ->
           if not !used then begin
             used := true;
             Location.prerr_warning loc unused
           end
        );
    let shadowed = ref [] in
    let slot s b =
      begin match check_shadowing env b with
      | Some kind when
          ovf = Asttypes.Fresh && not (List.mem (kind, s) !shadowed) ->
          shadowed := (kind, s) :: !shadowed;
          let w =
            match kind with
            | "label" | "constructor" ->
                Warnings.Open_shadow_label_constructor (kind, s)
            | _ -> Warnings.Open_shadow_identifier (kind, s)
          in
          Location.prerr_warning loc w
      | _ -> ()
      end;
      used := true
    in
    open_signature (Some slot) root env
  end
  else open_signature None root env

(* Read a signature from a file *)
let read_signature modname filename =
  let mda = read_pers_mod modname filename in
  let md = EnvLazy.force subst_modtype_maker mda.mda_declaration in
  match md.md_type with
  | Mty_signature sg -> sg
  | Mty_ident _ | Mty_functor _ | Mty_alias _ -> assert false

let is_identchar_latin1 = function
  | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214' | '\216'..'\246'
  | '\248'..'\255' | '\'' | '0'..'9' -> true
  | _ -> false

let unit_name_of_filename fn =
  match Filename.extension fn with
  | ".cmi" -> begin
      let unit =
        String.capitalize_ascii (Filename.remove_extension fn)
      in
      if String.for_all is_identchar_latin1 unit then
        Some unit
      else
        None
    end
  | _ -> None

let persistent_structures_of_dir dir =
  Load_path.Dir.files dir
  |> List.to_seq
  |> Seq.filter_map unit_name_of_filename
  |> String.Set.of_seq

(* Save a signature to a file *)
let save_signature_with_transform cmi_transform ~alerts sg modname filename =
  Btype.cleanup_abbrev ();
  Subst.reset_for_saving ();
  let sg = Subst.signature Make_local (Subst.for_saving Subst.identity) sg in
  let cmi =
    Persistent_env.make_cmi !persistent_env modname sg alerts
    |> cmi_transform in
  let pm = save_sign_of_cmi
      { Persistent_env.Persistent_signature.cmi; filename } in
  Persistent_env.save_cmi !persistent_env
    { Persistent_env.Persistent_signature.filename; cmi } pm;
  cmi

let save_signature ~alerts sg modname filename =
  save_signature_with_transform (fun cmi -> cmi)
    ~alerts sg modname filename

let save_signature_with_imports ~alerts sg modname filename imports =
  let with_imports cmi = { cmi with cmi_crcs = imports } in
  save_signature_with_transform with_imports
    ~alerts sg modname filename

(* Make the initial environment *)
let (initial_safe_string, initial_unsafe_string) =
  Predef.build_initial_env
    (add_type ~check:false)
    (add_extension ~check:false ~rebind:false)
    empty

(* Tracking usage *)

let mark_module_used uid =
  match Types.Uid.Tbl.find !module_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_modtype_used _uid = ()

let mark_value_used uid =
  match Types.Uid.Tbl.find !value_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_type_used uid =
  match Types.Uid.Tbl.find !type_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_type_path_used env path =
  match find_type path env with
  | decl -> mark_type_used decl.type_uid
  | exception Not_found -> ()

let mark_constructor_used usage cd =
  match Types.Uid.Tbl.find !used_constructors cd.cd_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_extension_used usage ext =
  match Types.Uid.Tbl.find !used_constructors ext.ext_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_constructor_description_used usage env cstr =
  let ty_path =
    match repr cstr.cstr_res with
    | {desc=Tconstr(path, _, _)} -> path
    | _ -> assert false
  in
  mark_type_path_used env ty_path;
  match Types.Uid.Tbl.find !used_constructors cstr.cstr_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_label_description_used () env lbl =
  let ty_path =
    match repr lbl.lbl_res with
    | {desc=Tconstr(path, _, _)} -> path
    | _ -> assert false
  in
  mark_type_path_used env ty_path

let mark_class_used uid =
  match Types.Uid.Tbl.find !type_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_cltype_used uid =
  match Types.Uid.Tbl.find !type_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let set_value_used_callback vd callback =
  Types.Uid.Tbl.add !value_declarations vd.val_uid callback

let set_type_used_callback td callback =
  if Uid.for_actual_declaration td.type_uid then
    let old =
      try Types.Uid.Tbl.find !type_declarations td.type_uid
      with Not_found -> ignore
    in
    Types.Uid.Tbl.replace !type_declarations td.type_uid
      (fun () -> callback old)

(* Lookup by name *)

let may_lookup_error report_errors loc env err =
  if report_errors then lookup_error loc env err
  else raise Not_found

let report_module_unbound ~errors ~loc env reason =
  match reason with
  | Mod_unbound_illegal_recursion ->
      (* see #5965 *)
    may_lookup_error errors loc env Illegal_reference_to_recursive_module

let report_value_unbound ~errors ~loc env reason lid =
  match reason with
  | Val_unbound_instance_variable ->
      may_lookup_error errors loc env (Masked_instance_variable lid)
  | Val_unbound_self ->
      may_lookup_error errors loc env (Masked_self_variable lid)
  | Val_unbound_ancestor ->
      may_lookup_error errors loc env (Masked_ancestor_variable lid)
  | Val_unbound_ghost_recursive rloc ->
      let show_hint =
        (* Only display the "missing rec" hint for non-ghost code *)
        not loc.Location.loc_ghost
        && not rloc.Location.loc_ghost
      in
      let hint =
        if show_hint then Missing_rec rloc else No_hint
      in
      may_lookup_error errors loc env (Unbound_value(lid, hint))

let use_module ~use ~loc path mda =
  if use then begin
    let comps = mda.mda_components in
    mark_module_used comps.uid;
    Misc.Stdlib.String.Map.iter
      (fun kind message ->
         let message = if message = "" then "" else "\n" ^ message in
         Location.alert ~kind loc
           (Printf.sprintf "module %s%s" (Path.name path) message)
      )
      comps.alerts
  end

let use_value ~use ~loc path vda =
  if use then begin
    let desc = vda.vda_description in
    mark_value_used desc.val_uid;
    Builtin_attributes.check_alerts loc desc.val_attributes
      (Path.name path)
  end

let use_type ~use ~loc path tda =
  if use then begin
    let decl = tda.tda_declaration in
    mark_type_used decl.type_uid;
    Builtin_attributes.check_alerts loc decl.type_attributes
      (Path.name path)
  end

let use_modtype ~use ~loc path desc =
  if use then begin
    mark_modtype_used desc.mtd_uid;
    Builtin_attributes.check_alerts loc desc.mtd_attributes
      (Path.name path)
  end

let use_class ~use ~loc path clda =
  if use then begin
    let desc = clda.clda_declaration in
    mark_class_used desc.cty_uid;
    Builtin_attributes.check_alerts loc desc.cty_attributes
      (Path.name path)
  end

let use_cltype ~use ~loc path desc =
  if use then begin
    mark_cltype_used desc.clty_uid;
    Builtin_attributes.check_alerts loc desc.clty_attributes
      (Path.name path)
  end

let use_label ~use ~loc env lbl =
  if use then begin
    mark_label_description_used () env lbl;
    Builtin_attributes.check_alerts loc lbl.lbl_attributes lbl.lbl_name
  end

let use_constructor_desc ~use ~loc usage env cstr =
  if use then begin
    mark_constructor_description_used usage env cstr;
    Builtin_attributes.check_alerts loc cstr.cstr_attributes cstr.cstr_name
  end

let use_constructor ~use ~loc usage env cda =
  use_constructor_desc ~use ~loc usage env cda.cda_description

type _ load =
  | Load : module_data load
  | Don't_load : unit load

let lookup_ident_module (type a) (load : a load) ~errors ~use ~loc s env =
  let path, data =
    match find_name_module ~mark:use s env.modules with
    | res -> res
    | exception Not_found ->
        may_lookup_error errors loc env (Unbound_module (Lident s))
  in
  match data with
  | Mod_local mda -> begin
      use_module ~use ~loc path mda;
      match load with
      | Load -> path, (mda : a)
      | Don't_load -> path, (() : a)
    end
  | Mod_unbound reason ->
      report_module_unbound ~errors ~loc env reason
  | Mod_persistent -> begin
      match load with
      | Don't_load ->
          check_pers_mod ~loc s;
          path, (() : a)
      | Load -> begin
          match find_pers_mod s with
          | mda ->
              use_module ~use ~loc path mda;
              path, (mda : a)
          | exception Not_found ->
              may_lookup_error errors loc env (Unbound_module (Lident s))
        end
    end

let lookup_ident_value ~errors ~use ~loc name env =
  match IdTbl.find_name wrap_value ~mark:use name env.values with
  | (path, Val_bound vda) ->
      use_value ~use ~loc path vda;
      path, vda.vda_description
  | (_, Val_unbound reason) ->
      report_value_unbound ~errors ~loc env reason (Lident name)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_value (Lident name, No_hint))

let lookup_ident_type ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.types with
  | (path, data) as res ->
      use_type ~use ~loc path data;
      res
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_type (Lident s))

let lookup_ident_modtype ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.modtypes with
  | (path, data) as res ->
      use_modtype ~use ~loc path data;
      res
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_modtype (Lident s))

let lookup_ident_class ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.classes with
  | (path, clda) ->
      use_class ~use ~loc path clda;
      path, clda.clda_declaration
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_class (Lident s))

let lookup_ident_cltype ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.cltypes with
  | (path, data) as res ->
      use_cltype ~use ~loc path data;
      res
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_cltype (Lident s))

let lookup_all_ident_labels ~errors ~use ~loc s env =
  match TycompTbl.find_all ~mark:use s env.labels with
  | [] -> may_lookup_error errors loc env (Unbound_label (Lident s))
  | lbls -> begin
      List.map
        (fun (lbl, use_fn) ->
           let use_fn () =
             use_label ~use ~loc env lbl;
             use_fn ()
           in
           (lbl, use_fn))
        lbls
    end

let lookup_all_ident_constructors ~errors ~use ~loc usage s env =
  match TycompTbl.find_all ~mark:use s env.constrs with
  | [] -> may_lookup_error errors loc env (Unbound_constructor (Lident s))
  | cstrs ->
      List.map
        (fun (cda, use_fn) ->
           let use_fn () =
             use_constructor ~use ~loc usage env cda;
             use_fn ()
           in
           (cda.cda_description, use_fn))
        cstrs

let rec lookup_module_components ~errors ~use ~loc lid env =
  match lid with
  | Lident s ->
      let path, data = lookup_ident_module Load ~errors ~use ~loc s env in
      path, data.mda_components
  | Ldot(l, s) ->
      let path, data = lookup_dot_module ~errors ~use ~loc l s env in
      path, data.mda_components
  | Lapply(l1, l2) ->
      let p1, f, arg = lookup_functor_components ~errors ~use ~loc l1 env in
      let p2, md = lookup_module ~errors ~use ~loc l2 env in
      check_functor_appl ~errors ~loc env p1 f arg p2 md;
      let comps = !components_of_functor_appl' ~loc f env p1 p2 in
      (Papply(p1, p2), comps)

and lookup_structure_components ~errors ~use ~loc lid env =
  let path, comps = lookup_module_components ~errors ~use ~loc lid env in
  match get_components_res comps with
  | Ok (Structure_comps comps) -> path, comps
  | Ok (Functor_comps _) ->
      may_lookup_error errors loc env (Functor_used_as_structure lid)
  | Error No_components_abstract ->
      may_lookup_error errors loc env (Abstract_used_as_structure lid)
  | Error (No_components_alias p) ->
      may_lookup_error errors loc env (Cannot_scrape_alias(lid, p))

and lookup_functor_components ~errors ~use ~loc lid env =
  let path, comps = lookup_module_components ~errors ~use ~loc lid env in
  match get_components_res comps with
  | Ok (Functor_comps fcomps) -> begin
      match fcomps.fcomp_arg with
      | Unit -> (* PR#7611 *)
          may_lookup_error errors loc env (Generative_used_as_applicative lid)
      | Named (_, arg) -> path, fcomps, arg
    end
  | Ok (Structure_comps _) ->
      may_lookup_error errors loc env (Structure_used_as_functor lid)
  | Error No_components_abstract ->
      may_lookup_error errors loc env (Abstract_used_as_functor lid)
  | Error (No_components_alias p) ->
      may_lookup_error errors loc env (Cannot_scrape_alias(lid, p))

and lookup_module ~errors ~use ~loc lid env =
  match lid with
  | Lident s ->
      let path, data = lookup_ident_module Load ~errors ~use ~loc s env in
      let md = EnvLazy.force subst_modtype_maker data.mda_declaration in
      path, md
  | Ldot(l, s) ->
      let path, data = lookup_dot_module ~errors ~use ~loc l s env in
      let md = EnvLazy.force subst_modtype_maker data.mda_declaration in
      path, md
  | Lapply(l1, l2) ->
      let p1, fc, arg = lookup_functor_components ~errors ~use ~loc l1 env in
      let p2, md2 = lookup_module ~errors ~use ~loc l2 env in
      check_functor_appl ~errors ~loc env p1 fc arg p2 md2;
      let md = md (modtype_of_functor_appl fc p1 p2) in
      Papply(p1, p2), md

and lookup_dot_module ~errors ~use ~loc l s env =
  let p, comps = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_modules with
  | mda ->
      let path = Pdot(p, s) in
      use_module ~use ~loc path mda;
      (path, mda)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_module (Ldot(l, s)))

let lookup_dot_value ~errors ~use ~loc l s env =
  let (path, comps) =
    lookup_structure_components ~errors ~use ~loc l env
  in
  match NameMap.find s comps.comp_values with
  | vda ->
      let path = Pdot(path, s) in
      use_value ~use ~loc path vda;
      (path, vda.vda_description)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_value (Ldot(l, s), No_hint))

let lookup_dot_type ~errors ~use ~loc l s env =
  let (p, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_types with
  | tda ->
      let path = Pdot(p, s) in
      use_type ~use ~loc path tda;
      (path, tda)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_type (Ldot(l, s)))

let lookup_dot_modtype ~errors ~use ~loc l s env =
  let (p, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_modtypes with
  | desc ->
      let path = Pdot(p, s) in
      use_modtype ~use ~loc path desc;
      (path, desc)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_modtype (Ldot(l, s)))

let lookup_dot_class ~errors ~use ~loc l s env =
  let (p, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_classes with
  | clda ->
      let path = Pdot(p, s) in
      use_class ~use ~loc path clda;
      (path, clda.clda_declaration)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_class (Ldot(l, s)))

let lookup_dot_cltype ~errors ~use ~loc l s env =
  let (p, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_cltypes with
  | desc ->
      let path = Pdot(p, s) in
      use_cltype ~use ~loc path desc;
      (path, desc)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_cltype (Ldot(l, s)))

let lookup_all_dot_labels ~errors ~use ~loc l s env =
  let (_, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_labels with
  | [] | exception Not_found ->
      may_lookup_error errors loc env (Unbound_label (Ldot(l, s)))
  | lbls ->
      List.map
        (fun lbl ->
           let use_fun () = use_label ~use ~loc env lbl in
           (lbl, use_fun))
        lbls

let lookup_all_dot_constructors ~errors ~use ~loc usage l s env =
  match l with
  | Longident.Lident "*predef*" ->
      (* Hack to support compilation of default arguments *)
      lookup_all_ident_constructors
        ~errors ~use ~loc usage s initial_safe_string
  | _ ->
      let (_, comps) = lookup_structure_components ~errors ~use ~loc l env in
      match NameMap.find s comps.comp_constrs with
      | [] | exception Not_found ->
          may_lookup_error errors loc env (Unbound_constructor (Ldot(l, s)))
      | cstrs ->
          List.map
            (fun cda ->
               let use_fun () = use_constructor ~use ~loc usage env cda in
               (cda.cda_description, use_fun))
            cstrs

(* General forms of the lookup functions *)

let lookup_module_path ~errors ~use ~loc ~load lid env : Path.t =
  match lid with
  | Lident s ->
      if !Clflags.transparent_modules && not load then
        fst (lookup_ident_module Don't_load ~errors ~use ~loc s env)
      else
        fst (lookup_ident_module Load ~errors ~use ~loc s env)
  | Ldot(l, s) -> fst (lookup_dot_module ~errors ~use ~loc l s env)
  | Lapply(l1, l2) ->
      let (p1, f, arg) = lookup_functor_components ~errors ~use ~loc l1 env in
      let p2, md2 = lookup_module ~errors ~use ~loc l2 env in
      check_functor_appl ~errors ~loc env p1 f arg p2 md2;
      Papply(p1, p2)

let lookup_value ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_value ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_value ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_type_full ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_type ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_type ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_type ~errors ~use ~loc lid env =
  let (path, tda) = lookup_type_full ~errors ~use ~loc lid env in
  path, tda.tda_declaration

let lookup_modtype ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_modtype ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_modtype ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_class ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_class ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_class ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_cltype ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_cltype ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_cltype ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_all_labels ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_all_ident_labels ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_all_dot_labels ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_label ~errors ~use ~loc lid env =
  match lookup_all_labels ~errors ~use ~loc lid env with
  | [] -> assert false
  | (desc, use) :: _ -> use (); desc

let lookup_all_labels_from_type ~use ~loc ty_path env =
  match find_type_descrs ty_path env with
  | exception Not_found -> []
  | (_, lbls) ->
      List.map
        (fun lbl ->
           let use_fun () = use_label ~use ~loc env lbl in
           (lbl, use_fun))
        lbls

let lookup_all_constructors ~errors ~use ~loc usage lid env =
  match lid with
  | Lident s -> lookup_all_ident_constructors ~errors ~use ~loc usage s env
  | Ldot(l, s) -> lookup_all_dot_constructors ~errors ~use ~loc usage l s env
  | Lapply _ -> assert false

let lookup_constructor ~errors ~use ~loc usage lid env =
  match lookup_all_constructors ~errors ~use ~loc usage lid env with
  | [] -> assert false
  | (desc, use) :: _ -> use (); desc

let lookup_all_constructors_from_type ~use ~loc usage ty_path env =
  match find_type_descrs ty_path env with
  | exception Not_found -> []
  | (cstrs, _) ->
      List.map
        (fun cstr ->
           let use_fun () =
             use_constructor_desc ~use ~loc usage env cstr
           in
           (cstr, use_fun))
        cstrs

(* Lookup functions that do not mark the item as used or
   warn if it has alerts, and raise [Not_found] rather
   than report errors *)

let find_module_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_module ~errors:false ~use:false ~loc lid env

let find_value_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_value ~errors:false ~use:false ~loc lid env

let find_type_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_type ~errors:false ~use:false ~loc lid env

let find_modtype_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_modtype ~errors:false ~use:false ~loc lid env

let find_class_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_class ~errors:false ~use:false ~loc lid env

let find_cltype_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_cltype ~errors:false ~use:false ~loc lid env

let find_constructor_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_constructor ~errors:false ~use:false ~loc Positive lid env

let find_label_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_label ~errors:false ~use:false ~loc lid env

(* Ordinary lookup functions *)

let lookup_module_path ?(use=true) ~loc ~load lid env =
  lookup_module_path ~errors:true ~use ~loc ~load lid env

let lookup_module ?(use=true) ~loc lid env =
  lookup_module ~errors:true ~use ~loc lid env

let lookup_value ?(use=true) ~loc lid env =
  check_value_name (Longident.last lid) loc;
  lookup_value ~errors:true ~use ~loc lid env

let lookup_type ?(use=true) ~loc lid env =
  lookup_type ~errors:true ~use ~loc lid env

let lookup_modtype ?(use=true) ~loc lid env =
  lookup_modtype ~errors:true ~use ~loc lid env

let lookup_class ?(use=true) ~loc lid env =
  lookup_class ~errors:true ~use ~loc lid env

let lookup_cltype ?(use=true) ~loc lid env =
  lookup_cltype ~errors:true ~use ~loc lid env

let lookup_all_constructors ?(use=true) ~loc usage lid env =
  match lookup_all_constructors ~errors:true ~use ~loc usage lid env with
  | exception Error(Lookup_error(loc', env', err)) ->
      (Error(loc', env', err) : _ result)
  | cstrs -> Ok cstrs

let lookup_constructor ?(use=true) ~loc lid env =
  lookup_constructor ~errors:true ~use ~loc lid env

let lookup_all_constructors_from_type ?(use=true) ~loc usage ty_path env =
  lookup_all_constructors_from_type ~use ~loc usage ty_path env

let lookup_all_labels ?(use=true) ~loc lid env =
  match lookup_all_labels ~errors:true ~use ~loc lid env with
  | exception Error(Lookup_error(loc', env', err)) ->
      (Error(loc', env', err) : _ result)
  | lbls -> Ok lbls

let lookup_label ?(use=true) ~loc lid env =
  lookup_label ~errors:true ~use ~loc lid env

let lookup_all_labels_from_type ?(use=true) ~loc ty_path env =
  lookup_all_labels_from_type ~use ~loc ty_path env

let lookup_instance_variable ?(use=true) ~loc name env =
  match IdTbl.find_name wrap_value ~mark:use name env.values with
  | (path, Val_bound vda) -> begin
      let desc = vda.vda_description in
      match desc.val_kind with
      | Val_ivar(mut, cl_num) ->
          use_value ~use ~loc path vda;
          path, mut, cl_num, desc.val_type
      | _ ->
          lookup_error loc env (Not_an_instance_variable name)
    end
  | (_, Val_unbound Val_unbound_instance_variable) ->
      lookup_error loc env (Masked_instance_variable (Lident name))
  | (_, Val_unbound Val_unbound_self) ->
      lookup_error loc env (Not_an_instance_variable name)
  | (_, Val_unbound Val_unbound_ancestor) ->
      lookup_error loc env (Not_an_instance_variable name)
  | (_, Val_unbound Val_unbound_ghost_recursive _) ->
      lookup_error loc env (Unbound_instance_variable name)
  | exception Not_found ->
      lookup_error loc env (Unbound_instance_variable name)

(* Checking if a name is bound *)

let bound_module name env =
  match IdTbl.find_name wrap_module ~mark:false name env.modules with
  | _ -> true
  | exception Not_found ->
      if Current_unit_name.is name then false
      else begin
        match find_pers_mod name with
        | _ -> true
        | exception Not_found -> false
      end

let bound wrap proj name env =
  match IdTbl.find_name wrap ~mark:false name (proj env) with
  | _ -> true
  | exception Not_found -> false

let bound_value name env =
  bound wrap_value (fun env -> env.values) name env

let bound_type name env =
  bound wrap_identity (fun env -> env.types) name env

let bound_modtype name env =
  bound wrap_identity (fun env -> env.modtypes) name env

let bound_class name env =
  bound wrap_identity (fun env -> env.classes) name env

let bound_cltype name env =
  bound wrap_identity (fun env -> env.cltypes) name env

(* Folding on environments *)

let find_all wrap proj1 proj2 f lid env acc =
  match lid with
  | None ->
      IdTbl.fold_name wrap
        (fun name (p, data) acc -> f name p data acc)
        (proj1 env) acc
  | Some l ->
      let p, desc =
        lookup_module_components
          ~errors:false ~use:false ~loc:Location.none l env
      in
      begin match get_components desc with
      | Structure_comps c ->
          NameMap.fold
            (fun s data acc -> f s (Pdot (p, s)) (wrap data) acc)
            (proj2 c) acc
      | Functor_comps _ ->
          acc
      end

let find_all_simple_list proj1 proj2 f lid env acc =
  match lid with
  | None ->
      TycompTbl.fold_name
        (fun data acc -> f data acc)
        (proj1 env) acc
  | Some l ->
      let (_p, desc) =
        lookup_module_components
          ~errors:false ~use:false ~loc:Location.none l env
      in
      begin match get_components desc with
      | Structure_comps c ->
          NameMap.fold
            (fun _s comps acc ->
               match comps with
               | [] -> acc
               | data :: _ -> f data acc)
            (proj2 c) acc
      | Functor_comps _ ->
          acc
      end

let fold_modules f lid env acc =
  match lid with
  | None ->
      IdTbl.fold_name wrap_module
        (fun name (p, entry) acc ->
           match entry with
           | Mod_unbound _ -> acc
           | Mod_local mda ->
               let md =
                 EnvLazy.force subst_modtype_maker mda.mda_declaration
               in
               f name p md acc
           | Mod_persistent ->
               match Persistent_env.find_in_cache !persistent_env name with
               | None -> acc
               | Some mda ->
                   let md =
                     EnvLazy.force subst_modtype_maker mda.mda_declaration
                   in
                   f name p md acc)
        env.modules
        acc
  | Some l ->
      let p, desc =
        lookup_module_components
          ~errors:false ~use:false ~loc:Location.none l env
      in
      begin match get_components desc with
      | Structure_comps c ->
          NameMap.fold
            (fun s mda acc ->
               let md =
                 EnvLazy.force subst_modtype_maker mda.mda_declaration
               in
               f s (Pdot (p, s)) md acc)
            c.comp_modules
            acc
      | Functor_comps _ ->
          acc
      end

let fold_values f =
  find_all wrap_value (fun env -> env.values) (fun sc -> sc.comp_values)
    (fun k p ve acc ->
       match ve with
       | Val_unbound _ -> acc
       | Val_bound vda -> f k p vda.vda_description acc)
and fold_constructors f =
  find_all_simple_list (fun env -> env.constrs) (fun sc -> sc.comp_constrs)
    (fun cda acc -> f cda.cda_description acc)
and fold_labels f =
  find_all_simple_list (fun env -> env.labels) (fun sc -> sc.comp_labels) f
and fold_types f =
  find_all wrap_identity
    (fun env -> env.types) (fun sc -> sc.comp_types)
    (fun k p tda acc -> f k p tda.tda_declaration acc)
and fold_modtypes f =
  find_all wrap_identity
    (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) f
and fold_classes f =
  find_all wrap_identity (fun env -> env.classes) (fun sc -> sc.comp_classes)
    (fun k p clda acc -> f k p clda.clda_declaration acc)
and fold_cltypes f =
  find_all wrap_identity
    (fun env -> env.cltypes) (fun sc -> sc.comp_cltypes) f

let filter_non_loaded_persistent f env =
  let to_remove =
    IdTbl.fold_name wrap_module
      (fun name (_, entry) acc ->
         match entry with
         | Mod_local _ -> acc
         | Mod_unbound _ -> acc
         | Mod_persistent ->
             match Persistent_env.find_in_cache !persistent_env name with
             | Some _ -> acc
             | None ->
                 if f (Ident.create_persistent name) then
                   acc
                 else
                   String.Set.add name acc)
      env.modules
      String.Set.empty
  in
  let remove_ids tbl ids =
    String.Set.fold
      (fun name tbl -> IdTbl.remove (Ident.create_persistent name) tbl)
      ids
      tbl
  in
  let rec filter_summary summary ids =
    if String.Set.is_empty ids then
      summary
    else
      match summary with
      | Env_empty -> summary
      | Env_value (s, id, vd) ->
          Env_value (filter_summary s ids, id, vd)
      | Env_type (s, id, td) ->
          Env_type (filter_summary s ids, id, td)
      | Env_extension (s, id, ec) ->
          Env_extension (filter_summary s ids, id, ec)
      | Env_module (s, id, mp, md) ->
          Env_module (filter_summary s ids, id, mp, md)
      | Env_modtype (s, id, md) ->
          Env_modtype (filter_summary s ids, id, md)
      | Env_class (s, id, cd) ->
          Env_class (filter_summary s ids, id, cd)
      | Env_cltype (s, id, ctd) ->
          Env_cltype (filter_summary s ids, id, ctd)
      | Env_open (s, p) ->
          Env_open (filter_summary s ids, p)
      | Env_functor_arg (s, id) ->
          Env_functor_arg (filter_summary s ids, id)
      | Env_constraints (s, cstrs) ->
          Env_constraints (filter_summary s ids, cstrs)
      | Env_copy_types s ->
          Env_copy_types (filter_summary s ids)
      | Env_persistent (s, id) ->
          if String.Set.mem (Ident.name id) ids then
            filter_summary s (String.Set.remove (Ident.name id) ids)
          else
            Env_persistent (filter_summary s ids, id)
      | Env_value_unbound (s, n, r) ->
          Env_value_unbound (filter_summary s ids, n, r)
      | Env_module_unbound (s, n, r) ->
          Env_module_unbound (filter_summary s ids, n, r)
  in
  { env with
    modules = remove_ids env.modules to_remove;
    summary = filter_summary env.summary to_remove;
  }

(* Return the environment summary *)

let summary env =
  if Path.Map.is_empty env.local_constraints then env.summary
  else Env_constraints (env.summary, env.local_constraints)

let last_env = s_ref empty
let last_reduced_env = s_ref empty

let keep_only_summary env =
  if !last_env == env then !last_reduced_env
  else begin
    let new_env =
      {
       empty with
       summary = env.summary;
       local_constraints = env.local_constraints;
       flags = env.flags;
      }
    in
    last_env := env;
    last_reduced_env := new_env;
    new_env
  end


let env_of_only_summary env_from_summary env =
  let new_env = env_from_summary env.summary Subst.identity in
  { new_env with
    local_constraints = env.local_constraints;
    flags = env.flags;
  }

(* Error report *)

open Format

(* Forward declarations *)

let print_longident =
  ref ((fun _ _ -> assert false) : formatter -> Longident.t -> unit)

let print_path =
  ref ((fun _ _ -> assert false) : formatter -> Path.t -> unit)

let spellcheck ppf extract env lid =
  let choices ~path name = Misc.spellcheck (extract path env) name in
  match lid with
    | Longident.Lapply _ -> ()
    | Longident.Lident s ->
       Misc.did_you_mean ppf (fun () -> choices ~path:None s)
    | Longident.Ldot (r, s) ->
       Misc.did_you_mean ppf (fun () -> choices ~path:(Some r) s)

let spellcheck_name ppf extract env name =
  Misc.did_you_mean ppf
    (fun () -> Misc.spellcheck (extract env) name)

let extract_values path env =
  fold_values (fun name _ _ acc -> name :: acc) path env []
let extract_types path env =
  fold_types (fun name _ _ acc -> name :: acc) path env []
let extract_modules path env =
  fold_modules (fun name _ _ acc -> name :: acc) path env []
let extract_constructors path env =
  fold_constructors (fun desc acc -> desc.cstr_name :: acc) path env []
let extract_labels path env =
  fold_labels (fun desc acc -> desc.lbl_name :: acc) path env []
let extract_classes path env =
  fold_classes (fun name _ _ acc -> name :: acc) path env []
let extract_modtypes path env =
  fold_modtypes (fun name _ _ acc -> name :: acc) path env []
let extract_cltypes path env =
  fold_cltypes (fun name _ _ acc -> name :: acc) path env []
let extract_instance_variables env =
  fold_values
    (fun name _ descr acc ->
       match descr.val_kind with
       | Val_ivar _ -> name :: acc
       | _ -> acc) None env []

let report_lookup_error _loc env ppf = function
  | Unbound_value(lid, hint) -> begin
      fprintf ppf "Unbound value %a" !print_longident lid;
      spellcheck ppf extract_values env lid;
      match hint with
      | No_hint -> ()
      | Missing_rec def_loc ->
          let (_, line, _) =
            Location.get_pos_info def_loc.Location.loc_start
          in
          fprintf ppf
            "@.@[%s@ %s %i@]"
            "Hint: If this is a recursive definition,"
            "you should add the 'rec' keyword on line"
            line
    end
  | Unbound_type lid ->
      fprintf ppf "Unbound type constructor %a" !print_longident lid;
      spellcheck ppf extract_types env lid;
  | Unbound_module lid -> begin
      fprintf ppf "Unbound module %a" !print_longident lid;
       match find_modtype_by_name lid env with
      | exception Not_found -> spellcheck ppf extract_modules env lid;
      | _ ->
         fprintf ppf
           "@.@[%s %a, %s@]"
           "Hint: There is a module type named"
           !print_longident lid
           "but module types are not modules"
    end
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" !print_longident lid;
      spellcheck ppf extract_constructors env lid;
  | Unbound_label lid ->
      fprintf ppf "Unbound record field %a" !print_longident lid;
      spellcheck ppf extract_labels env lid;
  | Unbound_class lid -> begin
      fprintf ppf "Unbound class %a" !print_longident lid;
      match find_cltype_by_name lid env with
      | exception Not_found -> spellcheck ppf extract_classes env lid;
      | _ ->
         fprintf ppf
           "@.@[%s %a, %s@]"
           "Hint: There is a class type named"
           !print_longident lid
           "but classes are not class types"
    end
  | Unbound_modtype lid -> begin
      fprintf ppf "Unbound module type %a" !print_longident lid;
      match find_module_by_name lid env with
      | exception Not_found -> spellcheck ppf extract_modtypes env lid;
      | _ ->
         fprintf ppf
           "@.@[%s %a, %s@]"
           "Hint: There is a module named"
           !print_longident lid
           "but modules are not module types"
    end
  | Unbound_cltype lid ->
      fprintf ppf "Unbound class type %a" !print_longident lid;
      spellcheck ppf extract_cltypes env lid;
  | Unbound_instance_variable s ->
      fprintf ppf "Unbound instance variable %s" s;
      spellcheck_name ppf extract_instance_variables env s;
  | Not_an_instance_variable s ->
      fprintf ppf "The value %s is not an instance variable" s;
      spellcheck_name ppf extract_instance_variables env s;
  | Masked_instance_variable lid ->
      fprintf ppf
        "The instance variable %a@ \
         cannot be accessed from the definition of another instance variable"
        !print_longident lid
  | Masked_self_variable lid ->
      fprintf ppf
        "The self variable %a@ \
         cannot be accessed from the definition of an instance variable"
        !print_longident lid
  | Masked_ancestor_variable lid ->
      fprintf ppf
        "The ancestor variable %a@ \
         cannot be accessed from the definition of an instance variable"
        !print_longident lid
  | Illegal_reference_to_recursive_module ->
     fprintf ppf "Illegal recursive module reference"
  | Structure_used_as_functor lid ->
      fprintf ppf "@[The module %a is a structure, it cannot be applied@]"
        !print_longident lid
  | Abstract_used_as_functor lid ->
      fprintf ppf "@[The module %a is abstract, it cannot be applied@]"
        !print_longident lid
  | Functor_used_as_structure lid ->
      fprintf ppf "@[The module %a is a functor, \
                   it cannot have any components@]" !print_longident lid
  | Abstract_used_as_structure lid ->
      fprintf ppf "@[The module %a is abstract, \
                   it cannot have any components@]" !print_longident lid
  | Generative_used_as_applicative lid ->
      fprintf ppf "@[The functor %a is generative,@ it@ cannot@ be@ \
                   applied@ in@ type@ expressions@]" !print_longident lid
  | Cannot_scrape_alias(lid, p) ->
      let cause =
        if Current_unit_name.is_path p then "is the current compilation unit"
        else "is missing"
      in
      fprintf ppf
        "The module %a is an alias for module %a, which %s"
        !print_longident lid !print_path p cause

let report_error ppf = function
  | Missing_module(_, path1, path2) ->
      fprintf ppf "@[@[<hov>";
      if Path.same path1 path2 then
        fprintf ppf "Internal path@ %s@ is dangling." (Path.name path1)
      else
        fprintf ppf "Internal path@ %s@ expands to@ %s@ which is dangling."
          (Path.name path1) (Path.name path2);
      fprintf ppf "@]@ @[%s@ %s@ %s.@]@]"
        "The compiled interface for module" (Ident.name (Path.head path2))
        "was not found"
  | Illegal_value_name(_loc, name) ->
      fprintf ppf "'%s' is not a valid value identifier."
        name
  | Lookup_error(loc, t, err) -> report_lookup_error loc t ppf err

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          let loc =
            match err with
            | Missing_module (loc, _, _)
            | Illegal_value_name (loc, _)
            | Lookup_error(loc, _, _) -> loc
          in
          let error_of_printer =
            if loc = Location.none
            then Location.error_of_printer_file
            else Location.error_of_printer ~loc ?sub:None
          in
          Some (error_of_printer report_error err)
      | _ ->
          None
    )
