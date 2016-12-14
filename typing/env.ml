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
open Config
open Misc
open Asttypes
open Longident
open Path
open Types
open Btype

let add_delayed_check_forward = ref (fun _ -> assert false)

let value_declarations : ((string * Location.t), (unit -> unit)) Hashtbl.t =
  Hashtbl.create 16
    (* This table is used to usage of value declarations.  A declaration is
       identified with its name and location.  The callback attached to a
       declaration is called whenever the value is used explicitly
       (lookup_value) or implicitly (inclusion test between signatures,
       cf Includemod.value_descriptions). *)

let type_declarations = Hashtbl.create 16
let module_declarations = Hashtbl.create 16

type constructor_usage = Positive | Pattern | Privatize
type constructor_usages =
    {
     mutable cu_positive: bool;
     mutable cu_pattern: bool;
     mutable cu_privatize: bool;
    }
let add_constructor_usage cu = function
  | Positive -> cu.cu_positive <- true
  | Pattern -> cu.cu_pattern <- true
  | Privatize -> cu.cu_privatize <- true
let constructor_usages () =
  {cu_positive = false; cu_pattern = false; cu_privatize = false}

let used_constructors :
    (string * Location.t * string, (constructor_usage -> unit)) Hashtbl.t
  = Hashtbl.create 16

let prefixed_sg = Hashtbl.create 113

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string
  | Depend_on_unsafe_string_unit of string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string

exception Error of error

let error err = raise (Error err)

module EnvLazy : sig
  type ('a,'b) t

  val force : ('a -> 'b) -> ('a,'b) t -> 'b
  val create : 'a -> ('a,'b) t
  val get_arg : ('a,'b) t -> 'a option

end  = struct

  type ('a,'b) t = ('a,'b) eval ref

  and ('a,'b) eval =
      Done of 'b
    | Raise of exn
    | Thunk of 'a

  let force f x =
    match !x with
        Done x -> x
      | Raise e -> raise e
      | Thunk e ->
          try
            let y = f e in
            x := Done y;
            y
          with e ->
            x := Raise e;
            raise e

  let get_arg x =
    match !x with Thunk a -> Some a | _ -> None

  let create x =
    ref (Thunk x)

end

module PathMap = Map.Make(Path)

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration PathMap.t

module EnvTbl =
  struct
    (* A table indexed by identifier, with an extra slot to record usage. *)
    type 'a t = ('a * (unit -> unit)) Ident.tbl

    let empty = Ident.empty
    let nothing = fun () -> ()

    let already_defined wrap s tbl x =
      wrap (try Some (fst (Ident.find_name s tbl), x) with Not_found -> None)

    let add slot wrap id x tbl ref_tbl =
      let slot =
        match slot with
        | None -> nothing
        | Some f ->
          (fun () ->
             let s = Ident.name id in
             f s (already_defined wrap s ref_tbl x)
          )
      in
      Ident.add id (x, slot) tbl

    let find_same_not_using id tbl =
      fst (Ident.find_same id tbl)

    let find_same id tbl =
      let (x, slot) = Ident.find_same id tbl in
      slot ();
      x

    let find_name s tbl =
      let (x, slot) = Ident.find_name s tbl in
      slot ();
      x

    let find_all s tbl =
      Ident.find_all s tbl

    let fold_name f = Ident.fold_name (fun k (d,_) -> f k d)
    let keys tbl = Ident.fold_all (fun k _ accu -> k::accu) tbl []
  end

type type_descriptions =
    constructor_description list * label_description list

let in_signature_flag = 0x01
let implicit_coercion_flag = 0x02

type t = {
  values: (Path.t * value_description) EnvTbl.t;
  constrs: constructor_description EnvTbl.t;
  labels: label_description EnvTbl.t;
  types: (Path.t * (type_declaration * type_descriptions)) EnvTbl.t;
  modules: (Path.t * module_declaration) EnvTbl.t;
  modtypes: (Path.t * modtype_declaration) EnvTbl.t;
  components: (Path.t * module_components) EnvTbl.t;
  classes: (Path.t * class_declaration) EnvTbl.t;
  cltypes: (Path.t * class_type_declaration) EnvTbl.t;
  functor_args: unit Ident.tbl;
  summary: summary;
  local_constraints: type_declaration PathMap.t;
  gadt_instances: (int * TypeSet.t ref) list;
  flags: int;
}

and module_components =
  {
    deprecated: string option;
    loc: Location.t;
    comps: (t * Subst.t * Path.t * Types.module_type, module_components_repr)
           EnvLazy.t;
  }

and module_components_repr =
    Structure_comps of structure_components
  | Functor_comps of functor_components

and structure_components = {
  mutable comp_values: (string, (value_description * int)) Tbl.t;
  mutable comp_constrs: (string, (constructor_description * int) list) Tbl.t;
  mutable comp_labels: (string, (label_description * int) list) Tbl.t;
  mutable comp_types:
   (string, ((type_declaration * type_descriptions) * int)) Tbl.t;
  mutable comp_modules:
   (string, ((Subst.t * Types.module_type,module_type) EnvLazy.t * int)) Tbl.t;
  mutable comp_modtypes: (string, (modtype_declaration * int)) Tbl.t;
  mutable comp_components: (string, (module_components * int)) Tbl.t;
  mutable comp_classes: (string, (class_declaration * int)) Tbl.t;
  mutable comp_cltypes: (string, (class_type_declaration * int)) Tbl.t
}

and functor_components = {
  fcomp_param: Ident.t;                 (* Formal parameter *)
  fcomp_arg: module_type option;        (* Argument signature *)
  fcomp_res: module_type;               (* Result signature *)
  fcomp_cache: (Path.t, module_components) Hashtbl.t;  (* For memoization *)
  fcomp_subst_cache: (Path.t, module_type) Hashtbl.t
}

let copy_local ~from env =
  { env with
    local_constraints = from.local_constraints;
    gadt_instances = from.gadt_instances;
    flags = from.flags }

let same_constr = ref (fun _ _ _ -> assert false)

(* Helper to decide whether to report an identifier shadowing
   by some 'open'. For labels and constructors, we do not report
   if the two elements are from the same re-exported declaration.

   Later, one could also interpret some attributes on value and
   type declarations to silence the shadowing warnings. *)

let check_shadowing env = function
  | `Constructor (Some (c1, c2))
    when not (!same_constr env c1.cstr_res c2.cstr_res) ->
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

let subst_modtype_maker (subst, mty) = Subst.modtype subst mty

let empty = {
  values = EnvTbl.empty; constrs = EnvTbl.empty;
  labels = EnvTbl.empty; types = EnvTbl.empty;
  modules = EnvTbl.empty; modtypes = EnvTbl.empty;
  components = EnvTbl.empty; classes = EnvTbl.empty;
  cltypes = EnvTbl.empty;
  summary = Env_empty; local_constraints = PathMap.empty; gadt_instances = [];
  flags = 0;
  functor_args = Ident.empty;
 }

let in_signature b env =
  let flags =
    if b then env.flags lor in_signature_flag
    else env.flags land (lnot in_signature_flag)
  in
  {env with flags}

let implicit_coercion env =
  {env with flags = env.flags lor implicit_coercion_flag}

let is_in_signature env = env.flags land in_signature_flag <> 0
let is_implicit_coercion env = env.flags land implicit_coercion_flag <> 0

let diff_keys is_local tbl1 tbl2 =
  let keys2 = EnvTbl.keys tbl2 in
  List.filter
    (fun id ->
      is_local (EnvTbl.find_same_not_using id tbl2) &&
      try ignore (EnvTbl.find_same_not_using id tbl1); false
      with Not_found -> true)
    keys2

let is_ident = function
    Pident _ -> true
  | Pdot _ | Papply _ -> false

let is_local (p, _) = is_ident p

let is_local_ext = function
  | {cstr_tag = Cstr_extension(p, _)} -> is_ident p
  | _ -> false

let diff env1 env2 =
  diff_keys is_local env1.values env2.values @
  diff_keys is_local_ext env1.constrs env2.constrs @
  diff_keys is_local env1.modules env2.modules @
  diff_keys is_local env1.classes env2.classes

(* Forward declarations *)

let components_of_module' =
  ref ((fun ~deprecated:_ ~loc:_ _env _sub _path _mty -> assert false) :
         deprecated:string option -> loc:Location.t -> t -> Subst.t ->
       Path.t -> module_type ->
       module_components)
let components_of_module_maker' =
  ref ((fun (_env, _sub, _path, _mty) -> assert false) :
          t * Subst.t * Path.t * module_type -> module_components_repr)
let components_of_functor_appl' =
  ref ((fun _f _env _p1 _p2 -> assert false) :
          functor_components -> t -> Path.t -> Path.t -> module_components)
let check_modtype_inclusion =
  (* to be filled with Includemod.check_modtype_inclusion *)
  ref ((fun _env _mty1 _path1 _mty2 -> assert false) :
          t -> module_type -> Path.t -> module_type -> unit)
let strengthen =
  (* to be filled with Mtype.strengthen *)
  ref ((fun ~aliasable:_ _env _mty _path -> assert false) :
         aliasable:bool -> t -> module_type -> Path.t -> module_type)

let md md_type =
  {md_type; md_attributes=[]; md_loc=Location.none}

let get_components c =
  EnvLazy.force !components_of_module_maker' c.comps


(* The name of the compilation unit currently compiled.
   "" if outside a compilation unit. *)

let current_unit = ref ""

(* Persistent structure descriptions *)

type pers_struct =
  { ps_name: string;
    ps_sig: signature Lazy.t;
    ps_comps: module_components;
    ps_crcs: (string * Digest.t option) list;
    ps_filename: string;
    ps_flags: pers_flags list }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct option) Hashtbl.t)

(* Consistency between persistent structures *)

let crc_units = Consistbl.create()

module StringSet =
  Set.Make(struct type t = string let compare = String.compare end)

let imported_units = ref StringSet.empty

let add_import s =
  imported_units := StringSet.add s !imported_units

let imported_opaque_units = ref StringSet.empty

let add_imported_opaque s =
  imported_opaque_units := StringSet.add s !imported_opaque_units

let clear_imports () =
  Consistbl.clear crc_units;
  imported_units := StringSet.empty;
  imported_opaque_units := StringSet.empty

let check_consistency ps =
  try
    List.iter
      (fun (name, crco) ->
         match crco with
            None -> ()
          | Some crc ->
              add_import name;
              Consistbl.check crc_units name crc ps.ps_filename)
      ps.ps_crcs;
  with Consistbl.Inconsistency(name, source, auth) ->
    error (Inconsistent_import(name, auth, source))

(* Reading persistent structures from .cmi files *)

let save_pers_struct crc ps =
  let modname = ps.ps_name in
  Hashtbl.add persistent_structures modname (Some ps);
  List.iter
    (function
        | Rectypes -> ()
        | Deprecated _ -> ()
        | Unsafe_string -> ()
        | Opaque -> add_imported_opaque modname)
    ps.ps_flags;
  Consistbl.set crc_units modname crc ps.ps_filename;
  add_import modname

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos }

  let load = ref (fun ~unit_name ->
    match find_in_path_uncap !load_path (unit_name ^ ".cmi") with
    | filename -> Some { filename; cmi = read_cmi filename }
    | exception Not_found -> None)
end

let acknowledge_pers_struct check modname
      { Persistent_signature.filename; cmi } =
  let name = cmi.cmi_name in
  let sign = cmi.cmi_sign in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let deprecated =
    List.fold_left (fun acc -> function Deprecated s -> Some s | _ -> acc) None
      flags
  in
  let comps =
      !components_of_module' ~deprecated ~loc:Location.none
        empty Subst.identity
                             (Pident(Ident.create_persistent name))
                             (Mty_signature sign)
  in
  let ps = { ps_name = name;
             ps_sig = lazy (Subst.signature Subst.identity sign);
             ps_comps = comps;
             ps_crcs = crcs;
             ps_filename = filename;
             ps_flags = flags;
           } in
  if ps.ps_name <> modname then
    error (Illegal_renaming(modname, ps.ps_name, filename));

  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(ps.ps_name, !current_unit))
        | Unsafe_string ->
            if Config.safe_string then
              error (Depend_on_unsafe_string_unit (ps.ps_name, !current_unit));
        | Deprecated _ -> ()
        | Opaque -> add_imported_opaque modname)
    ps.ps_flags;
  if check then check_consistency ps;
  Hashtbl.add persistent_structures modname (Some ps);
  ps

let read_pers_struct check modname filename =
  add_import modname;
  let cmi = read_cmi filename in
  acknowledge_pers_struct check modname
    { Persistent_signature.filename; cmi }

let can_load_cmis = ref true
let without_cmis f x =
  if !can_load_cmis then begin
    can_load_cmis := false;
    try let r = f x in can_load_cmis := true; r
    with e -> can_load_cmis := true; raise e
  end else f x

let find_pers_struct check name =
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Some ps -> ps
  | None -> raise Not_found
  | exception Not_found when !can_load_cmis ->
      let ps =
        match !Persistent_signature.load ~unit_name:name with
        | Some ps -> ps
        | None ->
          Hashtbl.add persistent_structures name None;
          raise Not_found
      in
      add_import name;
      acknowledge_pers_struct check name ps

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct name =
  try
    ignore (find_pers_struct false name)
  with
  | Not_found ->
      let warn = Warnings.No_cmi_file(name, None) in
        Location.prerr_warning Location.none warn
  | Cmi_format.Error err ->
      let msg = Format.asprintf "%a" Cmi_format.report_error err in
      let warn = Warnings.No_cmi_file(name, Some msg) in
        Location.prerr_warning Location.none warn
  | Error err ->
      let msg =
        match err with
        | Illegal_renaming(name, ps_name, filename) ->
            Format.asprintf
              " %a@ contains the compiled interface for @ \
               %s when %s was expected"
              Location.print_filename filename ps_name name
        | Inconsistent_import _ -> assert false
        | Need_recursive_types(name, _) ->
            Format.sprintf
              "%s uses recursive types"
              name
        | Depend_on_unsafe_string_unit (name, _) ->
            Printf.sprintf "%s uses -unsafe-string"
              name
        | Missing_module _ -> assert false
        | Illegal_value_name _ -> assert false
      in
      let warn = Warnings.No_cmi_file(name, Some msg) in
        Location.prerr_warning Location.none warn

let read_pers_struct modname filename =
  read_pers_struct true modname filename

let find_pers_struct name =
  find_pers_struct true name

let check_pers_struct name =
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check suceeds, to help make builds more
       deterministic. *)
    add_import name;
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct name)
  end

let reset_cache () =
  current_unit := "";
  Hashtbl.clear persistent_structures;
  clear_imports ();
  Hashtbl.clear value_declarations;
  Hashtbl.clear type_declarations;
  Hashtbl.clear module_declarations;
  Hashtbl.clear used_constructors;
  Hashtbl.clear prefixed_sg

let reset_cache_toplevel () =
  (* Delete 'missing cmi' entries from the cache. *)
  let l =
    Hashtbl.fold
      (fun name r acc -> if r = None then name :: acc else acc)
      persistent_structures []
  in
  List.iter (Hashtbl.remove persistent_structures) l;
  Hashtbl.clear value_declarations;
  Hashtbl.clear type_declarations;
  Hashtbl.clear module_declarations;
  Hashtbl.clear used_constructors;
  Hashtbl.clear prefixed_sg


let set_unit_name name =
  current_unit := name

let get_unit_name () =
  !current_unit

(* Lookup by identifier *)

let rec find_module_descr path env =
  match path with
    Pident id ->
      begin try
        let (_p, desc) = EnvTbl.find_same id env.components
        in desc
      with Not_found ->
        if Ident.persistent id && not (Ident.name id = !current_unit)
        then (find_pers_struct (Ident.name id)).ps_comps
        else raise Not_found
      end
  | Pdot(p, s, _pos) ->
      begin match get_components (find_module_descr p env) with
        Structure_comps c ->
          let (descr, _pos) = Tbl.find s c.comp_components in
          descr
      | Functor_comps _ ->
         raise Not_found
      end
  | Papply(p1, p2) ->
      begin match get_components (find_module_descr p1 env) with
        Functor_comps f ->
          !components_of_functor_appl' f env p1 p2
      | Structure_comps _ ->
          raise Not_found
      end

let find proj1 proj2 path env =
  match path with
    Pident id ->
      let (_p, data) = EnvTbl.find_same id (proj1 env)
      in data
  | Pdot(p, s, _pos) ->
      begin match get_components (find_module_descr p env) with
        Structure_comps c ->
          let (data, _pos) = Tbl.find s (proj2 c) in data
      | Functor_comps _ ->
          raise Not_found
      end
  | Papply _ ->
      raise Not_found

let find_value =
  find (fun env -> env.values) (fun sc -> sc.comp_values)
and find_type_full =
  find (fun env -> env.types) (fun sc -> sc.comp_types)
and find_modtype =
  find (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)
and find_class =
  find (fun env -> env.classes) (fun sc -> sc.comp_classes)
and find_cltype =
  find (fun env -> env.cltypes) (fun sc -> sc.comp_cltypes)

let type_of_cstr path = function
  | {cstr_inlined = Some d; _} ->
      (d, ([], List.map snd (Datarepr.labels_of_type path d)))
  | _ ->
      assert false

let find_type_full path env =
  match Path.constructor_typath path with
  | Regular p ->
      (try (PathMap.find p env.local_constraints, ([], []))
       with Not_found -> find_type_full p env)
  | Cstr (ty_path, s) ->
      let (_, (cstrs, _)) =
        try find_type_full ty_path env
        with Not_found -> assert false
      in
      let cstr =
        try List.find (fun cstr -> cstr.cstr_name = s) cstrs
        with Not_found -> assert false
      in
      type_of_cstr path cstr
  | LocalExt id ->
      let cstr =
        try EnvTbl.find_same id env.constrs
        with Not_found -> assert false
      in
      type_of_cstr path cstr
  | Ext (mod_path, s) ->
      let comps =
        try find_module_descr mod_path env
        with Not_found -> assert false
      in
      let comps =
        match get_components comps with
        | Structure_comps c -> c
        | Functor_comps _ -> assert false
      in
      let exts =
        List.filter
          (function ({cstr_tag=Cstr_extension _}, _) -> true | _ -> false)
          (try Tbl.find s comps.comp_constrs
           with Not_found -> assert false)
      in
      match exts with
      | [(cstr, _)] -> type_of_cstr path cstr
      | _ -> assert false

let find_type p env =
  fst (find_type_full p env)
let find_type_descrs p env =
  snd (find_type_full p env)

let find_module ~alias path env =
  match path with
    Pident id ->
      begin try
        let (_p, data) = EnvTbl.find_same id env.modules
        in data
      with Not_found ->
        if Ident.persistent id && not (Ident.name id = !current_unit) then
          let ps = find_pers_struct (Ident.name id) in
          md (Mty_signature(Lazy.force ps.ps_sig))
        else raise Not_found
      end
  | Pdot(p, s, _pos) ->
      begin match get_components (find_module_descr p env) with
        Structure_comps c ->
          let (data, _pos) = Tbl.find s c.comp_modules in
          md (EnvLazy.force subst_modtype_maker data)
      | Functor_comps _ ->
          raise Not_found
      end
  | Papply(p1, p2) ->
      let desc1 = find_module_descr p1 env in
      begin match get_components desc1 with
        Functor_comps f ->
          md begin match f.fcomp_res with
          | Mty_alias _ as mty -> mty
          | mty ->
              if alias then mty else
              try
                Hashtbl.find f.fcomp_subst_cache p2
              with Not_found ->
                let mty =
                  Subst.modtype
                    (Subst.add_module f.fcomp_param p2 Subst.identity)
                    f.fcomp_res in
                Hashtbl.add f.fcomp_subst_cache p2 mty;
                mty
          end
      | Structure_comps _ ->
          raise Not_found
      end

let required_globals = ref []
let reset_required_globals () = required_globals := []
let get_required_globals () = !required_globals
let add_required_global id =
  if Ident.global id && not !Clflags.transparent_modules
  && not (List.exists (Ident.same id) !required_globals)
  then required_globals := id :: !required_globals

let rec normalize_path lax env path =
  let path =
    match path with
      Pdot(p, s, pos) ->
        Pdot(normalize_path lax env p, s, pos)
    | Papply(p1, p2) ->
        Papply(normalize_path lax env p1, normalize_path true env p2)
    | _ -> path
  in
  try match find_module ~alias:true path env with
    {md_type=Mty_alias(_, path1)} ->
      let path' = normalize_path lax env path1 in
      if lax || !Clflags.transparent_modules then path' else
      let id = Path.head path in
      if Ident.global id && not (Ident.same id (Path.head path'))
      then add_required_global id;
      path'
  | _ -> path
  with Not_found when lax
  || (match path with Pident id -> not (Ident.persistent id) | _ -> true) ->
      path

let normalize_path oloc env path =
  try normalize_path (oloc = None) env path
  with Not_found ->
    match oloc with None -> assert false
    | Some loc ->
        raise (Error(Missing_module(loc, path, normalize_path true env path)))

let normalize_path_prefix oloc env path =
  match path with
    Pdot(p, s, pos) ->
      Pdot(normalize_path oloc env p, s, pos)
  | Pident _ ->
      path
  | Papply _ ->
      assert false


let find_module = find_module ~alias:false

(* Find the manifest type associated to a type when appropriate:
   - the type should be public or should have a private row,
   - the type should have an associated manifest type. *)
let find_type_expansion path env =
  let decl = find_type path env in
  match decl.type_manifest with
  | Some body when decl.type_private = Public
              || decl.type_kind <> Type_abstract
              || Btype.has_constr_row body ->
                  (decl.type_params, body, may_map snd decl.type_newtype_level)
  (* The manifest type of Private abstract data types without
     private row are still considered unknown to the type system.
     Hence, this case is caught by the following clause that also handles
     purely abstract data types without manifest type definition. *)
  | _ ->
      (* another way to expand is to normalize the path itself *)
      let path' = normalize_path None env path in
      if Path.same path path' then raise Not_found else
      (decl.type_params,
       newgenty (Tconstr (path', decl.type_params, ref Mnil)),
       may_map snd decl.type_newtype_level)

(* Find the manifest type information associated to a type, i.e.
   the necessary information for the compiler's type-based optimisations.
   In particular, the manifest type associated to a private abstract type
   is revealed for the sake of compiler's type-based optimisations. *)
let find_type_expansion_opt path env =
  let decl = find_type path env in
  match decl.type_manifest with
  (* The manifest type of Private abstract data types can still get
     an approximation using their manifest type. *)
  | Some body -> (decl.type_params, body, may_map snd decl.type_newtype_level)
  | _ ->
      let path' = normalize_path None env path in
      if Path.same path path' then raise Not_found else
      (decl.type_params,
       newgenty (Tconstr (path', decl.type_params, ref Mnil)),
       may_map snd decl.type_newtype_level)

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
  | Pdot (p, _s, _) -> is_functor_arg p env
  | Papply _ -> true

(* Lookup by name *)

exception Recmodule

let report_deprecated ?loc p deprecated =
  match loc, deprecated with
  | Some loc, Some txt ->
      let txt = if txt = "" then "" else "\n" ^ txt in
      Location.prerr_warning loc
        (Warnings.Deprecated (Printf.sprintf "module %s%s"
                                (Path.name p) txt))
  | _ -> ()

let mark_module_used env name loc =
  if not (is_implicit_coercion env) then
    try Hashtbl.find module_declarations (name, loc) ()
    with Not_found -> ()

let rec lookup_module_descr_aux ?loc lid env =
  match lid with
    Lident s ->
      begin try
        EnvTbl.find_name s env.components
      with Not_found ->
        if s = !current_unit then raise Not_found;
        let ps = find_pers_struct s in
        (Pident(Ident.create_persistent s), ps.ps_comps)
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr ?loc l env in
      begin match get_components descr with
        Structure_comps c ->
          let (descr, pos) = Tbl.find s c.comp_components in
          (Pdot(p, s, pos), descr)
      | Functor_comps _ ->
          raise Not_found
      end
  | Lapply(l1, l2) ->
      let (p1, desc1) = lookup_module_descr ?loc l1 env in
      let p2 = lookup_module ~load:true ?loc l2 env in
      let {md_type=mty2} = find_module p2 env in
      begin match get_components desc1 with
        Functor_comps f ->
          Misc.may (!check_modtype_inclusion env mty2 p2) f.fcomp_arg;
          (Papply(p1, p2), !components_of_functor_appl' f env p1 p2)
      | Structure_comps _ ->
          raise Not_found
      end

and lookup_module_descr ?loc lid env =
  let (p, comps) as res = lookup_module_descr_aux ?loc lid env in
  mark_module_used env (Path.last p) comps.loc;
(*
  Format.printf "USE module %s at %a@." (Path.last p)
    Location.print comps.loc;
*)
  report_deprecated ?loc p comps.deprecated;
  res

and lookup_module ~load ?loc lid env : Path.t =
  match lid with
    Lident s ->
      begin try
        let (p, {md_type; md_attributes; md_loc}) =
          EnvTbl.find_name s env.modules
        in
        mark_module_used env s md_loc;
        begin match md_type with
        | Mty_ident (Path.Pident id) when Ident.name id = "#recmod#" ->
          (* see #5965 *)
          raise Recmodule
        | _ -> ()
        end;
        report_deprecated ?loc p
          (Builtin_attributes.deprecated_of_attrs md_attributes);
        p
      with Not_found ->
        if s = !current_unit then raise Not_found;
        let p = Pident(Ident.create_persistent s) in
        if !Clflags.transparent_modules && not load then check_pers_struct s
        else begin
          let ps = find_pers_struct s in
          report_deprecated ?loc p ps.ps_comps.deprecated
        end;
        p
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr ?loc l env in
      begin match get_components descr with
        Structure_comps c ->
          let (_data, pos) = Tbl.find s c.comp_modules in
          let (comps, _) = Tbl.find s c.comp_components in
          mark_module_used env s comps.loc;
          let p = Pdot(p, s, pos) in
          report_deprecated ?loc p comps.deprecated;
          p
      | Functor_comps _ ->
          raise Not_found
      end
  | Lapply(l1, l2) ->
      let (p1, desc1) = lookup_module_descr ?loc l1 env in
      let p2 = lookup_module ~load:true ?loc l2 env in
      let {md_type=mty2} = find_module p2 env in
      let p = Papply(p1, p2) in
      begin match get_components desc1 with
        Functor_comps f ->
          Misc.may (!check_modtype_inclusion env mty2 p2) f.fcomp_arg;
          p
      | Structure_comps _ ->
          raise Not_found
      end

let lookup proj1 proj2 ?loc lid env =
  match lid with
    Lident s ->
      EnvTbl.find_name s (proj1 env)
  | Ldot(l, s) ->
      let (p, desc) = lookup_module_descr ?loc l env in
      begin match get_components desc with
        Structure_comps c ->
          let (data, pos) = Tbl.find s (proj2 c) in
          (Pdot(p, s, pos), data)
      | Functor_comps _ ->
          raise Not_found
      end
  | Lapply _ ->
      raise Not_found

let lookup_all_simple proj1 proj2 shadow ?loc lid env =
  match lid with
    Lident s ->
      let xl = EnvTbl.find_all s (proj1 env) in
      let rec do_shadow =
        function
        | [] -> []
        | ((x, f) :: xs) ->
            (x, f) ::
              (do_shadow (List.filter (fun (y, _) -> not (shadow x y)) xs))
      in
        do_shadow xl
  | Ldot(l, s) ->
      let (_p, desc) = lookup_module_descr ?loc l env in
      begin match get_components desc with
        Structure_comps c ->
          let comps =
            try Tbl.find s (proj2 c) with Not_found -> []
          in
          List.map
            (fun (data, _pos) -> (data, (fun () -> ())))
            comps
      | Functor_comps _ ->
          raise Not_found
      end
  | Lapply _ ->
      raise Not_found

let has_local_constraints env = not (PathMap.is_empty env.local_constraints)

let cstr_shadow cstr1 cstr2 =
  match cstr1.cstr_tag, cstr2.cstr_tag with
  | Cstr_extension _, Cstr_extension _ -> true
  | _ -> false

let lbl_shadow _lbl1 _lbl2 = false

let lookup_value =
  lookup (fun env -> env.values) (fun sc -> sc.comp_values)
and lookup_all_constructors =
  lookup_all_simple (fun env -> env.constrs) (fun sc -> sc.comp_constrs)
    cstr_shadow
and lookup_all_labels =
  lookup_all_simple (fun env -> env.labels) (fun sc -> sc.comp_labels)
    lbl_shadow
and lookup_type =
  lookup (fun env -> env.types) (fun sc -> sc.comp_types)
and lookup_modtype =
  lookup (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)
and lookup_class =
  lookup (fun env -> env.classes) (fun sc -> sc.comp_classes)
and lookup_cltype =
  lookup (fun env -> env.cltypes) (fun sc -> sc.comp_cltypes)

let update_value s f env =
  try
    let ((p, vd), slot) = Ident.find_name s env.values in
    match p with
    | Pident id ->
        let vd2 = f vd in
        {env with values = Ident.add id ((p, vd2), slot) env.values;
                  summary = Env_value(env.summary, id, vd2)}
    | _ ->
        env
  with Not_found ->
    env

let mark_value_used env name vd =
  if not (is_implicit_coercion env) then
    try Hashtbl.find value_declarations (name, vd.val_loc) ()
    with Not_found -> ()

let mark_type_used env name vd =
  if not (is_implicit_coercion env) then
    try Hashtbl.find type_declarations (name, vd.type_loc) ()
    with Not_found -> ()

let mark_constructor_used usage env name vd constr =
  if not (is_implicit_coercion env) then
    try Hashtbl.find used_constructors (name, vd.type_loc, constr) usage
    with Not_found -> ()

let mark_extension_used usage env ext name =
  if not (is_implicit_coercion env) then
    let ty_name = Path.last ext.ext_type_path in
    try Hashtbl.find used_constructors (ty_name, ext.ext_loc, name) usage
    with Not_found -> ()

let set_value_used_callback name vd callback =
  let key = (name, vd.val_loc) in
  try
    let old = Hashtbl.find value_declarations key in
    Hashtbl.replace value_declarations key (fun () -> old (); callback ())
      (* this is to support cases like:
               let x = let x = 1 in x in x
         where the two declarations have the same location
         (e.g. resulting from Camlp4 expansion of grammar entries) *)
  with Not_found ->
    Hashtbl.add value_declarations key callback

let set_type_used_callback name td callback =
  let loc = td.type_loc in
  if loc.Location.loc_ghost then ()
  else let key = (name, loc) in
  let old =
    try Hashtbl.find type_declarations key
    with Not_found -> assert false
  in
  Hashtbl.replace type_declarations key (fun () -> callback old)

let lookup_value ?loc lid env =
  let (_, desc) as r = lookup_value ?loc lid env in
  mark_value_used env (Longident.last lid) desc;
  r

let lookup_type ?loc lid env =
  let (path, (decl, _)) = lookup_type ?loc lid env in
  mark_type_used env (Longident.last lid) decl;
  path

let mark_type_path env path =
  try
    let decl = find_type path env in
    mark_type_used env (Path.last path) decl
  with Not_found -> ()

let ty_path t =
  match repr t with
  | {desc=Tconstr(path, _, _)} -> path
  | _ -> assert false

let lookup_constructor ?loc lid env =
  match lookup_all_constructors ?loc lid env with
    [] -> raise Not_found
  | (desc, use) :: _ ->
      mark_type_path env (ty_path desc.cstr_res);
      use ();
      desc

let is_lident = function
    Lident _ -> true
  | _ -> false

let lookup_all_constructors ?loc lid env =
  try
    let cstrs = lookup_all_constructors ?loc lid env in
    let wrap_use desc use () =
      mark_type_path env (ty_path desc.cstr_res);
      use ()
    in
    List.map (fun (cstr, use) -> (cstr, wrap_use cstr use)) cstrs
  with
    Not_found when is_lident lid -> []

let mark_constructor usage env name desc =
  if not (is_implicit_coercion env)
  then match desc.cstr_tag with
  | Cstr_extension _ ->
      begin
        let ty_path = ty_path desc.cstr_res in
        let ty_name = Path.last ty_path in
        try Hashtbl.find used_constructors (ty_name, desc.cstr_loc, name) usage
        with Not_found -> ()
      end
  | _ ->
      let ty_path = ty_path desc.cstr_res in
      let ty_decl = try find_type ty_path env with Not_found -> assert false in
      let ty_name = Path.last ty_path in
      mark_constructor_used usage env ty_name ty_decl name

let lookup_label ?loc lid env =
  match lookup_all_labels ?loc lid env with
    [] -> raise Not_found
  | (desc, use) :: _ ->
      mark_type_path env (ty_path desc.lbl_res);
      use ();
      desc

let lookup_all_labels ?loc lid env =
  try
    let lbls = lookup_all_labels ?loc lid env in
    let wrap_use desc use () =
      mark_type_path env (ty_path desc.lbl_res);
      use ()
    in
    List.map (fun (lbl, use) -> (lbl, wrap_use lbl use)) lbls
  with
    Not_found when is_lident lid -> []

let lookup_class ?loc lid env =
  let (_, desc) as r = lookup_class ?loc lid env in
  (* special support for Typeclass.unbound_class *)
  if Path.name desc.cty_path = "" then ignore (lookup_type ?loc lid env)
  else mark_type_path env desc.cty_path;
  r

let lookup_cltype ?loc lid env =
  let (_, desc) as r = lookup_cltype ?loc lid env in
  if Path.name desc.clty_path = "" then ignore (lookup_type ?loc lid env)
  else mark_type_path env desc.clty_path;
  mark_type_path env desc.clty_path;
  r

(* Iter on an environment (ignoring the body of functors and
   not yet evaluated structures) *)

type iter_cont = unit -> unit
let iter_env_cont = ref []

let rec scrape_alias_for_visit env mty =
  match mty with
  | Mty_alias(_, Pident id)
    when Ident.persistent id
      && not (Hashtbl.mem persistent_structures (Ident.name id)) -> false
  | Mty_alias(_, path) -> (* PR#6600: find_module may raise Not_found *)
      begin try scrape_alias_for_visit env (find_module path env).md_type
      with Not_found -> false
      end
  | _ -> true

let iter_env proj1 proj2 f env () =
  Ident.iter (fun id (x,_) -> f (Pident id) x) (proj1 env);
  let rec iter_components path path' mcomps =
    let cont () =
      let visit =
        match EnvLazy.get_arg mcomps.comps with
        | None -> true
        | Some (env, _sub, _path, mty) -> scrape_alias_for_visit env mty
      in
      if not visit then () else
      match get_components mcomps with
        Structure_comps comps ->
          Tbl.iter
            (fun s (d, n) -> f (Pdot (path, s, n)) (Pdot (path', s, n), d))
            (proj2 comps);
          Tbl.iter
            (fun s (c, n) ->
              iter_components (Pdot (path, s, n)) (Pdot (path', s, n)) c)
            comps.comp_components
      | Functor_comps _ -> ()
    in iter_env_cont := (path, cont) :: !iter_env_cont
  in
  Hashtbl.iter
    (fun s pso ->
      match pso with None -> ()
      | Some ps ->
          let id = Pident (Ident.create_persistent s) in
          iter_components id id ps.ps_comps)
    persistent_structures;
  Ident.iter
    (fun id ((path, comps), _) -> iter_components (Pident id) path comps)
    env.components

let run_iter_cont l =
  iter_env_cont := [];
  List.iter (fun c -> c ()) l;
  let cont = List.rev !iter_env_cont in
  iter_env_cont := [];
  cont

let iter_types f = iter_env (fun env -> env.types) (fun sc -> sc.comp_types) f

let same_types env1 env2 =
  env1.types == env2.types && env1.components == env2.components

let used_persistent () =
  let r = ref Concr.empty in
  Hashtbl.iter (fun s pso -> if pso != None then r := Concr.add s !r)
    persistent_structures;
  !r

let find_all_comps proj s (p,mcomps) =
  match get_components mcomps with
    Functor_comps _ -> []
  | Structure_comps comps ->
      try let (c,n) = Tbl.find s (proj comps) in [Pdot(p,s,n), c]
      with Not_found -> []

let rec find_shadowed_comps path env =
  match path with
    Pident id ->
      List.map fst (Ident.find_all (Ident.name id) env.components)
  | Pdot (p, s, _) ->
      let l = find_shadowed_comps p env in
      let l' =
        List.map (find_all_comps (fun comps -> comps.comp_components) s) l in
      List.flatten l'
  | Papply _ -> []

let find_shadowed proj1 proj2 path env =
  match path with
    Pident id ->
      List.map fst (Ident.find_all (Ident.name id) (proj1 env))
  | Pdot (p, s, _) ->
      let l = find_shadowed_comps p env in
      let l' = List.map (find_all_comps proj2 s) l in
      List.flatten l'
  | Papply _ -> []

let find_shadowed_types path env =
  let l =
    find_shadowed
      (fun env -> env.types) (fun comps -> comps.comp_types) path env
  in
  List.map fst l


(* GADT instance tracking *)

let add_gadt_instance_level lv env =
  {env with
   gadt_instances = (lv, ref TypeSet.empty) :: env.gadt_instances}

let is_Tlink = function {desc = Tlink _} -> true | _ -> false

let gadt_instance_level env t =
  let rec find_instance = function
      [] -> None
    | (lv, r) :: rem ->
        if TypeSet.exists is_Tlink !r then
          (* Should we use set_typeset ? *)
          r := TypeSet.fold (fun ty -> TypeSet.add (repr ty)) !r TypeSet.empty;
        if TypeSet.mem t !r then Some lv else find_instance rem
  in find_instance env.gadt_instances

let add_gadt_instances env lv tl =
  let r =
    try List.assoc lv env.gadt_instances with Not_found -> assert false in
  (* Format.eprintf "Added";
  List.iter (fun ty -> Format.eprintf "@ %a" !Btype.print_raw ty) tl;
  Format.eprintf "@."; *)
  set_typeset r (List.fold_right TypeSet.add tl !r)

(* Only use this after expand_head! *)
let add_gadt_instance_chain env lv t =
  let r =
    try List.assoc lv env.gadt_instances with Not_found -> assert false in
  let rec add_instance t =
    let t = repr t in
    if not (TypeSet.mem t !r) then begin
      (* Format.eprintf "@ %a" !Btype.print_raw t; *)
      set_typeset r (TypeSet.add t !r);
      match t.desc with
        Tconstr (p, _, memo) ->
          may add_instance (find_expans Private p !memo)
      | _ -> ()
    end
  in
  (* Format.eprintf "Added chain"; *)
  add_instance t
  (* Format.eprintf "@." *)

(* Expand manifest module type names at the top of the given module type *)

let rec scrape_alias env ?path mty =
  match mty, path with
    Mty_ident p, _ ->
      begin try
        scrape_alias env (find_modtype_expansion p env) ?path
      with Not_found ->
        mty
      end
  | Mty_alias(_, path), _ ->
      begin try
        scrape_alias env (find_module path env).md_type ~path
      with Not_found ->
        (*Location.prerr_warning Location.none
          (Warnings.No_cmi_file (Path.name path));*)
        mty
      end
  | mty, Some path ->
      !strengthen ~aliasable:true env mty path
  | _ -> mty

let scrape_alias env mty = scrape_alias env mty

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let rec prefix_idents root pos sub = function
    [] -> ([], sub)
  | Sig_value(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let nextpos = match decl.val_kind with Val_prim _ -> pos | _ -> pos+1 in
      let (pl, final_sub) = prefix_idents root nextpos sub rem in
      (p::pl, final_sub)
  | Sig_type(id, _, _) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | Sig_typext(id, _, _) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      (* we extend the substitution in case of an inlined record *)
      let (pl, final_sub) =
        prefix_idents root (pos+1) (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | Sig_module(id, _, _) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos+1) (Subst.add_module id p sub) rem in
      (p::pl, final_sub)
  | Sig_modtype(id, _) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos
                      (Subst.add_modtype id (Mty_ident p) sub) rem in
      (p::pl, final_sub)
  | Sig_class(id, _, _) :: rem ->
      (* pretend this is a type, cf. PR#6650 *)
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos + 1) (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | Sig_class_type(id, _, _) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos (Subst.add_type id p sub) rem in
      (p::pl, final_sub)

let subst_signature sub sg =
  List.map
    (fun item ->
      match item with
      | Sig_value(id, decl) ->
          Sig_value (id, Subst.value_description sub decl)
      | Sig_type(id, decl, x) ->
          Sig_type(id, Subst.type_declaration sub decl, x)
      | Sig_typext(id, ext, es) ->
          Sig_typext (id, Subst.extension_constructor sub ext, es)
      | Sig_module(id, mty, x) ->
          Sig_module(id, Subst.module_declaration sub mty,x)
      | Sig_modtype(id, decl) ->
          Sig_modtype(id, Subst.modtype_declaration sub decl)
      | Sig_class(id, decl, x) ->
          Sig_class(id, Subst.class_declaration sub decl, x)
      | Sig_class_type(id, decl, x) ->
          Sig_class_type(id, Subst.cltype_declaration sub decl, x)
    )
    sg


let prefix_idents_and_subst root sub sg =
  let (pl, sub) = prefix_idents root 0 sub sg in
  pl, sub, lazy (subst_signature sub sg)

let prefix_idents_and_subst root sub sg =
  if sub = Subst.identity then
    let sgs =
      try
        Hashtbl.find prefixed_sg root
      with Not_found ->
        let sgs = ref [] in
        Hashtbl.add prefixed_sg root sgs;
        sgs
    in
    try
      List.assq sg !sgs
    with Not_found ->
      let r = prefix_idents_and_subst root sub sg in
      sgs := (sg, r) :: !sgs;
      r
  else
    prefix_idents_and_subst root sub sg

(* Compute structure descriptions *)

let add_to_tbl id decl tbl =
  let decls =
    try Tbl.find id tbl with Not_found -> [] in
  Tbl.add id (decl :: decls) tbl

let rec components_of_module ~deprecated ~loc env sub path mty =
  {
    deprecated;
    loc;
    comps = EnvLazy.create (env, sub, path, mty)
  }

and components_of_module_maker (env, sub, path, mty) =
  (match scrape_alias env mty with
    Mty_signature sg ->
      let c =
        { comp_values = Tbl.empty;
          comp_constrs = Tbl.empty;
          comp_labels = Tbl.empty; comp_types = Tbl.empty;
          comp_modules = Tbl.empty; comp_modtypes = Tbl.empty;
          comp_components = Tbl.empty; comp_classes = Tbl.empty;
          comp_cltypes = Tbl.empty } in
      let pl, sub, _ = prefix_idents_and_subst path sub sg in
      let env = ref env in
      let pos = ref 0 in
      List.iter2 (fun item path ->
        match item with
          Sig_value(id, decl) ->
            let decl' = Subst.value_description sub decl in
            c.comp_values <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
            begin match decl.val_kind with
              Val_prim _ -> () | _ -> incr pos
            end
        | Sig_type(id, decl, _) ->
            let decl' = Subst.type_declaration sub decl in
            let constructors =
              List.map snd (Datarepr.constructors_of_type path decl') in
            let labels =
              List.map snd (Datarepr.labels_of_type path decl') in
            c.comp_types <-
              Tbl.add (Ident.name id)
                ((decl', (constructors, labels)), nopos)
                  c.comp_types;
            List.iter
              (fun descr ->
                c.comp_constrs <-
                  add_to_tbl descr.cstr_name (descr, nopos) c.comp_constrs)
              constructors;
            List.iter
              (fun descr ->
                c.comp_labels <-
                  add_to_tbl descr.lbl_name (descr, nopos) c.comp_labels)
              labels;
            env := store_type_infos None id (Pident id) decl !env !env
        | Sig_typext(id, ext, _) ->
            let ext' = Subst.extension_constructor sub ext in
            let descr = Datarepr.extension_descr path ext' in
            c.comp_constrs <-
              add_to_tbl (Ident.name id) (descr, !pos) c.comp_constrs;
            incr pos
        | Sig_module(id, md, _) ->
            let mty = md.md_type in
            let mty' = EnvLazy.create (sub, mty) in
            c.comp_modules <-
              Tbl.add (Ident.name id) (mty', !pos) c.comp_modules;
            let deprecated =
              Builtin_attributes.deprecated_of_attrs md.md_attributes
            in
            let comps =
              components_of_module ~deprecated ~loc:md.md_loc !env sub path mty
            in
            c.comp_components <-
              Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
            env := store_module ~check:false None id (Pident id) md !env !env;
            incr pos
        | Sig_modtype(id, decl) ->
            let decl' = Subst.modtype_declaration sub decl in
            c.comp_modtypes <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_modtypes;
            env := store_modtype None id (Pident id) decl !env !env
        | Sig_class(id, decl, _) ->
            let decl' = Subst.class_declaration sub decl in
            c.comp_classes <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_classes;
            incr pos
        | Sig_class_type(id, decl, _) ->
            let decl' = Subst.cltype_declaration sub decl in
            c.comp_cltypes <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_cltypes)
        sg pl;
        Structure_comps c
  | Mty_functor(param, ty_arg, ty_res) ->
        Functor_comps {
          fcomp_param = param;
          (* fcomp_arg and fcomp_res must be prefixed eagerly, because
             they are interpreted in the outer environment *)
          fcomp_arg = may_map (Subst.modtype sub) ty_arg;
          fcomp_res = Subst.modtype sub ty_res;
          fcomp_cache = Hashtbl.create 17;
          fcomp_subst_cache = Hashtbl.create 17 }
  | Mty_ident _
  | Mty_alias _ ->
        Structure_comps {
          comp_values = Tbl.empty;
          comp_constrs = Tbl.empty;
          comp_labels = Tbl.empty;
          comp_types = Tbl.empty;
          comp_modules = Tbl.empty; comp_modtypes = Tbl.empty;
          comp_components = Tbl.empty; comp_classes = Tbl.empty;
          comp_cltypes = Tbl.empty })

(* Insertion of bindings by identifier + path *)

and check_usage loc id warn tbl =
  if not loc.Location.loc_ghost && Warnings.is_active (warn "") then begin
    let name = Ident.name id in
    let key = (name, loc) in
    if Hashtbl.mem tbl key then ()
    else let used = ref false in
    Hashtbl.add tbl key (fun () -> used := true);
    if not (name = "" || name.[0] = '_' || name.[0] = '#')
    then
      !add_delayed_check_forward
        (fun () -> if not !used then Location.prerr_warning loc (warn name))
  end;

and check_value_name name loc =
  (* Note: we could also check here general validity of the
     identifier, to protect against bad identifiers forged by -pp or
     -ppx preprocessors. *)

  if String.length name > 0 && (name.[0] = '#') then
    for i = 1 to String.length name - 1 do
      if name.[i] = '#' then
        raise (Error(Illegal_value_name(loc, name)))
    done


and store_value ?check slot id path decl env renv =
  check_value_name (Ident.name id) decl.val_loc;
  may (fun f -> check_usage decl.val_loc id f value_declarations) check;
  { env with
    values = EnvTbl.add slot (fun x -> `Value x) id (path, decl)
        env.values renv.values;
    summary = Env_value(env.summary, id, decl) }

and store_type ~check slot id path info env renv =
  let loc = info.type_loc in
  if check then
    check_usage loc id (fun s -> Warnings.Unused_type_declaration s)
      type_declarations;
  let constructors = Datarepr.constructors_of_type path info in
  let labels = Datarepr.labels_of_type path info in
  let descrs = (List.map snd constructors, List.map snd labels) in

  if check && not loc.Location.loc_ghost &&
    Warnings.is_active (Warnings.Unused_constructor ("", false, false))
  then begin
    let ty = Ident.name id in
    List.iter
      begin fun (_, {cstr_name = c; _}) ->
        let k = (ty, loc, c) in
        if not (Hashtbl.mem used_constructors k) then
          let used = constructor_usages () in
          Hashtbl.add used_constructors k (add_constructor_usage used);
          if not (ty = "" || ty.[0] = '_')
          then !add_delayed_check_forward
              (fun () ->
                if not (is_in_signature env) && not used.cu_positive then
                  Location.prerr_warning loc
                    (Warnings.Unused_constructor
                       (c, used.cu_pattern, used.cu_privatize)))
      end
      constructors
  end;
  { env with
    constrs =
      List.fold_right
        (fun (id, descr) constrs ->
           EnvTbl.add slot (fun x -> `Constructor x) id descr constrs
             renv.constrs)
        constructors
        env.constrs;
    labels =
      List.fold_right
        (fun (id, descr) labels ->
           EnvTbl.add slot (fun x -> `Label x) id descr labels renv.labels)
        labels
        env.labels;
    types =
      EnvTbl.add slot (fun x -> `Type x) id (path, (info, descrs)) env.types
                       renv.types;
    summary = Env_type(env.summary, id, info) }

and store_type_infos slot id path info env renv =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  { env with
    types = EnvTbl.add slot (fun x -> `Type x) id (path, (info,([],[])))
        env.types renv.types;
    summary = Env_type(env.summary, id, info) }

and store_extension ~check slot id path ext env renv =
  let loc = ext.ext_loc in
  if check && not loc.Location.loc_ghost &&
    Warnings.is_active (Warnings.Unused_extension ("", false, false, false))
  then begin
    let is_exception = Path.same ext.ext_type_path Predef.path_exn in
    let ty = Path.last ext.ext_type_path in
    let n = Ident.name id in
    let k = (ty, loc, n) in
    if not (Hashtbl.mem used_constructors k) then begin
      let used = constructor_usages () in
      Hashtbl.add used_constructors k (add_constructor_usage used);
      !add_delayed_check_forward
        (fun () ->
          if not (is_in_signature env) && not used.cu_positive then
            Location.prerr_warning loc
              (Warnings.Unused_extension
                 (n, is_exception, used.cu_pattern, used.cu_privatize)
              )
        )
    end;
  end;
  { env with
    constrs = EnvTbl.add slot (fun x -> `Constructor x) id
                (Datarepr.extension_descr path ext)
                env.constrs renv.constrs;
    summary = Env_extension(env.summary, id, ext) }

and store_module ~check slot id path md env renv =
  let loc = md.md_loc in
  if check then
    check_usage loc id (fun s -> Warnings.Unused_module s)
      module_declarations;

  let deprecated = Builtin_attributes.deprecated_of_attrs md.md_attributes in
  { env with
    modules = EnvTbl.add slot (fun x -> `Module x) id (path, md)
        env.modules renv.modules;
    components =
      EnvTbl.add slot (fun x -> `Component x) id
        (path, components_of_module ~deprecated ~loc:md.md_loc
           env Subst.identity path md.md_type)
        env.components renv.components;
    summary = Env_module(env.summary, id, md) }

and store_modtype slot id path info env renv =
  { env with
    modtypes = EnvTbl.add slot (fun x -> `Module_type x) id (path, info)
        env.modtypes renv.modtypes;
    summary = Env_modtype(env.summary, id, info) }

and store_class slot id path desc env renv =
  { env with
    classes = EnvTbl.add slot (fun x -> `Class x) id (path, desc)
        env.classes renv.classes;
    summary = Env_class(env.summary, id, desc) }

and store_cltype slot id path desc env renv =
  { env with
    cltypes = EnvTbl.add slot (fun x -> `Class_type x) id (path, desc)
        env.cltypes renv.cltypes;
    summary = Env_cltype(env.summary, id, desc) }

(* Compute the components of a functor application in a path. *)

let components_of_functor_appl f env p1 p2 =
  try
    Hashtbl.find f.fcomp_cache p2
  with Not_found ->
    let p = Papply(p1, p2) in
    let sub = Subst.add_module f.fcomp_param p2 Subst.identity in
    let mty = Subst.modtype sub f.fcomp_res in
    let comps = components_of_module ~deprecated:None ~loc:Location.none
        (*???*)
        env Subst.identity p mty in
    Hashtbl.add f.fcomp_cache p2 comps;
    comps

(* Define forward functions *)

let _ =
  components_of_module' := components_of_module;
  components_of_functor_appl' := components_of_functor_appl;
  components_of_module_maker' := components_of_module_maker

(* Insertion of bindings by identifier *)

let add_functor_arg id env =
  {env with
   functor_args = Ident.add id () env.functor_args;
   summary = Env_functor_arg (env.summary, id)}

let add_value ?check id desc env =
  store_value None ?check id (Pident id) desc env env

let add_type ~check id info env =
  store_type ~check None id (Pident id) info env env

and add_extension ~check id ext env =
  store_extension ~check None id (Pident id) ext env env

and add_module_declaration ?(arg=false) ~check id md env =
  let path =
    (*match md.md_type with
      Mty_alias path -> normalize_path env path
    | _ ->*) Pident id
  in
  let env = store_module ~check None id path md env env in
  if arg then add_functor_arg id env else env

and add_modtype id info env =
  store_modtype None id (Pident id) info env env

and add_class id ty env =
  store_class None id (Pident id) ty env env

and add_cltype id ty env =
  store_cltype None id (Pident id) ty env env

let add_module ?arg id mty env =
  add_module_declaration ~check:false ?arg id (md mty) env

let add_local_type path info env =
  { env with
    local_constraints = PathMap.add path info env.local_constraints }

let add_local_constraint path info elv env =
  match info with
    {type_manifest = Some _; type_newtype_level = Some (lv, _)} ->
      (* elv is the expansion level, lv is the definition level *)
      let info = {info with type_newtype_level = Some (lv, elv)} in
      add_local_type path info env
  | _ -> assert false


(* Insertion of bindings by name *)

let enter store_fun name data env =
  let id = Ident.create name in (id, store_fun None id (Pident id) data env env)

let enter_value ?check = enter (store_value ?check)
and enter_type = enter (store_type ~check:true)
and enter_extension = enter (store_extension ~check:true)
and enter_module_declaration ?arg id md env =
  add_module_declaration ?arg ~check:true id md env
  (* let (id, env) = enter store_module name md env in
  (id, add_functor_arg ?arg id env) *)
and enter_modtype = enter store_modtype
and enter_class = enter store_class
and enter_cltype = enter store_cltype

let enter_module ?arg s mty env =
  let id = Ident.create s in
  (id, enter_module_declaration ?arg id (md mty) env)

(* Insertion of all components of a signature *)

let add_item comp env =
  match comp with
    Sig_value(id, decl)     -> add_value id decl env
  | Sig_type(id, decl, _)   -> add_type ~check:false id decl env
  | Sig_typext(id, ext, _)  -> add_extension ~check:false id ext env
  | Sig_module(id, md, _)   -> add_module_declaration ~check:false id md env
  | Sig_modtype(id, decl)   -> add_modtype id decl env
  | Sig_class(id, decl, _)  -> add_class id decl env
  | Sig_class_type(id, decl, _) -> add_cltype id decl env

let rec add_signature sg env =
  match sg with
    [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)

(* Open a signature path *)

let open_signature slot root sg env0 =
  (* First build the paths and substitution *)
  let (pl, _sub, sg) = prefix_idents_and_subst root Subst.identity sg in
  let sg = Lazy.force sg in

  (* Then enter the components in the environment after substitution *)

  let newenv =
    List.fold_left2
      (fun env item p ->
        match item with
          Sig_value(id, decl) ->
            store_value slot (Ident.hide id) p decl env env0
        | Sig_type(id, decl, _) ->
            store_type ~check:false slot (Ident.hide id) p decl env env0
        | Sig_typext(id, ext, _) ->
            store_extension ~check:false slot (Ident.hide id) p ext env env0
        | Sig_module(id, mty, _) ->
            store_module ~check:false slot (Ident.hide id) p mty env env0
        | Sig_modtype(id, decl) ->
            store_modtype slot (Ident.hide id) p decl env env0
        | Sig_class(id, decl, _) ->
            store_class slot (Ident.hide id) p decl env env0
        | Sig_class_type(id, decl, _) ->
            store_cltype slot (Ident.hide id) p decl env env0
      )
      env0 sg pl in
  { newenv with summary = Env_open(env0.summary, root) }

(* Open a signature from a file *)

let open_pers_signature name env =
  let ps = find_pers_struct name in
  open_signature None (Pident(Ident.create_persistent name))
    (Lazy.force ps.ps_sig) env

let open_signature ?(loc = Location.none) ?(toplevel = false) ovf root sg env =
  if not toplevel && ovf = Asttypes.Fresh && not loc.Location.loc_ghost
     && (Warnings.is_active (Warnings.Unused_open "")
         || Warnings.is_active (Warnings.Open_shadow_identifier ("", ""))
         || Warnings.is_active (Warnings.Open_shadow_label_constructor ("","")))
  then begin
    let used = ref false in
    !add_delayed_check_forward
      (fun () ->
        if not !used then
          Location.prerr_warning loc (Warnings.Unused_open (Path.name root))
      );
    let shadowed = ref [] in
    let slot s b =
      begin match check_shadowing env b with
      | Some kind when not (List.mem (kind, s) !shadowed) ->
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
    open_signature (Some slot) root sg env
  end
  else open_signature None root sg env

(* Read a signature from a file *)

let read_signature modname filename =
  let ps = read_pers_struct modname filename in
  Lazy.force ps.ps_sig

(* Return the CRC of the interface of the given compilation unit *)

let crc_of_unit name =
  let ps = find_pers_struct name in
  let crco =
    try
      List.assoc name ps.ps_crcs
    with Not_found ->
      assert false
  in
    match crco with
      None -> assert false
    | Some crc -> crc

(* Return the list of imported interfaces with their CRCs *)

let imports () =
  Consistbl.extract (StringSet.elements !imported_units) crc_units

(* Returns true if [s] is an opaque imported module  *)
let is_imported_opaque s =
  StringSet.mem s !imported_opaque_units

(* Save a signature to a file *)

let save_signature_with_imports ~deprecated sg modname filename imports =
  (*prerr_endline filename;
  List.iter (fun (name, crc) -> prerr_endline name) imports;*)
  Btype.cleanup_abbrev ();
  Subst.reset_for_saving ();
  let sg = Subst.signature (Subst.for_saving Subst.identity) sg in
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      (if !Clflags.unsafe_string then [Cmi_format.Unsafe_string] else []);
      (match deprecated with Some s -> [Deprecated s] | None -> []);
    ]
  in
  let oc = open_out_bin filename in
  try
    let cmi = {
      cmi_name = modname;
      cmi_sign = sg;
      cmi_crcs = imports;
      cmi_flags = flags;
    } in
    let crc = output_cmi filename oc cmi in
    close_out oc;
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let comps =
      components_of_module ~deprecated ~loc:Location.none
        empty Subst.identity
        (Pident(Ident.create_persistent modname)) (Mty_signature sg) in
    let ps =
      { ps_name = modname;
        ps_sig = lazy (Subst.signature Subst.identity sg);
        ps_comps = comps;
        ps_crcs = (cmi.cmi_name, Some crc) :: imports;
        ps_filename = filename;
        ps_flags = cmi.cmi_flags;
      } in
    save_pers_struct crc ps;
    cmi
  with exn ->
    close_out oc;
    remove_file filename;
    raise exn

let save_signature ~deprecated sg modname filename =
  save_signature_with_imports ~deprecated sg modname filename (imports())

(* Folding on environments *)

let find_all proj1 proj2 f lid env acc =
  match lid with
    | None ->
      EnvTbl.fold_name
        (fun id (p, data) acc -> f (Ident.name id) p data acc)
        (proj1 env) acc
    | Some l ->
      let p, desc = lookup_module_descr l env in
      begin match get_components desc with
          Structure_comps c ->
            Tbl.fold
              (fun s (data, pos) acc -> f s (Pdot (p, s, pos)) data acc)
              (proj2 c) acc
        | Functor_comps _ ->
            acc
      end

let find_all_simple_list proj1 proj2 f lid env acc =
  match lid with
    | None ->
      EnvTbl.fold_name
        (fun _id data acc -> f data acc)
        (proj1 env) acc
    | Some l ->
      let (_p, desc) = lookup_module_descr l env in
      begin match get_components desc with
          Structure_comps c ->
            Tbl.fold
              (fun _s comps acc ->
                match comps with
                  [] -> acc
                | (data, _pos) :: _ ->
                  f data acc)
              (proj2 c) acc
        | Functor_comps _ ->
            acc
      end

let fold_modules f lid env acc =
  match lid with
    | None ->
      let acc =
        EnvTbl.fold_name
          (fun id (p, data) acc -> f (Ident.name id) p data acc)
          env.modules
          acc
      in
      Hashtbl.fold
        (fun name ps acc ->
          match ps with
              None -> acc
            | Some ps ->
              f name (Pident(Ident.create_persistent name))
                     (md (Mty_signature (Lazy.force ps.ps_sig))) acc)
        persistent_structures
        acc
    | Some l ->
      let p, desc = lookup_module_descr l env in
      begin match get_components desc with
          Structure_comps c ->
            Tbl.fold
              (fun s (data, pos) acc ->
                f s (Pdot (p, s, pos))
                    (md (EnvLazy.force subst_modtype_maker data)) acc)
              c.comp_modules
              acc
        | Functor_comps _ ->
            acc
      end

let fold_values f =
  find_all (fun env -> env.values) (fun sc -> sc.comp_values) f
and fold_constructors f =
  find_all_simple_list (fun env -> env.constrs) (fun sc -> sc.comp_constrs) f
and fold_labels f =
  find_all_simple_list (fun env -> env.labels) (fun sc -> sc.comp_labels) f
and fold_types f =
  find_all (fun env -> env.types) (fun sc -> sc.comp_types) f
and fold_modtypes f =
  find_all (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) f
and fold_classs f =
  find_all (fun env -> env.classes) (fun sc -> sc.comp_classes) f
and fold_cltypes f =
  find_all (fun env -> env.cltypes) (fun sc -> sc.comp_cltypes) f


(* Make the initial environment *)
let (initial_safe_string, initial_unsafe_string) =
  Predef.build_initial_env
    (add_type ~check:false)
    (add_extension ~check:false)
    empty

(* Return the environment summary *)

let summary env =
  if PathMap.is_empty env.local_constraints then env.summary
  else Env_constraints (env.summary, env.local_constraints)

let last_env = ref empty
let last_reduced_env = ref empty

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

let report_error ppf = function
  | Illegal_renaming(modname, ps_name, filename) -> fprintf ppf
      "Wrong file naming: %a@ contains the compiled interface for @ \
       %s when %s was expected"
      Location.print_filename filename ps_name modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Need_recursive_types(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]"
        export import "The compilation flag -rectypes is required"
  | Depend_on_unsafe_string_unit(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, compiled with -unsafe-string.@ %s@]"
        export import "This compiler has been configured in strict \
                       -safe-string mode"
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

let () =
  Location.register_error_of_exn
    (function
      | Error (Missing_module (loc, _, _)
              | Illegal_value_name (loc, _)
               as err) when loc <> Location.none ->
          Some (Location.error_of_printer loc report_error err)
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
