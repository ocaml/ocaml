
open Config
open Misc
open Asttypes
open Longident
open Path
open Types

type error =
  | Unbound_namespace of Longident.t
  | Namespace_expected of Longident.t
  | Invalid_namespace_name of string
  | Inconsistent_namespace of string * string

exception Error of Location.t * error

module StringMap = Map.Make(struct
  type t = string let compare = compare
end)

type namespace_dir = {
  mutable spa_entries : namespace_entry StringMap.t;
  spa_name : string;
}

and namespace_entry =
    NamespaceModule of
	string (* module name *)
      * string (* filename *)
  | NamespaceDir of namespace_dir

let namespace_root = {
  spa_entries = StringMap.empty;
  spa_name = "";
}

let check_unit_name name =
  try
    begin match name.[0] with
      | 'A'..'Z' -> ()
      | _ ->
	raise (Error (Location.none, Invalid_namespace_name name))
    end;
    for i = 1 to String.length name - 1 do
      match name.[i] with
	| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
	| _ ->
	  raise (Error (Location.none, Invalid_namespace_name name))
    done;
  with Exit -> ()
;;

let need_init = ref true
let current_unit = ref ""
let set_unit_name modulename =
  if !Clflags.debug_ns then
    Printf.fprintf stderr "Nsutils.set_unit_name\n%!";
  current_unit := modulename;
  Clflags.ns_current_in_file := None;
  need_init := true

let ns_init () =
  if !need_init then
    let in_flags =
      match !Clflags.ns_current_in_flags with
	  None -> None
	| Some "-" ->
	  let dirname = Filename.dirname !Location.input_name in
	  let rec iter name list =
	    let basename = Filename.basename name in
	    let dirname = Filename.dirname name in
	    if dirname = name then
	      String.concat "." (List.rev list)
	    else
	      if basename = "." then
		iter dirname list
	      else begin
		let modname = String.capitalize basename in
		check_unit_name modname;
		iter dirname (modname :: list)
	      end

	  in
	  Some (iter dirname [])
	| Some s -> Some s
    in
    need_init := false;
    begin
      match in_flags, !Clflags.ns_current_in_file with
	  None, None -> ()
	| Some s, None -> Clflags.ns_current_in_file := Some s
	| None, Some s -> ()
	| Some s1, Some s2 ->
	  if s1 <> s2 then
	    raise (Error (Location.none, Inconsistent_namespace (s1,s2)))
    end
(*
    match !Clflags.ns_current_in_file with
	None -> ()
      | Some s ->
	let len = String.length s in
	if len = 0 || s.[0] <> '.' then
	  Clflags.ns_current_in_file := Some ("." ^ s)
*)
let basename modulename =
  try
    let s = modulename in
    let pos = String.rindex s '.' in
    String.sub s (pos+1) (String.length s - pos - 1)
  with Not_found -> modulename

let modulename () =
  ns_init ();
  match !Clflags.ns_current_in_file with
      None -> !current_unit
    | Some "" -> (* Rooted "." ^*) !current_unit
    | Some ns ->
      Printf.sprintf (* Rooted ".%s.%s" *) "%s.%s" ns !current_unit

let modulename () =
  let m = modulename () in
  if !Clflags.debug_ns then
    Printf.fprintf stderr "Nsutils.modulename returned %s\n%!" m;
  m

let ns_modname ns modname =
  if ns.spa_name = "" then modname else
    Printf.sprintf "%s.%s" ns.spa_name modname

let ns_get ns_dir modname =
  try
    StringMap.find modname ns_dir.spa_entries
  with Not_found ->
    let new_ns = NamespaceDir {
      spa_entries = StringMap.empty;
      spa_name = ns_modname ns_dir modname;
    } in
    ns_dir.spa_entries <- StringMap.add modname new_ns ns_dir.spa_entries;
    new_ns

let rec add_to_namespace file_is_error ns_dir modname dir =
  let ns = ns_get ns_dir modname in
  match ns with
      NamespaceModule (_,file) ->
(*	failwith (Printf.sprintf "File %s conflicts with directory in namespace %s" file modname) *)
	if file_is_error then begin
	  Printf.fprintf stderr "Namespaces error: file %s is hiding current namespace %s (case 2)\n%!" file modname;
	  exit 2
	end else
	  Printf.fprintf stderr "Namespaces warning: file %s is hiding namespace %s (case 2)\n%!" file modname;
    | NamespaceDir ns ->
      let files = Sys.readdir dir in
      Array.iter (fun file ->
	let filename = Filename.concat dir file in
	if Filename.check_suffix file ".cmi" then begin
	  let prefix = Filename.chop_suffix file ".cmi" in
	  let modname = String.capitalize prefix in
	  if Nsdepend.StringSet.mem modname !Nsdepend.free_structure_names then begin
	    try
	      let subns = StringMap.find modname ns.spa_entries in
	      match subns with
		  NamespaceDir _ ->
		    Printf.fprintf stderr "Namespaces: file %s is hiding namespace %s\n%!" filename ns.spa_name;
		    raise Not_found
		| _ -> raise Not_found
	    with Not_found ->
	      ns.spa_entries <- StringMap.add modname (NamespaceModule (modname, filename)) ns.spa_entries
	  end
	end
	else
	  if Sys.is_directory filename then begin
	    let modname = String.capitalize file in
	    if
	      Nsdepend.StringSet.mem modname !Nsdepend.free_structure_names then begin
		if !Clflags.debug_ns then
		  Printf.fprintf stderr "Namespaces: %s is in namespace %s\n%!" filename ns.spa_name;
		add_to_namespace file_is_error ns modname filename
	      end
	  end
      ) files

let rec print_namespace indent ns =
  Printf.fprintf stderr "%s%s:\n" indent ns.spa_name;
  StringMap.iter (fun modname ns ->
    match ns with
	NamespaceDir ns ->
	  Printf.fprintf stderr "%s  %s\n" indent modname;
	  print_namespace ("    " ^ indent) ns
      | NamespaceModule (modname2,file) ->
	Printf.fprintf stderr "%s  %s --> %s/%s\n" indent modname modname2 file;
  ) ns.spa_entries


let pre_open_namespace env lid =

  let (path, mty) =
    try
      Env.lookup_module lid env
    with Not_found ->
      raise(Error(Location.none, Unbound_namespace lid))
  in
  if !Clflags.debug_ns then
    Printf.fprintf Pervasives.stderr "pre_type_open %s\n%!" (Longident.name lid);
  match Mtype.scrape env mty with
      Tmty_namespace (_, sg) ->
	Env.open_namespace path (Lazy.force sg) env
    | _ -> raise(Error(Location.none, Namespace_expected lid))

let rec type_namespace ns_dir =
  let items = ref [] in
  StringMap.iter (fun modname ns ->

    match ns with
	NamespaceDir ns ->
	  let sig_item =
	    Tsig_module (Ident.create_persistent modname, Tmty_namespace (ns.spa_name, lazy (type_namespace ns)), Trec_not)
	  in
	  items := sig_item :: !items

	| NamespaceModule (modname2, filename) ->
	  let modname = ns_modname ns_dir modname in
	  try
	    let msig = Env.read_signature modname filename in
	    let id = Ident.create_persistent modname in
	    let sig_item =
	      Tsig_module (id, Tmty_signature msig, Trec_not)
	    in
	    items := sig_item :: !items
	  with
	      Env.Error (Env.Illegal_renaming (ps_name, _, _)) ->
		Printf.fprintf stderr "Namespaces warning: discarding file %s containing interface %s instead of %s\n%!"
		  filename ps_name modname
  ) ns_dir.spa_entries;
  !items


let add_namespaces modulename initial_env compute_dependencies ast sourcefile =
  ns_init ();
  match !Clflags.ns_current_in_file with
      None -> initial_env
    | Some current_namespace ->
      if !Clflags.debug_ns then
	Printf.fprintf stderr "Namespaces activated\n%!";

      let opened_namespaces =
	if current_namespace = "" then !Clflags.ns_path
	else
	  !Clflags.ns_path @ [ current_namespace ] in
      let opened_namespaces = List.map Longident.parse opened_namespaces in

      namespace_root.spa_entries <- StringMap.empty;

      (* Compute an upper approximation of which modules may be useful to decrease the size
	 of the search in the directories. *)
      Nsdepend.free_structure_names := Nsdepend.StringSet.empty;
      compute_dependencies Nsdepend.StringSet.empty ast;
      List.iter (Nsdepend.addmodule Nsdepend.StringSet.empty) opened_namespaces;
      begin match modulename with None -> () | Some modulename ->
	Nsdepend.free_structure_names :=
	  Nsdepend.StringSet.add (Env.ns_short_name modulename) !Nsdepend.free_structure_names;
      end;
      let needed_modules = !Nsdepend.free_structure_names in


      let new_env = ref initial_env in

      (* If current_namespace <> "", we must remove the current directory from the load_path *)
      if current_namespace <> "" then begin
	match !load_path with
	  | "" :: path -> load_path := path
	    | _ -> assert false
      end;
      List.iter (fun dir ->
	if !Clflags.debug_ns then
	  Printf.fprintf stderr "Namespaces: scanning toplevel %s\n%!" dir;
	let dir = if dir = "" then "." else dir in
	let files = Sys.readdir dir in
	Array.iter (fun file ->
	  let modname = String.capitalize file in
	  let filename = Filename.concat dir file in
	  if Nsdepend.StringSet.mem modname needed_modules && Sys.is_directory filename then
	    (* take a toplevel directory as a namespace ONLY if there is no toplevel module
	       directly available with the same name *)
	    try
	      ignore (find_in_path_uncap !load_path (modname ^ ".cmi"))
	    with Not_found ->
	      if !Clflags.debug_ns then
		Printf.fprintf stderr "%s is in namespaces\n%!" filename;
	      add_to_namespace false namespace_root modname filename
	) files;
      ) (List.rev !load_path);

      if current_namespace <> "" then begin
	let lid = Longident.parse current_namespace in
	let (modname, ns_dir) =
	  match lid with
		    Lapply _ -> assert false
	    | Lident id -> (id, namespace_root)
	    | Ldot (lid, modname) ->
	      let rec iter ns_dir lid =
		match lid with
		    Lapply _ -> assert false
		  | Lident id ->
		      ns_get ns_dir id
		  | Ldot (lid, modname) ->
		    let ns = iter ns_dir lid in
		    match ns with
		      | NamespaceModule (modname, file) ->
			Printf.fprintf stderr "Namespaces error: file %s is hidding current namespace %s\n%!" file current_namespace;
			exit 2
		      | NamespaceDir ns_dir ->
			ns_get ns_dir modname
	      in
	      let ns = iter namespace_root lid in
	      match ns with
		| NamespaceModule (modname, file) ->
		  Printf.fprintf stderr "Namespace error: file %s is hidding current namespace %s\n%!" file current_namespace;
		  exit 2
		| NamespaceDir ns_dir ->
		  if !Clflags.debug_ns then
		    Printf.fprintf stderr "Namespaces: adding current directory as namespace %s\n%!" current_namespace;
		  (modname, ns_dir)
	in
	let dirname = Filename.dirname sourcefile in
	add_to_namespace true ns_dir modname dirname
      end;

      if !Clflags.debug_ns then
	print_namespace "" namespace_root;

      StringMap.iter (fun modname ns ->

	match ns with
	    NamespaceModule _ -> assert  false
	  | NamespaceDir ns ->

	    let path = Pident (Ident.create modname) in
	    let mty = Tmty_namespace (ns.spa_name, lazy (type_namespace ns)) in
	    (* a faire ?
	       let mty = Mtype.strengthen env mty path in
	    *)

	    new_env := Env.store_module (Ident.create modname)
	      path mty !new_env

      ) namespace_root.spa_entries;

      List.iter (fun ns_lid ->
	new_env := pre_open_namespace !new_env ns_lid
      ) opened_namespaces;

      if !Clflags.debug_ns then
	Printf.fprintf stderr "Namespaces: loaded environment with namespaces\n%!";

      !new_env



let add_structure_namespaces modulename initial_env ast =
  add_namespaces modulename initial_env Nsdepend.add_structure ast

let add_signature_namespaces initial_env ast =
  add_namespaces None initial_env Nsdepend.add_signature ast

    (* Error report *)

open Printtyp
open Format

let report_error ppf = function
  | Unbound_namespace lid ->
    fprintf ppf "Unbound namespace %a" longident lid
  | Namespace_expected lid ->
    fprintf ppf "Namespace expected for %a" longident lid
  | Invalid_namespace_name name ->
    fprintf ppf "Invalid namespace name %s" name
  | Inconsistent_namespace (flag, file) ->
    fprintf ppf "Inconsistent namespace naming: arg=%s declared=%s"
      flag file
