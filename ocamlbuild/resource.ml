(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log

module Resources = Set.Make(Pathname)

let print = Pathname.print

let equal = (=)
let compare = compare

module Cache = struct
  open Pathname.Operators

  let clean () = Shell.chdir Pathname.pwd; Shell.rm_rf !Options.build_dir

  type knowledge =
    | Yes
    | No
    | Unknown

  type suspension = (Command.t * (unit -> unit))

  type build_status =
    | Bbuilt
    | Bcannot_be_built
    | Bnot_built_yet
    | Bsuspension of suspension

  type cache_entry =
    { mutable built        : build_status;
      mutable changed      : knowledge;
      mutable dependencies : Resources.t }

  let empty () =
    { built        = Bnot_built_yet;
      changed      = Unknown;
      dependencies = Resources.empty }

  let print_knowledge f =
    function
    | Yes -> pp_print_string f "Yes"
    | No  -> pp_print_string f "No"
    | Unknown -> pp_print_string f "Unknown"

  let print_build_status f =
    function
    | Bbuilt -> pp_print_string f "Bbuilt"
    | Bnot_built_yet -> pp_print_string f "Bnot_built_yet"
    | Bcannot_be_built -> pp_print_string f "Bcannot_be_built"
    | Bsuspension(cmd, _) ->
        fprintf f "@[<2>Bsuspension(%a,@ (<fun> : unit -> unit))@]" Command.print cmd

  let print_cache_entry f e =
    fprintf f "@[<2>{ @[<2>built =@ %a@];@ @[<2>changed =@ %a@];@ @[<2>dependencies =@ %a@]@ }@]"
      print_build_status e.built print_knowledge e.changed Resources.print e.dependencies

  let cache = Hashtbl.create 103

  let get r =
    try Hashtbl.find cache r
    with Not_found ->
      let cache_entry = empty () in
      Hashtbl.add cache r cache_entry; cache_entry

  let fold_cache f x = Hashtbl.fold f cache x

  let print_cache f () =
    fprintf f "@[<hv0>@[<hv2>{:";
    fold_cache begin fun k v () ->
      fprintf f "@ @[<2>%a =>@ %a@];" print k print_cache_entry v
    end ();
    fprintf f "@]:}@]"

  let print_graph f () =
    fprintf f "@[<hv0>@[<hv2>{:";
    fold_cache begin fun k v () ->
      if not (Resources.is_empty v.dependencies) then
        fprintf f "@ @[<2>%a =>@ %a@];" print k Resources.print v.dependencies
    end ();
    fprintf f "@]@ :}@]"

  let resource_changed r =
    dprintf 10 "resource_changed:@ %a" print r;
    (get r).changed <- Yes

  let rec resource_has_changed r =
    let cache_entry = get r in
    match cache_entry.changed with
    | Yes -> true
    | No -> false
    | Unknown ->
      let res =
        match cache_entry.built with
        | Bbuilt -> false
        | Bsuspension _ -> assert false
        | Bcannot_be_built -> false
        | Bnot_built_yet -> not (Pathname.is_up_to_date false r) in
      let () = cache_entry.changed <- if res then Yes else No in res

  let resource_state r = (get r).built

  let resource_is_built r = (get r).built = Bbuilt

  let resource_built r = (get r).built <- Bbuilt

  let resource_is_failed r = (get r).built = Bcannot_be_built

  let resource_failed r = (get r).built <- Bcannot_be_built

  let suspend_resource r cmd kont prods =
    let cache_entry = get r in
    match cache_entry.built with
    | Bsuspension _ -> ()
    | Bbuilt -> ()
    | Bcannot_be_built -> assert false
    | Bnot_built_yet ->
        let kont = begin fun () ->
          kont ();
          List.iter begin fun prod ->
            (get prod).built <- Bbuilt
          end prods
        end in cache_entry.built <- Bsuspension(cmd, kont)

  let resume_suspension (cmd, kont) =
    Command.execute cmd;
    kont ()

  let resume_resource r =
    let cache_entry = get r in
    match cache_entry.built with
    | Bsuspension(s) -> resume_suspension s
    | Bbuilt -> ()
    | Bcannot_be_built -> ()
    | Bnot_built_yet -> ()

  let get_optional_resource_suspension r =
    match (get r).built with
    | Bsuspension cmd_kont -> Some cmd_kont
    | Bbuilt | Bcannot_be_built | Bnot_built_yet -> None

  let clear_resource_failed r = (get r).built <- Bnot_built_yet

  let dependencies r = (get r).dependencies

  let fold_dependencies f =
    fold_cache (fun k v -> Resources.fold (f k) v.dependencies)

  let add_dependency r s =
    let cache_entry = get r in
    cache_entry.dependencies <- Resources.add s cache_entry.dependencies

  let print_dependencies = print_graph

  let digest_resource p =
    let f = Pathname.to_string (Pathname.in_build_dir p) in
    let buf = Buffer.create 1024 in
    Buffer.add_string buf f;
    (if sys_file_exists f then Buffer.add_string buf (Digest.file f));
    Digest.string (Buffer.contents buf)

  let digests = Hashtbl.create 103

  let get_digest_for name =
    try Some (Hashtbl.find digests name)
    with Not_found -> None
  let store_digest name d = Hashtbl.replace digests name d

  let _digests = lazy (Pathname.pwd / !Options.build_dir / (Pathname.mk "_digests"))

  let finalize () =
    with_output_file !*_digests begin fun oc ->
      Hashtbl.iter begin fun name digest ->
        Printf.fprintf oc "%S: %S\n" name digest
      end digests
    end

  let init () =
    Shell.chdir !Options.build_dir;
    if Pathname.exists !*_digests then
      with_input_file !*_digests begin fun ic ->
        try while true do
          let l = input_line ic in
          Scanf.sscanf l "%S: %S" store_digest
        done with End_of_file -> ()
      end;
    My_unix.at_exit_once finalize

end

let clean p = Shell.rm_f p

(*
type env = string

let split_percent s =
  try
    let pos = String.index s '%' in
    Some (String.before s pos, String.after s (pos + 1))
  with Not_found -> None

let extract prefix suffix s =
  let lprefix = String.length prefix in
  let lsuffix = String.length suffix in
  let ls = String.length s in
  if lprefix + lsuffix > ls then None else
  let s' = String.sub s lprefix (ls - lsuffix - lprefix) in
  if equal (prefix ^ s' ^ suffix) s then Some s' else None

let matchit r1 r2 =
  match split_percent r1 with
  | Some (x, y) -> extract x y r2
  | _ -> if equal r1 r2 then Some "" else None

let rec subst percent r =
  match split_percent r with
  | Some (x, y) -> x ^ percent ^ y
  | _ -> r

let print_env = pp_print_string
*)

let is_up_to_date path = Pathname.is_up_to_date true path

let import x = x

module MetaPath : sig

        type t
	type env

        val mk : string -> t
	val matchit : t -> string -> env option
	val subst : env -> t -> string
	val print_env : Format.formatter -> env -> unit

end = struct

	type atoms = A of string | V of string
	type t = atoms list
	type env = (string * string) list

	exception No_solution

	let mk s = List.map (fun (s, is_var) -> if is_var then V s else A s) (Lexers.meta_path (Lexing.from_string s))

  let mk = memo mk

	let match_prefix s pos prefix =
		match String.contains_string s pos prefix with
		| Some(pos') ->	if pos = pos' then pos' + String.length prefix else raise No_solution
		| None -> raise No_solution

	let matchit p s =
	  let sl = String.length s in
		let rec loop xs pos acc =
			match xs with
			| [] -> if pos = sl then acc else raise No_solution
			| A prefix :: xs -> loop xs (match_prefix s pos prefix) acc
			| V var :: A s2 :: xs ->
					begin match String.contains_string s pos s2 with
					| Some(pos') ->	loop xs (pos' + String.length s2) ((var, String.sub s pos (pos' - pos)) :: acc)
					| None -> raise No_solution
				  end
			| [V var] -> (var, String.sub s pos (sl - pos)) :: acc
			| V _ :: _ -> assert false
		in
		try	Some (loop p 0 [])
		with No_solution -> None

  let pp_opt pp_elt f =
    function
    | None -> pp_print_string f "None"
    | Some x -> Format.fprintf f "Some(%a)" pp_elt x

  let print_env f env =
    List.iter begin fun (k, v) ->
      if k = "" then Format.fprintf f "%%=%s " v
      else Format.fprintf f "%%(%s)=%s " k v
    end env

  (* let matchit p s =
    let res = matchit p s in
      Format.eprintf "matchit %S %S = %a@." p s (pp_opt print_env) res;
    res

  let _ = begin
    assert (matchit "%(path)lib%(libname).a" "libfoo.a" <> None);
    assert (matchit "%(path)lib%(libname).a" "path/libfoo.a" <> None);
    assert (matchit "libfoo.a" "libfoo.a" <> None);
    assert (matchit "lib%(libname).a" "libfoo.a" <> None);
    assert (matchit "%(path)libfoo.a" "path/libfoo.a" <> None);
    assert (matchit "foo%" "foobar" <> None);
    exit 42
  end;; *)

	let subst env s =
		String.concat "" begin
			List.map begin fun x ->
				match x with
				| A atom -> atom
				| V var -> List.assoc var env
			end s
		end
end

type env = MetaPath.env
type resource_pattern = (Pathname.t * MetaPath.t)

let print_pattern f (x, _) = Pathname.print f x

let import_pattern x = x, MetaPath.mk x
let matchit (_, p) x = MetaPath.matchit p x

let subst env s = MetaPath.subst env (MetaPath.mk s)
let subst_pattern env (_, p) = MetaPath.subst env p

let print_env = MetaPath.print_env
