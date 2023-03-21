(*
  Note, a program being debugged does not have access to the
  debugger process, but a printer that has been loaded is
  running in the debugger process and has access to its
  internals. This can be used as a back-door to enable some
  unit testing of the debugger.
*)
open Format

let debug = true

(* Convert to system-dependent filename from part. *)
let to_sys_name parts =
  match parts with
  | [] -> ""
  | hd :: tl ->
     List.fold_left Filename.concat hd tl

let to_canonical parts =
  match parts with
  | [] -> "."
  | hd :: tl ->
     if hd = "/" then
       "/" ^ String.concat "/" tl
     else
       String.concat "/" parts

let filename_parts filename =
  let open Filename in
  let rec loop filename acc =
    let d = dirname filename in
    if d = filename then filename :: acc
    else
      let b = basename filename in
      loop d (b :: acc)
  in
  loop filename []

let canonicalize filename =
  let parts = filename_parts filename in
  to_canonical parts

let print_mapping ppf title src (alist: string list) =
  fprintf ppf "@[<v2>{%s, %s -> #items=%d@ " title (canonicalize src) (List.length alist);
  if debug then
    List.iteri (fun i item ->
        fprintf ppf "[%d]=%s@ " i (canonicalize item)
      ) alist;
  fprintf ppf "}@]\n@."

let print_list ppf title (alist: string list) =
  fprintf ppf "@[<v2>{%s: #items=%d@ " title (List.length alist);
  if debug then
    List.iteri (fun i item ->
        fprintf ppf "[%d]=%s@ " i (canonicalize item)
      ) alist;
  fprintf ppf "}@]\n@."

let p : Format.formatter -> int -> unit = fun ppf n ->
  fprintf ppf "%d@." n;
  let bppm = Sys.getenv "DEPLOY_PATH_PREFIX_MAP" in
  if debug then
    fprintf ppf "DEPLOY_PATH_PREFIX_MAP=\"%s\"\n%!" bppm;
  let test1_dir =
    to_sys_name ["/workspace_root"; "pack1"; "lib1"] in
  let result1 = Location.rewrite_find_all_existing_dirs test1_dir in
  print_mapping ppf "test1_dir" test1_dir result1;
  let test1_mod =
    to_sys_name ["/workspace_root"; "pack1"; "lib1"; "mod1.ml"] in
  let open Printf in
  match Location.rewrite_find_first_existing test1_mod with
  | None ->
     (* DEPLOY_PATH_PREFIX_MAP not set. *)
     printf "%s -> None\n%!" (canonicalize test1_mod)
  | Some target ->
     printf "%s -> Some %s\n%!"
       (canonicalize test1_mod) (canonicalize target)
  | exception Not_found ->
     printf "DEPLOY_PATH_PREFIX_MAP fails to find %s.\n%!"
       (canonicalize test1_mod);
  if false then begin
      let load_paths = Load_path.get_paths () in
      print_list ppf "Load_path" load_paths
  end
