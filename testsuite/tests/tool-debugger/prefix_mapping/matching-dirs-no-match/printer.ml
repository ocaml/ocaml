(*
  Note, a program being debugged does not have access to the
  debugger process, but a printer that has been loaded is
  running in the debugger process and has access to its
  internals. This can be used as a back-door to enable some
  unit testing of the debugger.
*)
open Format

let print_mapping ppf title src (alist: string list) =
  fprintf ppf "@[<v2>{%s, %s -> #items=%d@ " title src (List.length alist);
  List.iteri (fun i item ->
    fprintf ppf "[%d]=%s@ " i item
    ) alist;
  fprintf ppf "}@]\n@."

let print_list ppf title (alist: string list) =
  fprintf ppf "@[<v2>{%s: #items=%d@ " title (List.length alist);
  List.iteri (fun i item ->
    fprintf ppf "[%d]=%s@ " i item
    ) alist;
  fprintf ppf "}@]\n@."

let p : Format.formatter -> int -> unit = fun ppf n ->
  let debug = false in
  fprintf ppf "%d@." n;
  let bppm = Sys.getenv "DEPLOY_PATH_PREFIX_MAP" in
  fprintf ppf "DEPLOY_PATH_PREFIX_MAP=\"%s\"\n%!" bppm;
  let test1 = "/workspace_root/pack1/lib1/" in
  let result1 = match Location.rewrite_find_all_existing_dirs test1 with
  | dirs -> dirs
  | exception Not_found ->
    (* We are doing mapping, but either no prefix matches, or else
       mapped directories did not exists. *)
    Printf.printf "DEPLOY_PATH_PREFIX_MAP fails to map %S.\n%!" test1;
    [] in
  print_mapping ppf "t1" test1 result1;
  let load_paths = Load_path.get_paths () in
  fprintf ppf "Load path length = %d.@." (List.length load_paths);
  if debug then
    print_list ppf "Load_path" load_paths
