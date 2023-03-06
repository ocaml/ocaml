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
  fprintf ppf "@[<v2>{%s, #items=%d@ " title (List.length alist);
  List.iteri (fun i item ->
    fprintf ppf "[%d]=%s@ " i item
    ) alist;
  fprintf ppf "}@]\n@."

let p : Format.formatter -> int -> unit = fun ppf n ->
  fprintf ppf "%d@." n;
  let bppm = Sys.getenv "BUILD_PATH_PREFIX_MAP" in
  fprintf ppf "BUILD_PATH_PREFIX_MAP=\"%s\"\n%!" bppm;
  let test1 = "/workspace_root/pack1/lib1/" in
  let result1 = Ocamldebug.Symbols.bppm_expand_path test1 in
  print_mapping ppf "t1" test1 result1;
  let load_paths = Ocamldebug.Symbols.get_load_path () in
  print_list ppf "Load path" load_paths
