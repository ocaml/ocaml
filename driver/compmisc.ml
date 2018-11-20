(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Initialize the search path.
   [dir] is always searched first (default: the current directory),
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
 *)

let last_path = ref []
let last_native = ref false

let get_init_path ?dir native =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads && not native then
      "+vmthreads" :: !Clflags.include_dirs
    else
      !Clflags.include_dirs
  in
  let dirs =
    !Compenv.last_include_dirs
      @ dirs
      @ Config.flexdll_dirs
      @ !Compenv.first_include_dirs
  in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  let suffix = List.rev_append exp_dirs (Clflags.std_include_dir ()) in
  Option.fold ~none:suffix ~some:(fun dir -> List.cons dir suffix) dir

let init_path ?(dir="") native =
  let path = get_init_path ~dir native in
  Config.load_path := path;
  last_path := path;
  last_native := native;
  Env.reset_cache ()

(* Used by the toplevel to cleanup incorrect decisions taken by init_path at
   module initialisation-time, before the command line args had been parsed *)
let reinit_path ?dir () =
  let rec remove_tail = function
  | ((_, []) as r)
  | (([], _) as r) -> r
  | ((x::xs, y::ys) as r) ->
      if String.equal x y then
        remove_tail (xs, ys)
      else
        r
  in
  let get_suffix xs ys =
    match xs with
    | x::_ ->
        let rec loop acc = function
        | [] -> ([], acc)
        | (y::ys) as r ->
            if String.equal x y then
              (r, acc)
            else
              loop (y::acc) ys
        in
        loop [] ys
    | [] -> ([], List.rev ys)
  in
(*  Printf.eprintf "Reinit: load_path\n%!";
List.iter (Printf.eprintf "  -%s\n%!") (List.rev !Config.load_path);
  Printf.eprintf "Reinit: last_path\n%!";
List.iter (Printf.eprintf "  -%s\n%!") (List.rev !last_path);*)
  let last = List.rev !last_path in
  let (load_path, suffix) = get_suffix last (List.rev !Config.load_path) in
  match remove_tail (load_path, last) with
  | (prefix, []) ->
      let path = get_init_path ?dir !last_native in
      Config.load_path := List.rev_append prefix path @ suffix;
      last_path := path;
      Env.reset_cache ()
  | _ ->
      (* The directories added by Config.load_path have been altered *)
      ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#1775) *)

let initial_env () =
  Ident.reinit();
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~safe_string:(Config.safe_string || not !Clflags.unsafe_string)
    ~initially_opened_module
    ~open_implicit_modules:(List.rev !Clflags.open_modules)

let set_from_env flag Clflags.{ parse; usage; env_var } =
  try
    match parse (Sys.getenv env_var) with
    | None ->
        Location.prerr_warning Location.none
          (Warnings.Bad_env_variable (env_var, usage))
    | Some x -> match !flag with
      | None -> flag := Some x
      | Some _ -> ()
  with
    Not_found -> ()

let read_clflags_from_env () =
  set_from_env Clflags.color Clflags.color_reader;
  set_from_env Clflags.error_style Clflags.error_style_reader;
  ()

let with_ppf_dump ~fileprefix f =
  let ppf_dump, finally =
    if not !Clflags.dump_into_file
    then Format.err_formatter, ignore
    else
       let ch = open_out (fileprefix ^ ".dump") in
       let ppf = Format.formatter_of_out_channel ch in
       ppf,
       (fun () ->
         Format.pp_print_flush ppf ();
         close_out ch)
  in
  Misc.try_finally (fun () -> f ppf_dump) ~always:finally
