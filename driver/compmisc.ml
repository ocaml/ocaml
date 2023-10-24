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

let auto_include find_in_dir fn =
  if !Clflags.no_std_include then
    raise Not_found
  else
    let alert = Location.auto_include_alert in
    Load_path.auto_include_otherlibs alert find_in_dir fn

(* Initialize the search path.
   [dir] (default: the current directory)
   is always searched first  unless -nocwd is specified,
   then the directories specified with the -I option (in command line order),
   then the standard library directory (unless the -nostdlib option is given),
   then the directories specified with the -H option (in command line order).
 *)

let init_path ?(auto_include=auto_include) ?(dir="") () =
  let visible =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else
      !Clflags.include_dirs
  in
  let visible =
    List.concat
      [!Compenv.last_include_dirs;
       visible;
       Config.flexdll_dirs;
       !Compenv.first_include_dirs]
  in
  let visible =
    List.map (Misc.expand_directory Config.standard_library) visible
  in
  let visible =
    (if !Clflags.no_cwd then [] else [dir])
    @ List.rev_append visible (Clflags.std_include_dir ())
  in
  let hidden =
    List.rev_map (Misc.expand_directory Config.standard_library)
      !Clflags.hidden_include_dirs
  in
  Load_path.init ~auto_include ~visible ~hidden;
  Env.reset_cache ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#8227) *)

let initial_env () =
  Ident.reinit();
  Types.Uid.reinit();
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
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
  let no_color () = (* See https://no-color.org/ *)
    match Sys.getenv_opt "NO_COLOR" with
    | None | Some "" -> false
    | _ -> true
  in
  if Option.is_none !Clflags.color && no_color () then
    Clflags.color := Some Misc.Color.Never;
  set_from_env Clflags.error_style Clflags.error_style_reader;
  ()

let rec make_directory dir =
  if Sys.file_exists dir then () else
    begin
      make_directory (Filename.dirname dir);
      Sys.mkdir dir 0o777
    end

let with_ppf_dump ~file_prefix f =
  let with_ch ch =
    let ppf = Format.formatter_of_out_channel ch in
    ppf,
    (fun () ->
       Format.pp_print_flush ppf ();
       close_out ch)
  in
  let ppf_dump, finally =
    match !Clflags.dump_dir, !Clflags.dump_into_file with
    | None, false -> Format.err_formatter, ignore
    | None, true -> with_ch (open_out (file_prefix ^ ".dump"))
    | Some d, _ ->
        let () = make_directory Filename.(dirname @@ concat d @@ file_prefix) in
        let _, ch =
          Filename.open_temp_file ~temp_dir:d (file_prefix ^ ".")  ".dump"
        in
        with_ch ch

  in
  Misc.try_finally (fun () -> f ppf_dump) ~always:finally
