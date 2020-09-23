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

let init_path ?(dir="") () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else
      !Clflags.include_dirs
  in
  let dirs =
    !Compenv.last_include_dirs @ dirs @ Config.flexdll_dirs @
    !Compenv.first_include_dirs
  in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  Load_path.init (dir :: List.rev_append exp_dirs (Clflags.std_include_dir ()));
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

let with_ppf_dump ~file_prefix f =
  let ppf_dump, finally =
    if not !Clflags.dump_into_file
    then Format.err_formatter, ignore
    else
       let ch = open_out (file_prefix ^ ".dump") in
       let ppf = Format.formatter_of_out_channel ch in
       ppf,
       (fun () ->
         Format.pp_print_flush ppf ();
         close_out ch)
  in
  Misc.try_finally (fun () -> f ppf_dump) ~always:finally
