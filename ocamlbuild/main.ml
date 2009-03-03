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


(* Original author: Berke Durak *)
open My_std
open Log
open Pathname.Operators
open Command
open Tools
open Ocaml_specific
open Format
;;

exception Exit_build_error of string
exception Exit_silently

let clean () =
  Log.finish ();
  Shell.rm_rf !Options.build_dir;
  if !Options.make_links then begin
    let entry =
      Slurp.map (fun _ _ _ -> true)
        (Slurp.slurp Filename.current_dir_name)
    in
    Slurp.force (Resource.clean_up_links entry)
  end;
  raise Exit_silently
;;

let show_tags () =
  List.iter begin fun path ->
    Log.eprintf "@[<2>Tags for %S:@ {. %a .}@]" path Tags.print (tags_of_pathname path)
  end !Options.show_tags
;;

let show_documentation () =
  let rules = Rule.get_rules () in
  let flags = Flags.get_flags () in
  let pp fmt = Log.raw_dprintf (-1) fmt in
  List.iter begin fun rule ->
    pp "%a@\n@\n" (Rule.pretty_print Resource.print_pattern) rule
  end rules;
  List.iter begin fun (tags, flag) ->
    let sflag = Command.string_of_command_spec flag in
    pp "@[<2>flag@ {. %a .}@ %S@]@\n@\n" Tags.print tags sflag
  end flags;
  pp "@."
;;

let proceed () =
  Hooks.call_hook Hooks.Before_options;
  Options.init ();
  if !Options.must_clean then clean ();
  Hooks.call_hook Hooks.After_options;
  Plugin.execute_plugin_if_needed ();

  if !Options.targets = []
    && !Options.show_tags = []
    && not !Options.show_documentation
    then raise Exit_silently;

  let target_dirs = List.union [] (List.map Pathname.dirname !Options.targets) in

  Configuration.parse_string
    "<**/*.ml> or <**/*.mli> or <**/*.mlpack> or <**/*.ml.depends>: ocaml
     <**/*.byte>: ocaml, byte, program
     <**/*.odoc>: ocaml, doc
     <**/*.native>: ocaml, native, program
     <**/*.cma>: ocaml, byte, library
     <**/*.cmxa>: ocaml, native, library
     <**/*.cmo>: ocaml, byte
     <**/*.cmi>: ocaml, byte, native
     <**/*.cmx>: ocaml, native
    ";

  Configuration.tag_any !Options.tags;
  if !Options.recursive
  || Sys.file_exists (* authorized since we're not in build *) "_tags"
  || Sys.file_exists (* authorized since we're not in build *) "myocamlbuild.ml"
  then Configuration.tag_any ["traverse"];

  let newpwd = Sys.getcwd () in
  Sys.chdir Pathname.pwd;
  let entry_include_dirs = ref [] in
  let entry =
    Slurp.filter
      begin fun path name _ ->
        let dir =
          if path = Filename.current_dir_name then
            None
          else
            Some path
        in
        let path_name = path/name in
        if name = "_tags" then
          ignore (Configuration.parse_file ?dir path_name);

        (String.length name > 0 && name.[0] <> '_' && name <> !Options.build_dir && not (List.mem name !Options.exclude_dirs))
        && begin
          if path_name <> Filename.current_dir_name && Pathname.is_directory path_name then
            let tags = tags_of_pathname path_name in
            if Tags.mem "include" tags
            || List.mem path_name !Options.include_dirs then
              (entry_include_dirs := path_name :: !entry_include_dirs; true)
            else
              Tags.mem "traverse" tags
              || List.exists (Pathname.is_prefix path_name) !Options.include_dirs
              || List.exists (Pathname.is_prefix path_name) target_dirs
          else true
        end
      end
      (Slurp.slurp Filename.current_dir_name)
  in
  Hooks.call_hook Hooks.Before_hygiene;
  let hygiene_entry =
    Slurp.map begin fun path name () ->
      let tags = tags_of_pathname (path/name) in
      not (Tags.mem "not_hygienic" tags) && not (Tags.mem "precious" tags)
    end entry in
  if !Options.hygiene then
    Fda.inspect hygiene_entry
  else
    Slurp.force hygiene_entry;
  let entry = hygiene_entry in
  Hooks.call_hook Hooks.After_hygiene;
  Options.include_dirs := Pathname.current_dir_name :: List.rev !entry_include_dirs;
  dprintf 3 "include directories are:@ %a" print_string_list !Options.include_dirs;
  Options.entry := Some entry;

  List.iter Configuration.parse_string !Options.tag_lines;

  Hooks.call_hook Hooks.Before_rules;
  Ocaml_specific.init ();
  Hooks.call_hook Hooks.After_rules;

  Sys.chdir newpwd;
  (*let () = dprintf 0 "source_dir_path_set:@ %a" StringSet.print source_dir_path_set*)

  if !Options.show_documentation then begin
    show_documentation ();
    raise Exit_silently
  end;
  Digest_cache.init ();

  Sys.catch_break true;

  show_tags ();

  let targets =
    List.map begin fun starget ->
      let starget = Resource.import starget in
      let target = path_and_context_of_string starget in
      let ext = Pathname.get_extension starget in
      (target, starget, ext)
    end !Options.targets in

  try
    let targets =
      List.map begin fun (target, starget, ext) ->
        Shell.mkdir_p (Pathname.dirname starget);
        let target = Solver.solve_target starget target in
        (target, ext)
      end targets in

    Command.dump_parallel_stats ();

    Log.finish ();

    Shell.chdir Pathname.pwd;

    let call spec = sys_command (Command.string_of_command_spec spec) in

    let cmds =
      List.fold_right begin fun (target, ext) acc ->
        let cmd = !Options.build_dir/target in
        let link x =
          if !Options.make_links then ignore (call (S [A"ln"; A"-sf"; P x; A Pathname.current_dir_name])) in
        match ext with
        | "byte" | "native" | "top" ->
            link cmd; cmd :: acc
        | "html" ->
            link (Pathname.dirname cmd); acc
        | _ ->
            if !Options.program_to_execute then
              eprintf "Warning: Won't execute %s whose extension is neither .byte nor .native" cmd;
            acc
      end targets [] in

    if !Options.program_to_execute then
      begin
        match List.rev cmds with
        | [] -> raise (Exit_usage "Using -- requires one target");
        | cmd :: rest ->
          if rest <> [] then dprintf 0 "Warning: Using -- only run the last target";
          let cmd_spec = S [P cmd; atomize !Options.program_args] in
          dprintf 3 "Running the user command:@ %a" Pathname.print cmd;
          raise (Exit_with_code (call cmd_spec)) (* Exit with the exit code of the called command *)
      end
    else
      ()
  with
  | Ocaml_dependencies.Circular_dependencies(seen, p) ->
      raise
        (Exit_build_error
          (sbprintf "@[<2>Circular dependencies: %S already seen in@ %a@]@." p pp_l seen))
;;

open Exit_codes;;

let main () =
  let exit rc =
    Log.finish ~how:(if rc <> 0 then `Error else `Success) ();
    Pervasives.exit rc
  in
  try
    proceed ()
  with e ->
    if !Options.catch_errors then
      try raise e with
      | Exit_OK -> exit rc_ok
      | Fda.Exit_hygiene_failed ->
          Log.eprintf "Exiting due to hygiene violations.";
          exit rc_hygiene
      | Exit_usage u ->
          Log.eprintf "Usage:@ %s." u;
          exit rc_usage
      | Exit_system_error msg ->
          Log.eprintf "System error:@ %s." msg;
          exit rc_system_error
      | Exit_with_code rc ->
          exit rc
      | Exit_silently ->
          Log.finish ~how:`Quiet ();
          Pervasives.exit rc_ok
      | Exit_silently_with_code rc ->
          Log.finish ~how:`Quiet ();
          Pervasives.exit rc
      | Solver.Failed backtrace ->
          Log.raw_dprintf (-1) "@[<v0>@[<2>Solver failed:@ %a@]@\n@[<v2>Backtrace:%a@]@]@."
            Report.print_backtrace_analyze backtrace Report.print_backtrace backtrace;
          exit rc_solver_failed
      | Failure s ->
          Log.eprintf "Failure:@ %s." s;
          exit rc_failure
      | Solver.Circular(r, rs) ->
          Log.eprintf "Circular build detected@ (%a already seen in %a)"
          Resource.print r (List.print Resource.print) rs;
          exit rc_circularity
      | Invalid_argument s ->
          Log.eprintf
            "INTERNAL ERROR: Invalid argument %s\n\
            This is likely to be a bug, please report this to the ocamlbuild\n\
            developers." s;
          exit rc_invalid_argument
      | Ocaml_utils.Ocamldep_error msg ->
          Log.eprintf "Ocamldep error: %s" msg;
          exit rc_ocamldep_error
      | Lexers.Error msg ->
          Log.eprintf "Lexical analysis error: %s" msg;
          exit rc_lexing_error
      | Arg.Bad msg ->
          Log.eprintf "%s" msg;
          exit rc_usage
      | Exit_build_error msg ->
          Log.eprintf "%s" msg;
          exit rc_build_error
      | Arg.Help msg ->
          Log.eprintf "%s" msg;
          exit rc_ok
      | e ->
          try
            Log.eprintf "%a" My_unix.report_error e;
            exit 100 
          with
          | e ->
            Log.eprintf "Exception@ %s." (Printexc.to_string e);
            exit 100
    else raise e
;;
