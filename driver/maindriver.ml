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

open Clflags


module Options = Main_args.Make_bytecomp_options (Main_args.Default.Main)


let program = "ocamlc"

let main_log rlog ppf =
  let device = Clflags.create_log_device ppf in
  let log = Location.log_on_device ~prev:!rlog device in
  rlog := log;
  log

let process continue argv log ppf =
  Compenv.readenv !log Before_args;
  Compenv.parse_arguments (ref argv) Compenv.anonymous program;
  Compmisc.read_clflags_from_env ();
  let log = main_log log ppf in
  if !Clflags.plugin then
    Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
  begin try
    Compenv.process_deferred_actions {
      log;
      compile_implementation = Compile.implementation;
      compile_interface = Compile.interface;
      ocaml_mod_ext = ".cmo";
      ocaml_lib_ext = ".cma";
    }
  with Arg.Bad msg ->
    begin
      prerr_endline msg;
      Clflags.print_arguments program;
      exit 2
    end
  end;
  if Clflags.(should_stop_after Compiler_pass.Lambda)
  then continue ();
  Compenv.readenv log Before_link;
  if
    List.length
      (List.filter (fun x -> !x)
         [make_archive;make_package;Compenv.stop_early;output_c_object])
    > 1
  then begin
    let module P = Clflags.Compiler_pass in
    match !stop_after with
    | None ->
        Compenv.fatal
          "Please specify at most one of -pack, -a, -c, -output-obj";
    | Some ((P.Parsing | P.Typing | P.Lambda) as p) ->
        assert (P.is_compilation_pass p);
        Compenv.fatalf
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -output-obj"
          (String.concat "|"
             (P.available_pass_names ~filter:(fun _ -> true) ~native:false))
    | Some (P.Scheduling | P.Emit) -> assert false (* native only *)
  end;
  if !make_archive then begin
    Compmisc.init_path ();

    Bytelibrarian.create_archive
      (Compenv.get_objfiles ~with_ocamlparam:false)
      (Compenv.extract_output !output_name);
    Warnings.check_fatal ();
  end
  else if !make_package then begin
    Compmisc.init_path ();
    let extracted_output = Compenv.extract_output !output_name in
    let revd = Compenv.get_objfiles ~with_ocamlparam:false in
    Compmisc.with_ppf_dump ~file_prefix:extracted_output (fun ppf_dump ->
        Bytepackager.package_files ~ppf_dump (Compmisc.initial_env ())
          revd (extracted_output));
    Warnings.check_fatal ();
  end
  else if not !Compenv.stop_early && !objfiles <> [] then begin
    let target =
      if !output_c_object && not !output_complete_executable then
        let s = Compenv.extract_output !output_name in
        if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
            || Filename.check_suffix s ".c")
        then s
        else
          Compenv.fatal
            (Printf.sprintf
               "The extension of the output file must be .c, %s or %s"
               Config.ext_obj Config.ext_dll
            )
      else
        Compenv.default_output !output_name
    in
    Compmisc.init_path ();
    Bytelink.link (Compenv.get_objfiles ~with_ocamlparam:true) target;
    Warnings.check_fatal ();
  end


let main argv ppf =
  let log = ref (Location.temporary_log ()) in
  Clflags.add_arguments __LOC__ Options.list;
  Clflags.add_arguments __LOC__
    ["-depend", Arg.Unit Makedepend.main_from_option,
     "<options> Compute dependencies (use 'ocamlc -depend -help' for details)"];
  let exception Continue in
  let exit_number =
    match process (fun () -> raise Continue) argv log ppf with
    | exception (Compenv.Exit_with_status n) ->
        n
    | exception Continue
    | _ ->
        Compmisc.with_ppf_dump ~file_prefix:"profile"
          (fun ppf -> Profile.print ppf !Clflags.profile_columns);
        0
    | exception x ->
        Location.log_exception !log x;
        2
  in
  Log.flush !log; exit_number
