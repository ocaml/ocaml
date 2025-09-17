(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Harness.Import

type t = {
  (* Actual process environment to pass to Unix.create_process *)
  environment: string array;
  (* Displayed environment *)
  additional_bindings: (string * string) list;
    (* Set for testing (e.g. "PATH=.:$PATH") *)
  shim_bindings: (string * string) list;
    (* Set for shimming (e.g. "OCAMLLIB=...") *)
  (* Serial for this environment from {!make} *)
  serial: int;
  (* Various paths *)
  test_root: string;
  test_root_logical: string option;
  prefix: string;
  bindir_suffix: string;
  libdir_suffix: string;
  (* Meta-data *)
  phase: phase;
  pp_path: Format.formatter -> string -> unit;
  verbose: bool;
}

(* Projections *)
let pp_path {pp_path; _} = pp_path
let verbose {verbose; _} = verbose
let test_root {test_root; _} = test_root
let test_root_logical {test_root_logical; _} = test_root_logical
let prefix {prefix; _} = prefix
let libdir_suffix {libdir_suffix; _} = libdir_suffix

(* Derived properties *)

let is_renamed {phase; _} = (phase = Renamed)

let bindir {prefix; bindir_suffix; _} =
  Filename.concat prefix bindir_suffix

let libdir {prefix; libdir_suffix; _} =
  Filename.concat prefix libdir_suffix

let tool_path env mode bytecode native =
  let tool = Harness.exe (if mode = Bytecode then bytecode else native) in
  Filename.concat (bindir env) tool

let ocamlrun env =
  Filename.concat (bindir env) (Harness.exe "ocamlrun")

let in_libdir env path =
  Filename.concat (libdir env) path

let in_test_root {test_root; _} path =
  Filename.concat test_root path

(* Reverse the quoting of single quotes done by Filename.quote on Unix (which is
   used for the runtime name when embedded in sh-scripts. Any single quote
   characters are transformed to "'\\''". If the string is split on the single
   quote characters, the sequence ["\\"; ""] is a single quote character in the
   unescaped version. *)
let dequote s =
  let[@tail_mod_cons] rec loop = function
  | "\\" :: "" :: rest -> "'" :: loop rest
  | chunk :: rest -> chunk :: loop rest
  | [] -> []
  in
  String.concat "" (loop (String.split_on_char '\'' s))

(* [classify_executable file] determines if [file] is :
   - Tendered bytecode with an executable header
   - Scripted bytecode invoking ocamlrun with a #! header
   - Custom bytecode (produced with ocamlc -custom)
   - Vanilla executables (vanilla ocamlopt or any of the caml_startup mechanisms
     via -output-obj, -output-complete-exe, etc.). The actual OCaml program may
     be bytecode (but it will have been embedded in a C object). *)
let classify_executable file =
  try
    In_channel.with_open_bin file (fun ic ->
      let start = really_input_string ic 2 in
      let is_RNTM = function
      | Bytesections.{name = Name.RNTM; _} -> true
      | _ -> false
      in
      let is_DLLS = function
      | Bytesections.{name = Name.DLLS; len} when len > 0 -> true
      | _ -> false
      in
      let toc = Bytesections.read_toc ic in
      let sections = Bytesections.all toc in
      if start = "#!" then
        let runtime =
          seek_in ic 2;
          let shebang = String.trim (input_line ic) in
          if Filename.basename shebang = "sh" then
            let exec_line = input_line ic in
            if String.starts_with ~prefix:"exec '" exec_line
               && String.ends_with ~suffix:"' \"$0\" \"$@\"" exec_line then
              (* When the path to the runtime can't be directly used in a
                 shebang, the shell is used instead, the next line is then:
                   exec '<runtime>' "$0" "$@" *)
              dequote (String.sub exec_line 6 (String.length exec_line - 17))
            else
              Harness.fail_because "%s contains an unexpected exec line: %S"
                                   file exec_line
          else
            shebang
        in
        Tendered {header = Header_shebang;
                  dlls = List.exists is_DLLS sections;
                  runtime}
      else if List.exists is_RNTM sections then
        let rntm =
          Bytesections.read_section_string toc ic Bytesections.Name.RNTM in
        let len = String.length rntm in
        if len = 0 || rntm.[len - 1] <> '\000' then
          Harness.fail_because "%s contains corrupt RNTM: %S" file rntm;
        let runtime = String.sub rntm 0 (len - 1) in
        Tendered {header = Header_exe;
                  dlls = List.exists is_DLLS sections;
                  runtime}
      else
        Custom)
  with End_of_file | Bytesections.Bad_magic_number ->
    Vanilla

let is_shebang program =
  if Filename.is_relative program then
    false
  else
    match classify_executable program with
    | Tendered {header = Header_shebang; _} -> true
    | _ -> false

let launched_via_stub program =
  match classify_executable program with
  | Tendered {header = Header_exe; _} -> true
  | _ -> false

module StringSet = Set.Make(String)

let ld_library_path_name =
  if Config.system = "macosx" then
    "DYLD_LIBRARY_PATH"
  else
    "LD_LIBRARY_PATH"

(* The basic process environment. This is Unix.environment with various
   OCaml-specific variables removed (to create a "pristine" test environment).
   On non-Windows platforms, ensure that ld_library_path_name appears in this
   list by adding an empty binding if one isn't already present. *)
let base_bindings =
  (* List of environment variables to remove from the calling environment *)
  let scrub =
    let names = [
      "BUILD_PATH_PREFIX_MAP";
      "CAMLLIB";
      "CAMLRUNPARAM";
      "CAML_LD_LIBRARY_PATH";
      "OCAMLLIB";
      "OCAMLPARAM";
      "OCAMLRUNPARAM";
      "OCAMLTOP_INCLUDE_PATH";
      "OCAML_RUNTIME_EVENTS_DIR";
      "OCAML_RUNTIME_EVENTS_PRESERVE";
      "OCAML_RUNTIME_EVENTS_START";
    ] in
    let names =
      if Sys.win32 then ld_library_path_name::names else names in
    StringSet.of_list names
  in
  let keep s =
    not (StringSet.mem (String.sub s 0 (String.index s '=')) scrub)
  in
  let bindings = List.filter keep (Array.to_list (Unix.environment ())) in
  let has_ld_library_path_binding =
    let prefix = ld_library_path_name ^ "=" in
    List.exists (String.starts_with ~prefix)
  in
  if Sys.win32 || has_ld_library_path_binding bindings then
    bindings
  else
    (ld_library_path_name ^ "=") :: bindings

(* Tests whether the name of an environment variable is in fact PATH, masking
   the fact that environment variable names are case-insensitive on
   Windows. *)
let is_path_env =
  if Sys.win32 then
    fun name -> String.lowercase_ascii name = "path"
  else
    String.equal "PATH"

(* For displaying, only display the updated environment when it changes. This
   hash is used to provide the serial property for each environment which is
   tracked in the display logic (see last_environment below). *)
let environments = Hashtbl.create 15

(* Returns an environment where any variables in scrub have been removed and
   with effectively PATH=$bindir:$PATH and
   LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH on Unix or
   DYLD_LIBRARY_PATH=$libdir$:DYLD_LIBRARY_PATH on macOS or
   PATH=$bindir;$libdir;$PATH on Windows. *)
let make pp_path ~verbose ~test_root ~test_root_logical
         ~phase ~prefix ~bindir_suffix ~libdir_suffix =
  let bindir = Filename.concat prefix bindir_suffix in
  let libdir = Filename.concat prefix libdir_suffix in
  let update binding =
    let equals = String.index binding '=' in
    let name = String.sub binding 0 equals in
    let value =
      String.sub binding (equals + 1) (String.length binding - equals - 1)
    in
    if is_path_env name then
      if Sys.win32 then
        if String.index_opt bindir ';' <> None then
          Printf.sprintf "%s=\"%s\";%s" name bindir value
        else
          Printf.sprintf "%s=%s;%s" name bindir value
      else
        Printf.sprintf "%s=%s:%s" name bindir value
    else if name = ld_library_path_name then
      Printf.sprintf "%s=%s:%s" name libdir value
    else
      binding
  in
  let bindings = List.map update base_bindings in
  let serial =
    try Hashtbl.find environments bindings
    with Not_found ->
      let serial = Hashtbl.length environments + 1 in
      Hashtbl.add environments bindings serial;
      serial
  in
  let environment = Array.of_list bindings in
  {environment; additional_bindings = []; shim_bindings = []; serial;
   test_root; test_root_logical; prefix; bindir_suffix; libdir_suffix;
   phase; pp_path; verbose}

(* Last environment to be summarised on the console *)
let last_environment = ref (-1)

(* Display a line of output from a process on the console *)
let format_line () = Format.printf "@{<inline_code>>@} %s\n%!"

let string_of_process_status = function
| Unix.WEXITED n -> "exit " ^ string_of_int n
| Unix.WSIGNALED n -> Sys.signal_to_string n
| Unix.WSTOPPED n -> "stopped with " ^ Sys.signal_to_string n

(* Display the details of an executed command on the console. level controls the
   style (unexpected outcome; expected failure; normal). If the environment is
   different from the last command which is displayed, then the environment
   variable changes are summarised after the command. In verbose mode, the PID
   of the command is displayed. Shimmed parts of the command (environment
   variable tweaks or passing the command to ocamlrun directly) are
   highlighted. If argv0 is specified, then the original program executable is
   also shown. *)
let display_execution level status pid ~runtime program argv0 args
                      ({pp_path; verbose; serial; _} as env) =
  let pp_program style program f = function
  | Some argv0 ->
      Format.fprintf f "@{<%s>%s (from %a)@}"
                       style argv0 pp_path program
  | None ->
      Format.fprintf f "@{<%s>%a@}" style pp_path program
  in
  let pp_arg f x = Format.pp_print_char f ' '; pp_path f x in
  let pp_args = Format.pp_print_list ~pp_sep:(Fun.const ignore) pp_arg in
  let pp_status ~exited_normally style f status =
    if not exited_normally then
      Format.fprintf f " <@{<%s>%s@}>" style (string_of_process_status status)
  in
  let pp_env f {additional_bindings; shim_bindings; _} =
    let pp_binding fmt f (k, v) = Format.fprintf f fmt k pp_path v in
    List.iter (pp_binding "%s=%a " f) additional_bindings;
    List.iter (pp_binding "@{<warning>%s=%a@} " f) shim_bindings
  in
  let pp_pid f = function
  | Some pid when verbose -> Format.fprintf f " [@{<loc>%d@}]" pid
  | _ -> ()
  in
  let style_of_level = function
  | `Normal -> "inline_code"
  | `Warning -> "warning"
  | `Error -> "error"
  in
  let program_style =
    let level = if runtime then `Warning else level in
    style_of_level level
  in
  let style = style_of_level level in
  let exited_normally = (level = `Normal && status = Unix.WEXITED 0) in
  Format.printf "@{<%s>%a@}%a@{<%s>%a@}%a%a\n@?"
                style pp_env env
                (pp_program program_style program) argv0
                style pp_args args
                pp_pid pid
                (pp_status ~exited_normally style) status;
  if serial <> !last_environment then begin
    last_environment := serial;
    Format.printf "\
      @{<inline_code>> @}@{<loc>Environment@}\n\
      @{<inline_code>> @}  @{<loc>PATH=%a:$PATH@}\n"
      pp_path (bindir env);
    if not Sys.win32 then
      Format.printf "\
        @{<inline_code>> @}  @{<loc>%s=%a:$%s@}\n"
      ld_library_path_name pp_path (libdir env)
      ld_library_path_name
  end

(* Executes a single command, returning the exit code and lines of output *)
let run_one (~runtime, ~quiet, ~fails, ~program, ~argv0, ~args,
             ~env:({environment; verbose; _} as env)) =
  flush stderr;
  flush stdout;
  let quiet = quiet && not verbose in
  let captured_output = "process-output" in
  let stdout, stderr =
    let flags = Unix.([O_RDWR; O_CREAT; O_TRUNC; O_CLOEXEC]) in
    let fd = Unix.openfile captured_output flags 0o600 in
    fd, fd
  in
  let pid =
    let argv0 = Option.value ~default:program argv0 in
    try
      let pid =
        Unix.create_process_env program (Array.of_list (argv0::args))
                                environment Unix.stdin stdout stderr
      in
      Some pid
    with
    | Unix.(Unix_error(ENOENT, "create_process", _))
      when is_shebang program -> None
  in
  let _, status =
    Option.map (Unix.waitpid []) pid
    |> Option.value ~default:(-1, Unix.WEXITED 127)
  in
  let status =
    match status with
    | Unix.WSIGNALED n
      when n = Sys.sigabrt ->
        (* Convert SIGABRT to exit code 134 *)
        Unix.WEXITED 134
    | Unix.WSIGNALED n
      when n = Sys.sigsegv
           && List.mem Config.architecture ["s390x"; "riscv"] ->
        (* cf. ocaml/ocaml#13693 - s390x executables might segfault, so this
           gets converted to Docker's exit code so it can be skipped *)
        Unix.WEXITED 139
    | status ->
        status
  in
  let level, exit_code =
    match status with
    | Unix.WEXITED n
      when fails = (n <> 0) || status = Unix.WEXITED 139 ->
        let level =
          if n = 0 then
            `Normal
          else
            `Warning
        in
        level, n
    | _ ->
        let display_argv0 =
          match argv0 with
          | Some argv0 -> Printf.sprintf "%s (from %s)" argv0 program
          | None -> program
        in
        display_execution `Error status pid ~runtime program argv0 args env;
        let _ = Unix.lseek stdout 0 Unix.SEEK_SET in
        In_channel.fold_lines format_line () (Unix.in_channel_of_descr stdout);
        Harness.fail_because "%s did not terminate as expected (got %s)"
                             display_argv0 (string_of_process_status status)
  in
  if not quiet then
    display_execution level status pid ~runtime program argv0 args env;
  let _ = Unix.lseek stdout 0 Unix.SEEK_SET in
  let lines =
    let ic = Unix.in_channel_of_descr stdout in
    (* Some of the tests send lines of text which end with '\r'. On native
       Windows, this will _correctly_ cause "\r\r\n" to be be sent down
       the pipe and text mode will _correctly_ translate that to "\r\n"
       (and the caller receives a line ending with '\r').
       On Cygwin, where the process sending the text is a Unix process,
       the same text ending '\r' is just sent with "\r\n" which definitely
       does not want to be translated to just '\n'. Other Unix systems do
       not differentiate text and binary mode anyway, so the distinction
       is moot. *)
    In_channel.set_binary_mode ic Sys.cygwin;
    In_channel.input_lines ic
  in
  Unix.close stdout;
  Sys.remove captured_output;
  exit_code, lines

(* [apply_shims ~stubs ~stdlib env] augments [env] with the required environment
   bindings for [~stubs] (CAML_LD_LIBRARY_PATH set to the stublibs sub-directory
   of libdir) and [~stdlib] (OCAMLLIB set to libdir) *)
let apply_shims ~stubs ~stdlib ({environment; shim_bindings; _} as env) =
  let shim_bindings =
    if stdlib then
      ("OCAMLLIB", libdir env) :: shim_bindings
    else
      shim_bindings in
  let shim_bindings =
    if stubs then
      ("CAML_LD_LIBRARY_PATH", in_libdir env "stublibs") :: shim_bindings
    else
      shim_bindings in
  let environment =
    let shim_bindings =
      List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) shim_bindings in
    Array.append (Array.of_list shim_bindings) environment in
  {env with environment; shim_bindings}

(* Prefix the PATH with an entry for the current directory (i.e. prepend ".:").
   Since this is Unix-only, we don't have to worry about separators, case,
   etc.) *)
let add_cwd_to_path ({environment; additional_bindings; _} as env) =
  let update_path s =
    let l = String.length s in
    if l < 5 || not (String.starts_with ~prefix:"PATH=" s) then
      s
    else
      "PATH=.:" ^ String.sub s 5 (l - 5)
  in
  {env with environment = Array.map update_path environment;
            additional_bindings = ("PATH", ".:$PATH") :: additional_bindings}

let run_process ?(runtime = false) ?(stubs = false) ?(stdlib = false)
                ?(prefix_path_with_cwd = Sys.win32) ?(quiet = false)
                ?(fails = false) ({phase; _} as env) program ?argv0 args =
  (* Process ~prefix_path_with_cwd *)
  let env =
    if Sys.win32 then
      (* Windows implicitly searches the current directory, so be sure that it
         is never explicitly disabled on Windows *)
      if not prefix_path_with_cwd then
        invalid_arg "Can't use prefix_path_with_cwd on Windows"
      else
        env
    else
      if prefix_path_with_cwd then
        add_cwd_to_path env
      else
        env
  in
  let ocamlrun = ocamlrun env in
  (* Calculate the overall strategy. This is a non-empty list of environments to
     be tried. Each individual stratagem controls whether the program should be
     executed via ocamlrun and if an augmented environment should be used. *)
  let strategy =
    let shim ?(runtime = runtime) ?(stubs = stubs) ?(stdlib = stdlib)
             ?(fails = true) ?(quiet = true) env =
      (* The tests are easier to write with the assumption that shims are
         simply ignored in the Original phase (otherwise they all begin
         [Env.is_renamed env && (* ... *)] *)
      let runtime = runtime && phase = Renamed in
      let env =
        if phase = Renamed && (stubs || stdlib) then
          apply_shims ~stubs ~stdlib env
        else
          env
      in
      let program, argv0, args =
        if runtime then
          ocamlrun, None, program::args
        else
          program, argv0, args
      in
      ~runtime, ~quiet, ~fails, ~program, ~argv0, ~args, ~env
    in
    (* In order to ensure that bugs are not silently fixed (or, more to the
       point, that a shim isn't left enabled and so masks something different),
       ensure that each shim is necessary by checking that the execution still
       fails without each shim in turn. The final entry in the strategy must be
       the request itself. *)
    let test_without cond shim strategy =
      if phase = Renamed && cond then
        shim env :: strategy
      else
        strategy
    in
    (* Request *)
    [shim ~fails ~quiet env]
    (* If more than one shim is enabled, test with each of disabled *)
    |> test_without (stdlib && (runtime || stubs)) (shim ~stdlib:false)
    |> test_without (stubs && (runtime || stdlib)) (shim ~stubs:false)
    |> test_without (runtime && (stubs || stdlib)) (shim ~runtime:false)
    (* Finally, test with none of the shims enabled *)
    |> test_without (runtime || stubs || stdlib)
                    (shim ~runtime:false ~stubs:false ~stdlib:false)
  in
  List.fold_left (Fun.const run_one) (-1, []) strategy

(* Augments an environment with patches for CAML_LD_LIBRARY_PATH, OCAMLLIB and
   CAMLLIB and then calls run_process *)
let run_process_with_test_env
      ?runtime ~caml_ld_library_path ~ocamllib ~camllib ?quiet ?fails
      ({environment; _} as env) program =
  let add_binding f v name bindings =
    match v with
    | Some value ->
        (name, f value) :: bindings
    | None ->
        bindings
  in
  let to_path = String.concat (if Sys.win32 then ";" else ":") in
  let additional_bindings =
    add_binding to_path caml_ld_library_path "CAML_LD_LIBRARY_PATH" []
    |> add_binding Fun.id ocamllib "OCAMLLIB"
    |> add_binding Fun.id camllib "CAMLLIB"
  in
  let environment =
    let additional_bindings =
      List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) additional_bindings in
    Array.append (Array.of_list additional_bindings) environment
  in
  let env = {env with environment; additional_bindings} in
  run_process ?runtime ?quiet ?fails env program

let display_output output =
  List.iter (format_line ()) output

let read_content file ic =
  let len = in_channel_length ic in
  let content = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout len in
  if In_channel.really_input_bigarray ic content 0 len = None then
    Harness.fail_because "Error reading %s" file;
  content, len

let output_compunit ic oc (compunit : Cmo_format.compilation_unit) =
  seek_in ic compunit.cu_pos;
  Misc.copy_file_chunk ic oc compunit.cu_codesize;
  if compunit.cu_debug > 0 then begin
    seek_in ic compunit.cu_debug;
    output_value oc (Compression.input_value ic);
    output_value oc (Compression.input_value ic);
  end;
  output_value oc compunit

let with_decompressed_ocaml_artefact ic file f =
  let magic = Cmt_format.read_magic_number ic in
  let temp_file, oc =
    Filename.open_temp_file ~mode:[Open_binary] "ocaml-artefact-" ".tmp" in
  let () =
    if magic = Config.cmi_magic_number || magic = Config.cmt_magic_number then
      output_value oc (Cmt_format.read file)
    else if magic = Config.cmo_magic_number then begin
      seek_in ic (input_binary_int ic);
      let compunit = (input_value ic : Cmo_format.compilation_unit) in
      output_compunit ic oc compunit
    end else if magic = Config.cma_magic_number then begin
      seek_in ic (input_binary_int ic);
      let toc = (input_value ic : Cmo_format.library) in
      List.iter (output_compunit ic oc) toc.lib_units;
      output_value oc toc
    end else
      Harness.fail_because "Unexpected magic number %S in %s" magic file in
  close_out oc;
  let result = In_channel.with_open_bin temp_file (f temp_file) in
  Sys.remove temp_file;
  result

let input_artefact_from_file env file =
  In_channel.with_open_bin file @@ fun ic ->
    match Filename.extension file with
    | ".cma" | ".cmi" | ".cmo" | ".cmti" | ".cmt" ->
        with_decompressed_ocaml_artefact ic file read_content
    | ext when (ext = Config.ext_lib || ext = Config.ext_obj)
               && Sys.os_type = "Unix" && Config.system <> "macosx" ->
        let exit, lines =
          run_process ~quiet:true env "readelf" ["-tS"; file]
        in
        let contains_compressed l =
          if l = "" || l.[0] <> ' ' then
            false
          else
            let test = String.starts_with ~prefix:"COMPRESSED" in
            let l = String.split_on_char ' ' l in
            List.exists test l in
        if exit <> 0 then
          Harness.fail_because "readelf failed"
        else if List.exists contains_compressed lines then
          let temp_file = Filename.temp_file "ocaml-artefact-" ".tmp" in
          let exit, _ =
            let args = ["--decompress-debug-sections"; file; temp_file] in
            run_process ~quiet:true env "objcopy" args
          in
          if exit = 0 then
            let result =
              In_channel.with_open_bin temp_file (read_content temp_file) in
            Sys.remove temp_file;
            result
          else begin
            Sys.remove temp_file;
            Harness.fail_because "objcopy failed"
          end
        else
          read_content file ic
    | _ ->
        read_content file ic
