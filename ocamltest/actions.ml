(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of actions, basic blocks for tests *)

open Ocamltest_stdlib

module Eff = struct

  module Result = struct

    (* Definition of test-result related types and functions *)

    type status = Pass | Skip | Fail

    type t =
      {
        status : status;
        reason : string option
      }

    let result_of_status s = { status = s; reason = None }
    let pass = result_of_status Pass
    let skip = result_of_status Skip
    let fail = result_of_status Fail
    let result_with_reason s r = { status = s; reason = Some r }
    let pass_with_reason r = result_with_reason Pass r
    let skip_with_reason r = result_with_reason Skip r
    let fail_with_reason r = result_with_reason Fail r

    let string_of_status = function
      | Pass -> "passed"
      | Skip -> "skipped"
      | Fail -> "failed"

    let string_of_reason = function
      | None -> ""
      | Some reason -> (" (" ^ reason ^ ")")

    let string_of_result r =
      (string_of_status r.status) ^ (string_of_reason r.reason)

    let is_pass r = r.status = Pass
    let is_skip r = r.status = Skip
    let is_fail r = r.status = Fail
  end

  type run_params =
    {
      environment: string array;
      stdin_filename: string;
      stdout_filename: string;
      stderr_filename: string;
      append: bool;
      timeout: int;
      strace: bool;
      strace_logfile: string;
      strace_flags: string;
      expected_exit_codes: int list;
      skip_exit_codes: int list;
    }

  type t =
    | Run_cmd of run_params * string list
    | Setup_symlinks of string * string * string list
    | Force_remove of string
    | Chdir of string
    | Check_files of
        {
          kind_of_output: string;
          promote: bool option;
          ignore: Filecompare.ignore;
          files: Filecompare.files;
        }
    | Compare_files of
        {
          tool: Filecompare.tool;
          files: Filecompare.files;
        }
    | Seq of t list
    | Echo of string
    | Pure of Result.t
    | If_pass of t * t

  let run_cmd params commandline = Run_cmd (params, commandline)
  let setup_symlinks source_dir build_dir files =
    Setup_symlinks (source_dir, build_dir, files)
  let chdir s = Chdir s
  let check_files ~kind_of_output ~promote ignore files =
    Check_files {kind_of_output; promote; ignore; files}
  let compare_files ~tool files = Compare_files {tool; files}
  let seq l = Seq l
  let force_remove s = Force_remove s
  let if_pass a b = If_pass (a, b)
  let echo fmt = Printf.ksprintf (fun s -> Echo s) fmt
  let pass = Pure Result.pass
  let skip = Pure Result.skip
  let fail = Pure Result.fail
  let pass_with_reason s = Pure (Result.pass_with_reason s)
  let skip_with_reason s = Pure (Result.skip_with_reason s)
  let fail_with_reason s = Pure (Result.fail_with_reason s)

  let run_cmd_run
      {
        environment;
        stdin_filename;
        stdout_filename;
        stderr_filename;
        append;
        timeout;
        strace;
        strace_flags;
        strace_logfile;
        expected_exit_codes;
        skip_exit_codes;
      }
      original_cmd log
    =
    let log_redirection std filename =
      if filename<>"" then
        begin
          Printf.fprintf log "  Redirecting %s to %s \n%!"
            (relative_to_initial_cwd std)
            (relative_to_initial_cwd filename)
        end in
    let cmd =
      if strace then
        begin
          let strace_cmd =
            ["strace"; "-f"; "-o"; strace_logfile; strace_flags]
          in
          strace_cmd @ original_cmd
        end else original_cmd
    in
    let lst = List.concat (List.map String.words cmd) in
    let quoted_lst =
      if Sys.os_type="Win32"
      then List.map Filename.maybe_quote lst
      else lst in
    let cmd' = String.concat " " quoted_lst in
    Printf.fprintf log "Commandline: %s\n" cmd';
    let progname = List.hd quoted_lst in
    let arguments = Array.of_list quoted_lst in
    log_redirection "stdin" stdin_filename;
    log_redirection "stdout" stdout_filename;
    log_redirection "stderr" stderr_filename;
    let n =
      Run_command.run {
        Run_command.progname = progname;
        Run_command.argv = arguments;
        Run_command.envp = environment;
        Run_command.stdin_filename = stdin_filename;
        Run_command.stdout_filename = stdout_filename;
        Run_command.stderr_filename = stderr_filename;
        Run_command.append = append;
        Run_command.timeout = timeout;
        Run_command.log;
      }
    in
    if List.mem n expected_exit_codes then
      Result.pass
    else if List.mem n skip_exit_codes then
      (* Result.skip_with_reason reason *)
      Result.skip
    else
      (* Result.fail_with_reason reason *)
      Result.fail

  let setup_symlinks_run source_dir build_dir files _ =
    let symlink filename =
      let src = Filename.concat source_dir filename in
      let cmd = "ln -sf " ^ src ^" " ^ build_dir in
      Sys.run_system_command cmd
    in
    let copy filename =
      let src = Filename.concat source_dir filename in
      let dst = Filename.concat build_dir filename in
      Sys.copy_file src dst
    in
    let f = if Sys.win32 then copy else symlink in
    Sys.make_directory build_dir;
    List.iter f files;
    Sys.chdir build_dir;
    Result.pass

  let check_files_run ~kind_of_output ~promote ignore
      ({ Filecompare.reference_filename;
         Filecompare.output_filename; _ } as files)
      log
    =
    let tool = Filecompare.make_cmp_tool ~ignore in
    match Filecompare.check_file ~tool files with
    | Filecompare.Same -> Result.pass
    | Filecompare.Different ->
        let diff = Filecompare.diff files in
        let diffstr = match diff with
          | Ok difference -> difference
          | Error diff_file -> ("See " ^ diff_file) in
        let reason =
          Printf.sprintf "%s output %s differs from reference %s: \n%s\n"
            kind_of_output files.Filecompare.output_filename reference_filename diffstr in
        if promote = Some true
        then begin
          Printf.fprintf log "Promoting %s output %s to reference %s\n%!"
            kind_of_output output_filename reference_filename;
          Filecompare.promote files ignore
        end;
        Result.fail_with_reason reason
    | Filecompare.Unexpected_output ->
        let banner = String.make 40 '=' in
        let unexpected_output = Sys.string_of_file output_filename in
        let unexpected_output_with_banners = Printf.sprintf
            "%s\n%s%s\n" banner unexpected_output banner in
        let reason = Printf.sprintf
            "The file %s was expected to be empty because there is no \
             reference file %s but it is not:\n%s\n"
            output_filename reference_filename unexpected_output_with_banners in
        Result.fail_with_reason reason
    | Filecompare.Error (commandline, exitcode) ->
        let reason = Printf.sprintf "The command %s failed with status %d"
            commandline exitcode in
        Result.fail_with_reason reason

  let compare_files_run ~tool files _ =
    match Filecompare.compare_files ~tool files with
    | Filecompare.Same ->
        Result.pass
    | Filecompare.Different ->
        let reason =
          Printf.sprintf "Files %s and %s are different"
            files.Filecompare.reference_filename
            files.Filecompare.output_filename
        in
        Result.fail_with_reason reason
    | Filecompare.Unexpected_output ->
        assert false
    | Filecompare.Error (_commandline, _exitcode) ->
        (* let reason = Actions_helpers.mkreason what commandline exitcode in *)
        Result.fail_with_reason ""

  let rec run x log =
    match x with
    | Run_cmd (run_params, commandline) ->
        run_cmd_run run_params commandline log
    | Setup_symlinks (source_dir, build_dir, files) ->
        setup_symlinks_run source_dir build_dir files log
    | Force_remove s ->
        Sys.force_remove s;
        Result.pass
    | Chdir s ->
        begin try
          Sys.chdir s; Result.pass
        with _ ->
          let reason = "Could not chidir to \"" ^ s ^ "\"" in
          Result.fail_with_reason reason
        end
    | Check_files {kind_of_output; promote; ignore; files} ->
        check_files_run ~kind_of_output ~promote ignore files log
    | Compare_files {tool; files} ->
        compare_files_run ~tool files log
    | Seq l ->
        let rec loop = function
          | [] -> Result.pass
          | x :: l ->
              let r = run x log in
              if Result.is_pass r then
                loop l
              else
                r
        in
        loop l
    | Echo s ->
        Printf.fprintf log "%s\n%!" s;
        Result.pass
    | Pure r ->
        r
    | If_pass (a, b) ->
        let r = run a log in
        if Result.is_pass r then
          run b log
        else
          r
end

module A = struct

  type 'a t =
    | Pure : 'a -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t
    | Safe_lookup : Variables.t -> string t
    | Lookup : Variables.t -> string option t
    | Lookup_nonempty : Variables.t -> string option t
    | Lookup_bool : Variables.t -> bool option t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Add : Variables.t * string t * 'a t -> 'a t
    | Add_if_undefined : Variables.t * string t * 'a t -> 'a t
    | Env : Environments.t t
    | If : bool t * 'a t * 'a t -> 'a t
    | File_exists : string t -> bool t
    | Apply_modifiers : Environments.modifiers * 'a t -> 'a t
    | All : 'a t list -> 'a list t

  let return x = Pure x
  let map f a = Map (f, a)
  let file_exists s = File_exists s
  let safe_lookup v = Safe_lookup v
  let lookup v = Lookup v
  let lookup_nonempty v = Lookup_nonempty v
  let lookup_as_bool v = Lookup_bool v
  let both a b = Both (a, b)
  let env = Env
  let if_ c a b = If (c, a, b)
  let apply_modifiers mods x = Apply_modifiers (mods, x)
  let add v s x = Add (v, s, x)
  let add_if_undefined v s x = Add_if_undefined (v, s, x)
  let all xs = All xs

  let rec run : type a. a t -> Environments.t -> a = fun x env ->
    match x with
    | Pure x -> x
    | Map (f, x) -> f (run x env)
    | Safe_lookup v -> Environments.safe_lookup v env
    | Lookup v -> Environments.lookup v env
    | Lookup_nonempty v -> Environments.lookup_nonempty v env
    | Lookup_bool v -> Environments.lookup_as_bool v env
    | Both (a, b) -> (run a env, run b env)
    | Add (v, s, x) -> run x (Environments.add v (run s env) env)
    | Add_if_undefined (v, s, x) ->
        run x (Environments.add_if_undefined v (run s env) env)
    | Env -> env
    | If (a, b, c) -> if run a env then run b env else run c env
    | File_exists a -> Sys.file_exists (run a env)
    | Apply_modifiers (m, x) -> run x (Environments.apply_modifiers env m)
    | All xs -> List.map (fun x -> run x env) xs

  module Infix = struct
    let (let+) a f = map f a
    let (and+) a b = both a b
    let (||+) a b = if_ a (return true) b
    let (&&+) a b = if_ a b (return false)
  end
end


type code = (Eff.t * Environments.t) A.t

type t = {
  name : string;
  body : code;
  mutable hook : code option
}

let name a = a.name

let action_name = Variables.make ("action_name", "Name of the current action")

let make n c = { name = n; body = c; hook = None }

let update action code = { action with body = code }

let compare a1 a2 = String.compare a1.name a2.name

let (actions : (string, t) Hashtbl.t) = Hashtbl.create 10

let register action =
  Hashtbl.add actions action.name action

let get_registered_actions () =
  let f _name action acc = action::acc in
  let unsorted_actions = Hashtbl.fold f actions [] in
  List.sort compare unsorted_actions

let lookup name =
  try Some (Hashtbl.find actions name)
  with Not_found -> None

let set_hook name hook =
  let action = (Hashtbl.find actions name) in
  action.hook <- Some hook

let clear_hook name =
  let action = (Hashtbl.find actions name) in
  action.hook <- None

let clear_all_hooks () =
  let f _name action = action.hook <- None in
  Hashtbl.iter f actions

let run log env action =
  let code = match action.hook with
    | None -> action.body
    | Some code -> code in
  let env = Environments.add action_name action.name env in
  let eff, env = A.run code env in
  Eff.run eff log, env

module ActionSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)

let _ = Variables.register_variable action_name
