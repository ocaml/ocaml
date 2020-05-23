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

  type t = out_channel -> Result.t

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

  let run_cmd
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
        (* reason; *)
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

  let setup_symlinks source_dir build_dir files _ =
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

  let force_remove s _ =
    Sys.force_remove s;
    Result.pass

  let cd cwd _ =
    try
      Sys.chdir cwd; Result.pass
    with _ ->
      let reason = "Could not chidir to \"" ^ cwd ^ "\"" in
      Result.fail_with_reason reason

  let check_files ~kind_of_output ~promote ignore
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

  let compare_files ~tool files _ =
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

  let seq l log =
    let rec loop = function
      | [] -> Result.pass
      | x :: l ->
          let r = x log in
          if Result.is_pass r then
            loop l
          else
            r
    in
    loop l

  let if_pass a b log =
    let r = a log in
    if Result.is_pass r then
      b log
    else
      r

  let echo fmt =
    Printf.ksprintf (fun s log -> Printf.fprintf log "%s\n%!" s; Result.pass) fmt

  let of_result r _ =
    r
end

type code = Environments.t -> Eff.t * Environments.t

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
  let eff, env = code env in
  eff log, env

module ActionSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)

let _ = Variables.register_variable action_name

module A = struct
  type 'a t = Environments.t -> 'a

  let map f a env =
    f (a env)

  let return x _ = x

  let safe_lookup var env =
    Environments.safe_lookup var env

  let lookup var env =
    Environments.lookup var env

  let lookup_nonempty var env =
    Environments.lookup_nonempty var env

  let lookup_as_bool var env =
    Environments.lookup_as_bool var env

  let pair a b env =
    (a env, b env)

  let add v s x env =
    x (Environments.add v (s env) env)

  let add_if_undefined v s x env =
    x (Environments.add_if_undefined v (s env) env)

  let with_env x env =
    x env, env

  let if_ c a b env =
    if c env then
      a env
    else
      b env

  let all l env =
    let rec loop accu = function
      | [] ->
          List.rev accu
      | x :: l ->
          loop (x env :: accu) l
    in
    loop [] l

  let file_exists s env =
    Sys.file_exists (s env)

  let system_env env =
    Environments.to_system_env env

  let apply_modifiers modifiers x env =
    x (Environments.apply_modifiers env modifiers)

  let cast x _ env = x env

  module Infix = struct
    let (let+) a f = map f a
    let (and+) a b = pair a b
    let (||+) a b = if_ a (return true) b
    let (&&+) a b = if_ a b (return false)
  end
end
