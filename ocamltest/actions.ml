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

type code = out_channel -> Environments.t -> Result.t * Environments.t

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
  code log env

module ActionSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)

let _ = Variables.register_variable action_name

module A = struct
  type 'a t =
    out_channel -> Environments.t -> 'a * Environments.t

  let map f a log env =
    let a, env = a log env in
    f a, env

  let return x _ env = x, env

  let if_defined var a b log env =
    match Environments.lookup_nonempty var env with
    | Some _ -> a log env
    | None -> b log env

  open Ocamltest_stdlib

  let test_build_directory env =
    Environments.safe_lookup Builtin_variables.test_build_directory env

  let run_cmd
      ?(environment=[||])
      ?(stdin_variable=Builtin_variables.stdin)
      ?(stdout_variable=Builtin_variables.stdout)
      ?(stderr_variable=Builtin_variables.stderr)
      ?(append=false)
      ?(timeout=0)
      log env original_cmd
    =
    let log_redirection std filename =
      if filename<>"" then
        begin
          Printf.fprintf log "  Redirecting %s to %s \n%!"
            (relative_to_initial_cwd std)
            (relative_to_initial_cwd filename)
        end in
    let cmd =
      if (Environments.lookup_as_bool Strace.strace env) = Some true then
        begin
          let action_name = Environments.safe_lookup action_name env in
          let test_build_directory = test_build_directory env in
          let strace_logfile_name = Strace.get_logfile_name action_name in
          let strace_logfile =
            Filename.make_path [test_build_directory; strace_logfile_name]
          in
          let strace_flags = Environments.safe_lookup Strace.strace_flags env in
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
    let stdin_filename = Environments.safe_lookup stdin_variable env in
    let stdout_filename = Environments.safe_lookup stdout_variable env in
    let stderr_filename = Environments.safe_lookup stderr_variable env in
    log_redirection "stdin" stdin_filename;
    log_redirection "stdout" stdout_filename;
    log_redirection "stderr" stderr_filename;
    let systemenv =
      Array.append
        environment
        (Environments.to_system_env env)
    in
    Run_command.run {
      Run_command.progname = progname;
      Run_command.argv = arguments;
      Run_command.envp = systemenv;
      Run_command.stdin_filename = stdin_filename;
      Run_command.stdout_filename = stdout_filename;
      Run_command.stderr_filename = stderr_filename;
      Run_command.append = append;
      Run_command.timeout = timeout;
      Run_command.log = log
    }

  let run_cmd ~environment ~stdin_variable ~stdout_variable ~stderr_variable ~append
      cmdline log env =
    let n =
      let cmdline, env (* CHECK *) = cmdline log env in
      run_cmd
        ~environment
        ~stdin_variable
        ~stdout_variable
        ~stderr_variable
        ~append log env cmdline
    in
    n, env

  let safe_lookup var _ env =
    Environments.safe_lookup var env, env

  let pair a b log env =
    let a, _env' = a log env in
    let b, env' = b log env in
    (a, b), env' (* CHECK *)

  let (let+) a f = map f a
  let (and+) a b = pair a b
end
