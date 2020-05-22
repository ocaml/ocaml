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
    out_channel -> Environments.t -> 'a

  let map f a log env =
    f (a log env)

  let apply f a log env =
    let a = a log env in
    let f = f log env in
    f a

  let return x _ _ = x

  let select f a log env =
    match f log env with
    | Ok x ->
        let a = a log env in
        a x
    | Error b ->
        b

  open Ocamltest_stdlib

  let test_build_directory env =
    Environments.safe_lookup Builtin_variables.test_build_directory env

  let run_cmd
      ?environment
      ?(stdin_variable=Builtin_variables.stdin)
      ?(stdout_variable=Builtin_variables.stdout)
      ?(stderr_variable=Builtin_variables.stderr)
      ?(append=false)
      ?(timeout=0)
      log env original_cmd
    =
    let environment =
      match environment with
      | None -> [||]
      | Some e -> e log env
    in
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

  let run_cmd ?environment ?stdin_variable ?stdout_variable ?stderr_variable ?append
      cmdline log env =
    let cmdline = cmdline log env in
    run_cmd
      ?environment
      ?stdin_variable
      ?stdout_variable
      ?stderr_variable
      ?append log env cmdline

  let setup_symlinks source_dir build_dir files log env =
    let source_dir = source_dir log env in
    let build_dir = build_dir log env in
    let files = files log env in
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
    Sys.chdir build_dir

  let safe_lookup var _ env =
    Environments.safe_lookup var env

  let lookup var _ env =
    Environments.lookup var env

  let lookup_nonempty var _ env =
    Environments.lookup_nonempty var env

  let lookup_as_bool var _ env =
    Environments.lookup_as_bool var env

  let pair a b log env =
    let a = a log env in
    let b = b log env in
    (a, b)

  let force_remove s log env =
    let s = s log env in
    Sys.force_remove s

  let progn a b log env =
    let () = a log env in
    b log env

  let add v s x log env =
    let s = s log env in
    x log (Environments.add v s env)

  let add_if_undefined v s x log env =
    let s = s log env in
    x log (Environments.add_if_undefined v s env)

  let with_env x log env =
    let x = x log env in
    x, env

  let if_ c a b log env =
    if c log env then
      a log env
    else
      b log env

  let concatmap f l log env =
    let l = l log env in
    let rec loop acc = function
      | [] -> List.rev acc
      | x :: l ->
          let xl = f x log env in
          loop (xl :: acc) l
    in
    let l = loop [] l in
    List.flatten l

  let while_ f x l log env =
    let l = l log env in
    let rec loop res = function
      | [] -> Ok res
      | x :: l ->
          begin match f x log env with
          | Ok x ->
              loop x l
          | Error _ as r ->
              r
          end
    in
    loop x l

  let file_exists s log env =
    let s = s log env in
    Sys.file_exists s

  let system_env _ env =
    Environments.to_system_env env

  let apply_modifiers modifiers x log env =
    x log (Environments.apply_modifiers env modifiers)

  let branch f a b =
    select (apply (map Stdlib.Result.map_error b) f) a

  let cast x log env = x log env

  module Infix = struct
    let (let+) a f = map f a
    let (and+) a b = pair a b
    let (||+) a b = if_ a (return true) b
    let (&&+) a b = if_ a b (return false)
  end
end
