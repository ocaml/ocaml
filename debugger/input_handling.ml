(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**************************** Input control ****************************)

open Unix
open Primitives

(*** Actives files. ***)

(* List of the actives files. *)
let active_files =
  ref ([] : (file_descr * ((io_channel -> unit) * io_channel)) list)

(* Add a file to the list of actives files. *)
let add_file file controller =
  active_files := (file.io_fd, (controller, file))::!active_files

(* Remove a file from the list of actives files. *)
let remove_file file =
  active_files := List.remove_assoc file.io_fd !active_files

(* Change the controller for the given file. *)
let change_controller file controller =
  remove_file file; add_file file controller

(* Return the controller currently attached to the given file. *)
let current_controller file =
  fst (List.assoc file.io_fd !active_files)

(* Execute a function with `controller' attached to `file'. *)
(* ### controller file funct *)
let execute_with_other_controller controller file funct =
  let old_controller = current_controller file in
    change_controller file controller;
    let finally () = change_controller file old_controller in
    Fun.protect ~finally funct

(*** The "Main Loop" ***)

let continue_main_loop =
  ref true

let exit_main_loop _ =
  continue_main_loop := false

(* Handle active files until `continue_main_loop' is false. *)
let main_loop () =
  let finally =
    let old_state = !continue_main_loop in
    fun () -> continue_main_loop := old_state
  in
    Fun.protect ~finally @@ fun () ->
      continue_main_loop := true;
      while !continue_main_loop do
        try
          let (input, _, _) =
            select (List.map fst !active_files) [] [] (-1.)
          in
            List.iter
              (function fd ->
                 let (funct, iochan) = (List.assoc fd !active_files) in
                   funct iochan)
              input
        with
          Unix_error (EINTR, _, _) -> ()
      done

(*** Managing user inputs ***)

(* Are we in interactive mode ? *)
let interactif = ref true

let current_prompt = ref ""

(* Where the user input come from. *)
let user_channel = ref std_io

(* Character-mode callback support *)
type char_callback = char -> bool
let char_mode_callback : char_callback option ref = ref None

(* Controller for character-mode input *)
let char_mode_controller iochan =
  let buf = Bytes.create 1 in
  let n = input iochan.io_in buf 0 1 in
  let c =
    if n = 0 then
      '\004'  (* EOF - send Ctrl+D to trigger proper EOF handling *)
    else
      Bytes.get buf 0
  in
  match !char_mode_callback with
  | None ->
      (* Shouldn't happen, but fallback to normal behavior *)
      exit_main_loop ()
  | Some callback ->
      let should_continue = callback c in
      if not should_continue then
        exit_main_loop ()

let read_user_input buffer length =
  main_loop ();
  input !user_channel.io_in buffer 0 length

(* Stop reading user input. *)
let stop_user_input () =
  remove_file !user_channel

(* Resume reading user input. *)
let resume_user_input () =
  if not (List.mem_assoc !user_channel.io_fd !active_files) then begin
    if !interactif && !Parameters.prompt && !char_mode_callback = None then begin
      (* Only print prompt in line mode, not char mode *)
      print_string !current_prompt;
      flush Stdlib.stdout
      end;
    (* Choose controller based on mode *)
    let controller =
      match !char_mode_callback with
      | None -> exit_main_loop  (* Line mode *)
      | Some _ -> char_mode_controller  (* Character mode *)
    in
    add_file !user_channel controller
    end

(* Set character-mode callback *)
let set_char_mode_callback callback =
  char_mode_callback := callback;
  (* If already active, update the controller *)
  if List.mem_assoc !user_channel.io_fd !active_files then begin
    let controller =
      match callback with
      | None -> exit_main_loop
      | Some _ -> char_mode_controller
    in
    change_controller !user_channel controller
  end
