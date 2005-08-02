(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Program loading *)

open Unix
open Misc
open Debugger_config
open Parameters
open Input_handling

(*** Debugging. ***)

let debug_loading = ref false

(*** Load a program. ***)

(* Function used for launching the program. *)
let launching_func = ref (function () -> ())

let load_program () =
  !launching_func ();
  main_loop ()

(*** Launching functions. ***)

(* A generic function for launching the program *)
let generic_exec cmdline = function () ->
  if !debug_loading then
    prerr_endline "Launching program...";
  let child =
    try
      fork ()
    with x ->
      Unix_tools.report_error x;
      raise Toplevel in
  match child with
    0 ->
      begin try
         match fork () with
           0 -> (* Try to detach the process from the controlling terminal,
                   so that it does not receive SIGINT on ctrl-C. *)
                begin try ignore(setsid()) with Invalid_argument _ -> () end;
                execv shell [| shell; "-c"; cmdline() |]
         | _ -> exit 0
       with x ->
         Unix_tools.report_error x;
         exit 1
       end
  | _ ->
     match wait () with
       (_, WEXITED 0) -> ()
     | _ -> raise Toplevel

(* Execute the program by calling the runtime explicitely *)
let exec_with_runtime =
  generic_exec
    (function () ->
      Printf.sprintf "CAML_DEBUG_SOCKET=%s %s %s %s"
                     !socket_name
                     (Filename.quote runtime_program)
                     (Filename.quote !program_name)
                     !arguments)

(* Excute the program directly *)
let exec_direct =
  generic_exec
    (function () ->
      Printf.sprintf "CAML_DEBUG_SOCKET=%s %s %s"
                     !socket_name
                     (Filename.quote !program_name)
                     !arguments)

(* Ask the user. *)
let exec_manual =
  function () ->
    print_newline ();
    print_string "Waiting for connection...";
    print_string ("(the socket is " ^ !socket_name ^ ")");
    print_newline ()

(*** Selection of the launching function. ***)

type launching_function = (unit -> unit)

let loading_modes =
  ["direct", exec_direct;
   "runtime", exec_with_runtime;
   "manual", exec_manual]

let set_launching_function func =
  launching_func := func

(* Initialization *)

let _ =
  set_launching_function exec_direct

(*** Connection. ***)

let connection = ref Primitives.std_io
let connection_opened = ref false
