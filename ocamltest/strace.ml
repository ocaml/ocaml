(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Implementation of the strace feature *)

let strace = Variables.make ("strace", "Whether to use strace")
let strace_flags =
  Variables.make ("strace_flags", "Which flags to pass to strace")

let (counters : (string, int) Hashtbl.t) = Hashtbl.create 10

let get_logfile_name base =
  let n = try Hashtbl.find counters base with Not_found -> 1 in
  let filename = Printf.sprintf "strace-%s_%d.log" base n in
  Hashtbl.replace counters base (n+1);
  filename

let _ =
  Variables.register_variable strace;
  Variables.register_variable strace_flags
