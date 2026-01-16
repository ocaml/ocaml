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

(* Run programs and log their stdout/stderr, with a timer... *)

open Ocamltest_stdlib

type settings = {
  progname : string;
  argv : string array;
  envp : string array;
  stdin_filename : string;
  stdout_filename : string;
  stderr_filename : string;
  append : bool;
  timeout : int;
  log : out_channel;
}

let settings_of_commandline ?(stdout_fname="") ?(stderr_fname="") commandline =
  let words = String.words commandline in
  let quoted_words =
    if Sys.win32
    then List.map Filename.maybe_quote words
    else words in
  {
    progname = List.hd quoted_words;
    argv = Array.of_list quoted_words;
    envp = [||];
    stdin_filename = "";
    stdout_filename = stdout_fname;
    stderr_filename = stderr_fname;
    append = false;
    timeout = 0;
    log = stderr
  }

external run : settings -> int = "caml_run_command"
external drop_privilege : string -> unit = "caml_drop_privilege"

let () =
  (* This allows lib-runtime-events/test_create_cursor_failures.ml to operate
     correctly. That test removes its own access to a file and cannot be run as
     root. Cygwin intentionally enables SeBackupPrivilege (see
     set_cygwin_privileges in sec/helper.cc) in order to allow an elevated
     process to behave in a root-like fashion. Thwart this by dropping the
     privilege from our primary token - CreateProcess uses the primary token,
     which means anything called by ocamltest will not be able to enable
     SeBackupPrivilege. *)
  if Sys.cygwin then
    drop_privilege "SeBackupPrivilege"

let run_commandline commandline = run (settings_of_commandline commandline)
