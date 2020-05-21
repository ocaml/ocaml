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

(* Helper functions when writing actions *)

val skip_with_reason : string -> Actions.t

val pass_or_skip
  : bool -> string -> string -> out_channel -> Environments.t
         -> Result.t * Environments.t

val mkreason : string -> string -> int -> string

val testfile : string Actions.A.t

val test_build_directory : string Actions.A.t

val test_source_directory : string Actions.A.t

val words_of_variable : Variables.t -> string list Actions.A.t

val int_of_variable : Variables.t -> int Actions.A.t

val files : string list Actions.A.t

(* val setup_symlinks : string -> string -> string list -> unit *)

val setup_build_env : bool -> string list Actions.A.t -> Result.t Actions.A.t

val setup_simple_build_env : bool -> string list Actions.A.t -> Actions.code

val run_cmd :
  ?environment : string array ->
  ?stdin_variable : Variables.t ->
  ?stdout_variable : Variables.t ->
  ?stderr_variable : Variables.t ->
  ?append : bool ->
  ?timeout : int ->
  out_channel -> Environments.t -> string list -> int

val run : string -> bool -> bool -> Variables.t
                 -> Variables.t option -> Actions.code

val run_program : Actions.code

val run_script : Actions.code

val run_hook : string -> Actions.code

val check_output : string -> Variables.t -> Variables.t -> Actions.code
