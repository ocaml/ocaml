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

open Actions

val skip_with_reason : string -> Actions.t

val pass_or_skip : bool -> string -> string -> Eff.Result.t Actions.A.t

val mkreason : string -> string -> int -> string

val testfile : string Actions.A.t

val test_build_directory : string Actions.A.t

val test_source_directory : string Actions.A.t

val words_of_variable : Variables.t -> string list Actions.A.t

val int_of_variable : Variables.t -> int Actions.A.t

val files : string list Actions.A.t

val run_params:
    ?environment:string array A.t ->
    ?stdin_variable:Variables.t ->
    ?stdout_variable:Variables.t ->
    ?stderr_variable:Variables.t ->
    ?append:bool ->
    ?timeout:int ->
    ?expected_exit_codes:int A.t list ->
    ?skip_exit_codes:int A.t list -> unit -> Eff.run_params A.t

(* val setup_symlinks : string -> string -> string list -> unit *)

val setup_build_env :
  bool -> string list Actions.A.t -> Eff.t A.t

val setup_simple_build_env :
  bool -> string list Actions.A.t -> (Eff.t * Environments.t) A.t

val run : string -> bool -> bool -> Variables.t
                 -> Variables.t option -> Eff.t A.t

val run_program : Eff.t A.t

val run_script : Eff.t (* Environments.t FIXME *) A.t

val run_hook : string -> Eff.t A.t

val check_output : string -> Variables.t -> Variables.t -> Eff.t A.t
