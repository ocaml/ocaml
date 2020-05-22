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

val pass_or_skip : bool -> string -> string -> Result.t Actions.A.t

val mkreason : string -> string -> int -> string

val testfile : string Actions.A.t

val test_build_directory : string Actions.A.t

val test_source_directory : string Actions.A.t

val words_of_variable : Variables.t -> string list Actions.A.t

val int_of_variable : Variables.t -> int Actions.A.t

val files : string list Actions.A.t

(* val setup_symlinks : string -> string -> string list -> unit *)

val setup_build_env :
  bool -> string list Actions.A.t -> unit A.t

val setup_simple_build_env :
  bool -> string list Actions.A.t -> Environments.t A.t

val run : string -> bool -> bool -> Variables.t
                 -> Variables.t option -> Result.t A.t

val run_program : Result.t A.t

val run_script : (Result.t * Environments.t) A.t

val run_hook : string -> Result.t A.t

val check_output : string -> Variables.t -> Variables.t -> Result.t A.t
