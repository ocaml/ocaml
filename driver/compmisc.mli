(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val init_path :
  ?auto_include:Load_path.auto_include_callback -> ?dir:string -> unit -> unit
val initial_env : unit -> Env.t

(* Support for flags that can also be set from an environment variable *)
val set_from_env : 'a option ref -> 'a Clflags.env_reader -> unit
val read_clflags_from_env : unit -> unit

val with_ppf_dump : file_prefix:string -> (Format.formatter -> 'a) -> 'a

val auto_include : Load_path.auto_include_callback
(** [auto_include find_in_dir fn] is a callback function to be passed to
    {!Load_path.init} and automatically adds [-I +lib] to the load path after
    displaying an alert. *)
