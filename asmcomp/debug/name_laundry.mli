(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The name laundry: where names get (de)mangled.
    (This functionality is used by the debugger support library as well as
    the compiler.) *)

(** The name of the DWARF debugging information entry for the given
    function itself. *)
val die_name_from_function_name : function_name:string -> string

(** The name of the DWARF debugging information entry corresponding to the
    type of some identifier. *)
val base_type_die_name_for_var : Backend_var.t -> output_path:string -> string

type split_base_type_die_name_result = {
  ident_name : string;
  ident_stamp : int;
  output_path : string;
}

(** The inverse of [base_type_die_name_for_ident]. *)
val split_base_type_die_name : string -> split_base_type_die_name_result option
