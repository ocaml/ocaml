(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

type spec_list = list (string * Arg.spec * string);
value init : spec_list -> unit;
value add : string -> Arg.spec -> string -> unit;
  (** Add an option to the command line options. *)
value print_usage_list : spec_list -> unit;
value ext_spec_list : unit -> spec_list;
value parse : (string -> unit) -> array string -> list string;
