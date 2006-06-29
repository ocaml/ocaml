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

module type S = sig
  type t = 'abstract;
  exception Error of string and string;

  (** [mk ?ocaml_stdlib ?camlp4_stdlib]
      The stdlib flag is true by default.
      To disable it use: [mk ~ocaml_stdlib:False] *)
  value mk : ?ocaml_stdlib: bool -> ?camlp4_stdlib: bool -> unit -> t;

  (** Fold over the current load path list. *)
  value fold_load_path : t -> (string -> 'a -> 'a) -> 'a -> 'a;

  (** [load f] Load the file [f]. If [f] is not an absolute path name,
      the load path list used to find the directory of [f]. *)
  value load : t -> string -> unit;

  (** [include_dir d] Add the directory [d] in the current load path
      list (like the common -I option). *)
  value include_dir : t -> string -> unit;
end;
