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

(* A few extensions to OCaml's standard library *)

(* Pervasive *)

val input_line_opt : in_channel -> string option

module Char : sig
  include module type of Char
  val is_blank : char -> bool
end

module Filename  : sig
  include module type of Filename
  val path_sep : string
  val maybe_quote : string -> string
  val make_filename : string -> string -> string
  val make_path : string list -> string
  val mkexe : string -> string
end

module List : sig
  include module type of List
  val concatmap : ('a -> 'b list) -> 'a list -> 'b list
end

module String : sig
  include module type of String
  val words : string -> string list
end

module Sys : sig
  include module type of Sys
  val file_is_empty : string -> bool
  val run_system_command : string -> unit
  val make_directory : string -> unit
  val string_of_file : string -> string
  val copy_file : string -> string -> unit
  val with_chdir : string -> (unit -> 'a) -> 'a
end

module StringSet : sig
  include Set.S with type elt = string
  val string_of_stringset : t -> string
end

module StringMap : Map.S with type key = string
