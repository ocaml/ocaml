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
  val fold_left_result :
    ('acc -> 'a -> ('acc, 'e) result) -> 'acc -> 'a list -> ('acc, 'e) result
end

module String : sig
  include module type of Misc.Stdlib.String
  val words : string -> string list
end

module Sys : sig
  include module type of Sys
  val file_is_empty : string -> bool
  val make_directory : string -> unit
  val rm_rf : string -> unit
  val string_of_file : string -> string
  val iter_lines_of_file : (string -> unit) -> string -> unit
  val dump_file : out_channel -> ?prefix:string -> string -> unit
  val copy_chan : in_channel -> out_channel -> unit
  val copy_file : string -> string -> unit
  val copy_directory : string -> string -> unit
  val force_remove : string -> unit
  val with_chdir : string -> (unit -> 'a) -> 'a
  val getenv_with_default_value : string -> string -> string
  val safe_getenv : string -> string
  val with_input_file : ?bin:bool -> string -> (in_channel -> 'a) -> 'a
  val with_output_file : ?bin:bool -> string -> (out_channel -> 'a) -> 'a
end

module Seq : sig
  include module type of struct include Seq end

  val equal : 'a t -> 'a t -> bool
end

module Unix : sig
  include module type of Ocamltest_unix
end
