(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include module type of struct include Stdlib.Bigarray end
  with module Genarray := Stdlib.Bigarray.Genarray
  with module Array1   := Stdlib.Bigarray.Array1
  with module Array2   := Stdlib.Bigarray.Array2
  with module Array3   := Stdlib.Bigarray.Array3

module Genarray : sig
  include module type of struct include Stdlib.Bigarray.Genarray end
  val map_file:
    Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
    bool -> int array -> ('a, 'b, 'c) t
  [@@ocaml.deprecated "\
Use Unix.map_file instead.\n\
Note that Bigarray.Genarray.map_file raises Sys_error while\n\
Unix.map_file raises Unix_error."]
end

module Array1 : sig
  include module type of struct include Stdlib.Bigarray.Array1 end
  val map_file: Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
    bool -> int -> ('a, 'b, 'c) t
  [@@ocaml.deprecated "\
Use [array1_of_genarray (Unix.map_file ...)] instead.\n\
Note that Bigarray.Array1.map_file raises Sys_error while\n\
Unix.map_file raises Unix_error."]
end

module Array2 : sig
  include module type of struct include Stdlib.Bigarray.Array2 end
  val map_file: Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
                bool -> int -> int -> ('a, 'b, 'c) t
  [@@ocaml.deprecated "\
Use [array2_of_genarray (Unix.map_file ...)] instead.\n\
Note that Bigarray.Array2.map_file raises Sys_error while\n\
Unix.map_file raises Unix_error."]
end

module Array3 : sig
  include module type of struct include Stdlib.Bigarray.Array3 end
  val map_file: Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
             bool -> int -> int -> int -> ('a, 'b, 'c) t
  [@@ocaml.deprecated "\
Use [array3_of_genarray (Unix.map_file ...)] instead.\n\
Note that Bigarray.Array3.map_file raises Sys_error while\n\
Unix.map_file raises Unix_error."]
end
