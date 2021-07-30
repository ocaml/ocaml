(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Jacques Garrigue, Kyoto University RIMS                 *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Standard labeled libraries.

   This meta-module provides versions of the {!Array}, {!Bytes},
   {!List} and {!String} modules where function arguments are
   systematically labeled.  It is intended to be opened at the top of
   source files, as shown below.

   {[
     open StdLabels

     let to_upper = String.map ~f:Char.uppercase_ascii
     let seq len = List.init ~f:(function i -> i) ~len
     let everything = Array.create_matrix ~dimx:42 ~dimy:42 42
   ]}

*)

module Array = ArrayLabels
module Bytes = BytesLabels
module List = ListLabels
module String = StringLabels
