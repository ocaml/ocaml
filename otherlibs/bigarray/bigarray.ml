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

module Super = Stdlib.Bigarray

include (Super : module type of struct include Super end
         with module Genarray := Super.Genarray
         with module Array1   := Super.Array1
         with module Array2   := Super.Array2
         with module Array3   := Super.Array3)

module Genarray = struct
  include Super.Genarray
  external map_internal: Unix.file_descr -> ('a, 'b) kind -> 'c layout ->
    bool -> int array -> int64 -> ('a, 'b, 'c) t
    = "caml_ba_map_file_bytecode" "caml_ba_map_file"
  let map_file fd ?(pos = 0L) kind layout shared dims =
    map_internal fd kind layout shared dims pos
end

module Array1 = struct
  include Super.Array1
  let map_file fd ?pos kind layout shared dim =
    array1_of_genarray
      (Genarray.map_file fd ?pos kind layout shared [|dim|])
end

module Array2 = struct
  include Super.Array2
  let map_file fd ?pos kind layout shared dim1 dim2 =
    array2_of_genarray
      (Genarray.map_file fd ?pos kind layout shared [|dim1;dim2|])
end

module Array3 = struct
  include Super.Array3
  let map_file fd ?pos kind layout shared dim1 dim2 dim3 =
    array3_of_genarray
      (Genarray.map_file fd ?pos kind layout shared [|dim1;dim2;dim3|])
end
