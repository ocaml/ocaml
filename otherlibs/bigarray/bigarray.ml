(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Manuel Serrano et Xavier Leroy, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Bigarray]: large, multi-dimensional, numerical arrays *)

type ('a, 'b) kind = int

type int8_signed_elt
type int8_unsigned_elt
type int16_signed_elt
type int16_unsigned_elt
type int_elt
type int32_elt
type int64_elt
type nativeint_elt
type float32_elt
type float64_elt

(* Keep those constants in sync with the caml_bigarray_kind enumeration
   in bigarray.h *)

let float32 = 0
let float64 = 1
let int8_signed = 2
let int8_unsigned = 3
let int16_signed = 4
let int16_unsigned = 5
let int32 = 6
let int64 = 7
let int = 8
let nativeint = 9

type 'a layout = int

type c_layout
type fortran_layout

(* Keep those constants in sync with the caml_bigarray_layout enumeration
   in bigarray.h *)

let c_layout = 0
let fortran_layout = 0x100

module Genarray = struct
  type ('a, 'b, 'c) t
  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
     = "bigarray_create"
  external get: ('a, 'b, 'c) t -> int array -> 'a
     = "bigarray_get_generic"
  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit
     = "bigarray_set_generic"
  external num_dims: ('a, 'b, 'c) t -> int = "bigarray_num_dims"
  external nth_dim: ('a, 'b, 'c) t -> int -> int = "bigarray_dim"
  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
     = "bigarray_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int ->
                          ('a, 'b, fortran_layout) t
     = "bigarray_sub"
  external slice_left: ('a, 'b, c_layout) t -> int array -> 
                          ('a, 'b, c_layout) t
     = "bigarray_slice"
  external slice_right: ('a, 'b, fortran_layout) t -> int array -> 
                          ('a, 'b, fortran_layout) t
     = "bigarray_slice"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
     = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

module Array1 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim =
    Genarray.create kind layout [|dim|]
  external get: ('a, 'b, 'c) t -> int -> 'a = "bigarray_get_1"
  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "bigarray_set_1"
  let dim a = Genarray.nth_dim a 0
  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t = "bigarray_sub"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

module Array2 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim1 dim2 =
    Genarray.create kind layout [|dim1; dim2|]
  external get: ('a, 'b, 'c) t -> int -> int -> 'a = "bigarray_get_2"
  external set: ('a, 'b, 'c) t -> int -> int -> 'a -> unit = "bigarray_set_2"
  let dim1 a = Genarray.nth_dim a 0
  let dim2 a = Genarray.nth_dim a 1
  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t = "bigarray_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t = "bigarray_sub"
  let slice_left a n = Genarray.slice_left a [|n|]
  let slice_right a n = Genarray.slice_right a [|n|]
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

module Array3 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim1 dim2 dim3 =
    Genarray.create kind layout [|dim1; dim2; dim3|]
  external get: ('a, 'b, 'c) t -> int -> int -> int -> 'a = "bigarray_get_3"
  external set: ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit = "bigarray_set_3"
  let dim1 a = Genarray.nth_dim a 0
  let dim2 a = Genarray.nth_dim a 1
  let dim3 a = Genarray.nth_dim a 2
  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t = "bigarray_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t = "bigarray_sub"
  let slice_left_1 a n m = Genarray.slice_left a [|n; m|]
  let slice_right_1 a n m = Genarray.slice_right a [|n; m|]
  let slice_left_2 a n = Genarray.slice_left a [|n|]
  let slice_right_2 a n = Genarray.slice_right a [|n|]
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

external genarray_of_array1: ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t = "%identity"
external genarray_of_array2: ('a, 'b, 'c) Array2.t -> ('a, 'b, 'c) Genarray.t = "%identity"
external genarray_of_array3: ('a, 'b, 'c) Array3.t -> ('a, 'b, 'c) Genarray.t = "%identity"
let array1_of_genarray a =
  if Genarray.num_dims a = 1 then a else invalid_arg "Bigarray.array1_of_genarray"
let array2_of_genarray a =
  if Genarray.num_dims a = 2 then a else invalid_arg "Bigarray.array2_of_genarray"
let array3_of_genarray a =
  if Genarray.num_dims a = 3 then a else invalid_arg "Bigarray.array3_of_genarray"

