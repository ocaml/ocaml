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

(* Bla bla *)

type ('a, 'b) kind

type int8_signed_elt
type int8_unsigned_elt
type int16_signed_elt
type int16_unsigned_elt
type int_elt
type int32_elt
type int64_elt
type nativeint_elt
type float4_elt
type float8_elt

val int8_signed: (int, int8_signed_elt) kind
val int8_unsigned: (int, int8_unsigned_elt) kind
val int16_signed: (int, int16_signed_elt) kind
val int16_unsigned: (int, int16_unsigned_elt) kind
val int: (int, int_elt) kind
val int32: (int32, int32_elt) kind
val int64: (int64, int64_elt) kind
val nativeint: (nativeint, nativeint_elt) kind
val float4: (float, float4_elt) kind
val float8: (float, float8_elt) kind

type 'a layout

type c_layout
type fortran_layout

val c_layout: c_layout layout
val fortran_layout: fortran_layout layout

module Array1: sig
  type ('a, 'b, 'c) t
  val create: ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) t
  external get: ('a, 'b, 'c) t -> int -> 'a = "bigarray_get_1"
  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "bigarray_set_1"
  val dim: ('a, 'b, 'c) t -> int
  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t = "bigarray_sub"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

module Array2: sig
  type ('a, 'b, 'c) t
  val create: ('a, 'b) kind -> 'c layout -> int -> int -> ('a, 'b, 'c) t
  external get: ('a, 'b, 'c) t -> int -> int -> 'a = "bigarray_get_2"
  external set: ('a, 'b, 'c) t -> int -> int -> 'a -> unit = "bigarray_set_2"
  val dim1: ('a, 'b, 'c) t -> int
  val dim2: ('a, 'b, 'c) t -> int
  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t = "bigarray_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t = "bigarray_sub"
  val slice_left: ('a, 'b, c_layout) t -> int -> ('a, 'b, c_layout) Array1.t
  val slice_right: ('a, 'b, fortran_layout) t -> int -> ('a, 'b, fortran_layout) Array1.t
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

module Array3: sig
  type ('a, 'b, 'c) t
  val create: ('a, 'b) kind -> 'c layout -> int -> int -> int -> ('a, 'b, 'c) t
  external get: ('a, 'b, 'c) t -> int -> int -> int -> 'a = "bigarray_get_3"
  external set: ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit = "bigarray_set_3"
  val dim1: ('a, 'b, 'c) t -> int
  val dim2: ('a, 'b, 'c) t -> int
  val dim3: ('a, 'b, 'c) t -> int
  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t = "bigarray_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t = "bigarray_sub"
  val slice_left_1: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) Array1.t
  val slice_right_1: ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) Array1.t
  val slice_left_2: ('a, 'b, c_layout) t -> int -> ('a, 'b, c_layout) Array2.t
  val slice_right_2: ('a, 'b, fortran_layout) t -> int -> ('a, 'b, fortran_layout) Array2.t
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

module Genarray: sig
  type ('a, 'b, 'c) t
  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t = "bigarray_create"
  external get: ('a, 'b, 'c) t -> int array -> 'a = "bigarray_get_generic"
  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit = "bigarray_set_generic"
  external num_dims: ('a, 'b, 'c) t -> int = "bigarray_num_dims"
  external nth_dim: ('a, 'b, 'c) t -> int -> int = "bigarray_dim"
  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t = "bigarray_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t = "bigarray_sub"
  external slice_left: ('a, 'b, c_layout) t -> int array -> ('a, 'b, c_layout) t = "bigarray_slice"
  external slice_right: ('a, 'b, fortran_layout) t -> int array -> ('a, 'b, fortran_layout) t = "bigarray_slice"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "bigarray_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "bigarray_fill"
end

external genarray_of_array1: ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t = "%identity"
external genarray_of_array2: ('a, 'b, 'c) Array2.t -> ('a, 'b, 'c) Genarray.t = "%identity"
external genarray_of_array3: ('a, 'b, 'c) Array3.t -> ('a, 'b, 'c) Genarray.t = "%identity"
val array1_of_genarray: ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array1.t
val array2_of_genarray: ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array2.t
val array3_of_genarray: ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array3.t
