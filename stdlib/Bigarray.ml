(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Manuel Serrano et Xavier Leroy, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Bigarray]: large, multi-dimensional, numerical arrays *)

(* These types in must be kept in sync with the tables in
   ../typing/typeopt.ml *)

type float32_elt = Float32_elt
type float64_elt = Float64_elt
type int8_signed_elt = Int8_signed_elt
type int8_unsigned_elt = Int8_unsigned_elt
type int16_signed_elt = Int16_signed_elt
type int16_unsigned_elt = Int16_unsigned_elt
type int32_elt = Int32_elt
type int64_elt = Int64_elt
type int_elt = Int_elt
type nativeint_elt = Nativeint_elt
type complex32_elt = Complex32_elt
type complex64_elt = Complex64_elt

type ('a, 'b) kind =
    Float32 : (float, float32_elt) kind
  | Float64 : (float, float64_elt) kind
  | Int8_signed : (int, int8_signed_elt) kind
  | Int8_unsigned : (int, int8_unsigned_elt) kind
  | Int16_signed : (int, int16_signed_elt) kind
  | Int16_unsigned : (int, int16_unsigned_elt) kind
  | Int32 : (int32, int32_elt) kind
  | Int64 : (int64, int64_elt) kind
  | Int : (int, int_elt) kind
  | Nativeint : (nativeint, nativeint_elt) kind
  | Complex32 : (Complex.t, complex32_elt) kind
  | Complex64 : (Complex.t, complex64_elt) kind
  | Char : (char, int8_unsigned_elt) kind

type c_layout = C_layout_typ
type fortran_layout = Fortran_layout_typ (**)

type 'a layout =
    C_layout: c_layout layout
  | Fortran_layout: fortran_layout layout

(* Keep those constants in sync with the caml_ba_kind enumeration
   in bigarray.h *)

let float32 = Float32
let float64 = Float64
let int8_signed = Int8_signed
let int8_unsigned = Int8_unsigned
let int16_signed = Int16_signed
let int16_unsigned = Int16_unsigned
let int32 = Int32
let int64 = Int64
let int = Int
let nativeint = Nativeint
let complex32 = Complex32
let complex64 = Complex64
let char = Char

let kind_size_in_bytes : type a b. (a, b) kind -> int = function
  | Float32 -> 4
  | Float64 -> 8
  | Int8_signed -> 1
  | Int8_unsigned -> 1
  | Int16_signed -> 2
  | Int16_unsigned -> 2
  | Int32 -> 4
  | Int64 -> 8
  | Int -> Sys.word_size / 8
  | Nativeint -> Sys.word_size / 8
  | Complex32 -> 8
  | Complex64 -> 16
  | Char -> 1

(* Keep those constants in sync with the caml_ba_layout enumeration
   in bigarray.h *)

let c_layout = C_layout
let fortran_layout = Fortran_layout

module Genarray = struct
  type (!'a, !'b, !'c) t
  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
     = "caml_ba_create"
  external get: ('a, 'b, 'c) t -> int array -> 'a
     = "caml_ba_get_generic"
  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit
     = "caml_ba_set_generic"

  let rec cloop arr idx f col max =
    if col = Array.length idx then set arr idx (f idx)
    else for j = 0 to pred max.(col) do
           idx.(col) <- j;
           cloop arr idx f (succ col) max
         done
  let rec floop arr idx f col max =
    if col < 0 then set arr idx (f idx)
    else for j = 1 to max.(col) do
           idx.(col) <- j;
           floop arr idx f (pred col) max
         done
  let init (type t) kind (layout : t layout) dims f =
    let arr = create kind layout dims in
    match Array.length dims, layout with
    | 0, _ -> arr
    | dlen, C_layout -> cloop arr (Array.make dlen 0) f 0 dims; arr
    | dlen, Fortran_layout -> floop arr (Array.make dlen 1) f (pred dlen) dims;
                              arr

  external num_dims: ('a, 'b, 'c) t -> int = "caml_ba_num_dims"
  external nth_dim: ('a, 'b, 'c) t -> int -> int = "caml_ba_dim"
  let dims a =
    let n = num_dims a in
    let d = Array.make n 0 in
    for i = 0 to n-1 do d.(i) <- nth_dim a i done;
    d

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  external change_layout: ('a, 'b, 'c) t -> 'd layout -> ('a, 'b, 'd) t
     = "caml_ba_change_layout"

  let size_in_bytes arr =
    (kind_size_in_bytes (kind arr)) * (Array.fold_left ( * ) 1 (dims arr))

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
     = "caml_ba_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int ->
                          ('a, 'b, fortran_layout) t
     = "caml_ba_sub"
  external slice_left: ('a, 'b, c_layout) t -> int array ->
                          ('a, 'b, c_layout) t
     = "caml_ba_slice"
  external slice_right: ('a, 'b, fortran_layout) t -> int array ->
                          ('a, 'b, fortran_layout) t
     = "caml_ba_slice"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
     = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
end

module Array0 = struct
  type (!'a, !'b, !'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout =
    Genarray.create kind layout [||]
  let get arr = Genarray.get arr [||]
  let set arr = Genarray.set arr [||]
  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external change_layout: ('a, 'b, 'c) t -> 'd layout -> ('a, 'b, 'd) t
    = "caml_ba_change_layout"

  let size_in_bytes arr = kind_size_in_bytes (kind arr)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"

  let of_value kind layout v =
    let a = create kind layout in
    set a v;
    a
  let init = of_value
end

module Array1 = struct
  type (!'a, !'b, !'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim =
    Genarray.create kind layout [|dim|]
  external get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_ref_1"
  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_set_1"
  external unsafe_get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_unsafe_ref_1"
  external unsafe_set: ('a, 'b, 'c) t -> int -> 'a -> unit
     = "%caml_ba_unsafe_set_1"
  external dim: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external change_layout: ('a, 'b, 'c) t -> 'd layout -> ('a, 'b, 'd) t
    = "caml_ba_change_layout"

  let size_in_bytes arr =
    (kind_size_in_bytes (kind arr)) * (dim arr)

  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t = "caml_ba_sub"
  let slice (type t) (a : (_, _, t) Genarray.t) n =
    match layout a with
    | C_layout -> (Genarray.slice_left a [|n|] : (_, _, t) Genarray.t)
    | Fortran_layout -> (Genarray.slice_right a [|n|]: (_, _, t) Genarray.t)
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  let c_init arr dim f =
    for i = 0 to pred dim do unsafe_set arr i (f i) done
  let fortran_init arr dim f =
    for i = 1 to dim do unsafe_set arr i (f i) done
  let init (type t) kind (layout : t layout) dim f =
    let arr = create kind layout dim in
    match layout with
    | C_layout -> c_init arr dim f; arr
    | Fortran_layout -> fortran_init arr dim f; arr
  let of_array (type t) kind (layout: t layout) data =
    let ba = create kind layout (Array.length data) in
    let ofs =
      match layout with
        C_layout -> 0
      | Fortran_layout -> 1
    in
    for i = 0 to Array.length data - 1 do unsafe_set ba (i + ofs) data.(i) done;
    ba
end

module Array2 = struct
  type (!'a, !'b, !'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim1 dim2 =
    Genarray.create kind layout [|dim1; dim2|]
  external get: ('a, 'b, 'c) t -> int -> int -> 'a = "%caml_ba_ref_2"
  external set: ('a, 'b, 'c) t -> int -> int -> 'a -> unit = "%caml_ba_set_2"
  external unsafe_get: ('a, 'b, 'c) t -> int -> int -> 'a
     = "%caml_ba_unsafe_ref_2"
  external unsafe_set: ('a, 'b, 'c) t -> int -> int -> 'a -> unit
     = "%caml_ba_unsafe_set_2"
  external dim1: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  external dim2: ('a, 'b, 'c) t -> int = "%caml_ba_dim_2"
  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external change_layout: ('a, 'b, 'c) t -> 'd layout -> ('a, 'b, 'd) t
    = "caml_ba_change_layout"

  let size_in_bytes arr =
    (kind_size_in_bytes (kind arr)) * (dim1 arr) * (dim2 arr)

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
     = "caml_ba_sub"
  external sub_right:
    ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
     = "caml_ba_sub"
  let slice_left a n = Genarray.slice_left a [|n|]
  let slice_right a n = Genarray.slice_right a [|n|]
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  let c_init arr dim1 dim2 f =
    for i = 0 to pred dim1 do
      for j = 0 to pred dim2 do
        unsafe_set arr i j (f i j)
      done
    done
  let fortran_init arr dim1 dim2 f =
    for j = 1 to dim2 do
      for i = 1 to dim1 do
        unsafe_set arr i j (f i j)
      done
    done
  let init (type t) kind (layout : t layout) dim1 dim2 f =
    let arr = create kind layout dim1 dim2 in
    match layout with
    | C_layout -> c_init arr dim1 dim2 f; arr
    | Fortran_layout -> fortran_init arr dim1 dim2 f; arr
  let of_array (type t) kind (layout: t layout) data =
    let dim1 = Array.length data in
    let dim2 = if dim1 = 0 then 0 else Array.length data.(0) in
    let ba = create kind layout dim1 dim2 in
    let ofs =
      match layout with
        C_layout -> 0
      | Fortran_layout -> 1
    in
    for i = 0 to dim1 - 1 do
      let row = data.(i) in
      if Array.length row <> dim2 then
        invalid_arg("Bigarray.Array2.of_array: non-rectangular data");
      for j = 0 to dim2 - 1 do
        unsafe_set ba (i + ofs) (j + ofs) row.(j)
      done
    done;
    ba
end

module Array3 = struct
  type (!'a, !'b, !'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim1 dim2 dim3 =
    Genarray.create kind layout [|dim1; dim2; dim3|]
  external get: ('a, 'b, 'c) t -> int -> int -> int -> 'a = "%caml_ba_ref_3"
  external set: ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
     = "%caml_ba_set_3"
  external unsafe_get: ('a, 'b, 'c) t -> int -> int -> int -> 'a
     = "%caml_ba_unsafe_ref_3"
  external unsafe_set: ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
     = "%caml_ba_unsafe_set_3"
  external dim1: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  external dim2: ('a, 'b, 'c) t -> int = "%caml_ba_dim_2"
  external dim3: ('a, 'b, 'c) t -> int = "%caml_ba_dim_3"
  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external change_layout: ('a, 'b, 'c) t -> 'd layout -> ('a, 'b, 'd) t
    = "caml_ba_change_layout"

  let size_in_bytes arr =
    (kind_size_in_bytes (kind arr)) * (dim1 arr) * (dim2 arr) * (dim3 arr)

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
     = "caml_ba_sub"
  external sub_right:
     ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
     = "caml_ba_sub"
  let slice_left_1 a n m = Genarray.slice_left a [|n; m|]
  let slice_right_1 a n m = Genarray.slice_right a [|n; m|]
  let slice_left_2 a n = Genarray.slice_left a [|n|]
  let slice_right_2 a n = Genarray.slice_right a [|n|]
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  let c_init arr dim1 dim2 dim3 f =
    for i = 0 to pred dim1 do
      for j = 0 to pred dim2 do
        for k = 0 to pred dim3 do
          unsafe_set arr i j k (f i j k)
        done
      done
    done
  let fortran_init arr dim1 dim2 dim3 f =
    for k = 1 to dim3 do
      for j = 1 to dim2 do
        for i = 1 to dim1 do
          unsafe_set arr i j k (f i j k)
        done
      done
    done
  let init (type t) kind (layout : t layout) dim1 dim2 dim3 f =
    let arr = create kind layout dim1 dim2 dim3 in
    match layout with
    | C_layout -> c_init arr dim1 dim2 dim3 f; arr
    | Fortran_layout -> fortran_init arr dim1 dim2 dim3 f; arr
  let of_array (type t) kind (layout: t layout) data =
    let dim1 = Array.length data in
    let dim2 = if dim1 = 0 then 0 else Array.length data.(0) in
    let dim3 = if dim2 = 0 then 0 else Array.length data.(0).(0) in
    let ba = create kind layout dim1 dim2 dim3 in
    let ofs =
      match layout with
        C_layout -> 0
      | Fortran_layout -> 1
    in
    for i = 0 to dim1 - 1 do
      let row = data.(i) in
      if Array.length row <> dim2 then
        invalid_arg("Bigarray.Array3.of_array: non-cubic data");
      for j = 0 to dim2 - 1 do
        let col = row.(j) in
        if Array.length col <> dim3 then
          invalid_arg("Bigarray.Array3.of_array: non-cubic data");
        for k = 0 to dim3 - 1 do
          unsafe_set ba (i + ofs) (j + ofs) (k + ofs) col.(k)
        done
      done
    done;
    ba
end

external genarray_of_array0: ('a, 'b, 'c) Array0.t -> ('a, 'b, 'c) Genarray.t
   = "%identity"
external genarray_of_array1: ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t
   = "%identity"
external genarray_of_array2: ('a, 'b, 'c) Array2.t -> ('a, 'b, 'c) Genarray.t
   = "%identity"
external genarray_of_array3: ('a, 'b, 'c) Array3.t -> ('a, 'b, 'c) Genarray.t
   = "%identity"
let array0_of_genarray a =
  if Genarray.num_dims a = 0 then a
  else invalid_arg "Bigarray.array0_of_genarray"
let array1_of_genarray a =
  if Genarray.num_dims a = 1 then a
  else invalid_arg "Bigarray.array1_of_genarray"
let array2_of_genarray a =
  if Genarray.num_dims a = 2 then a
  else invalid_arg "Bigarray.array2_of_genarray"
let array3_of_genarray a =
  if Genarray.num_dims a = 3 then a
  else invalid_arg "Bigarray.array3_of_genarray"

external reshape:
   ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t
   = "caml_ba_reshape"
let reshape_0 a = reshape a [||]
let reshape_1 a dim1 = reshape a [|dim1|]
let reshape_2 a dim1 dim2 = reshape a [|dim1;dim2|]
let reshape_3 a dim1 dim2 dim3 = reshape a [|dim1;dim2;dim3|]

(* Force caml_ba_get_{1,2,3,N} to be linked in, since we don't refer
   to those primitives directly in this file *)

let _ =
  let _ = Genarray.get in
  let _ = Array1.get in
  let _ = Array2.get in
  let _ = Array3.get in
  ()

[@@@ocaml.warning "-32"]
external get1: unit -> unit = "caml_ba_get_1"
external get2: unit -> unit = "caml_ba_get_2"
external get3: unit -> unit = "caml_ba_get_3"
external set1: unit -> unit = "caml_ba_set_1"
external set2: unit -> unit = "caml_ba_set_2"
external set3: unit -> unit = "caml_ba_set_3"
