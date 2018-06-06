(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm

(** [bind name arg fn] is equivalent to [let name = arg in fn name],
    or simply [fn arg] if [arg] is simple enough *)
val bind :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but also treats loads from a variable as simple *)
val bind_load :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but does not treat variables as simple *)
val bind_nonvar :
  string -> expression -> (expression -> expression) -> expression

(** A null header with GC bits set to black *)
val caml_black : nativeint

(** A constant equal to the tag for float arrays *)
val floatarray_tag : Debuginfo.t -> expression

(** [block_header tag size] creates a header with tag [tag] for a
    block of size [size] *)
val block_header : int -> int -> nativeint

(** Same as block_header, but with GC bits set to black *)
val black_block_header : int -> int -> nativeint

(** Closure headers of the given size *)
val white_closure_header : int -> nativeint
val black_closure_header : int -> nativeint

(** Infix header at the given offset *)
val infix_header : int -> nativeint

(** Header for a boxed float value *)
val float_header : nativeint

(** Header for an unboxed float array of the given size *)
val floatarray_header : int -> nativeint

(** Header for a string (or bytes) of the given length *)
val string_header : int -> nativeint

(** Boxed integers headers *)
val boxedint32_header : nativeint
val boxedint64_header : nativeint
val boxedintnat_header : nativeint

(** Wrappers *)
val alloc_float_header : Debuginfo.t -> expression
val alloc_floatarray_header : int -> Debuginfo.t -> expression
val alloc_closure_header : int -> Debuginfo.t -> expression
val alloc_infix_header : int -> Debuginfo.t -> expression
val alloc_boxedint32_header : Debuginfo.t -> expression
val alloc_boxedint64_header : Debuginfo.t -> expression
val alloc_boxedintnat_header : Debuginfo.t -> expression

