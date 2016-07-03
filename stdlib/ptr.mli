(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                          Demi Marie Obenour                            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations on raw pointers.
 
   This module provides operations on pointers, which represent raw machine
   addresses.  Any external pointer can be stored in a {!Ptr.t} and
   presented back to external code.  Furthermore, pointers can be
   dereferenced, yielding the contents of the memory location they point to.

   It is almost always a very bad idea to expose pointers in library module
   interfaces -- they are intended for FFI work, such as accessing C data
   structures from OCaml without copying.  Libraries that use pointers
   should use them only to access data allocated by foreign allocators.

   Most users should not use pointers directly.  They are intended for use
   by higher-level libraries (such as ocaml-ctypes) as a faster alternative
   to C externals for accessing individual words of foreign data structures.

   If you control where the data is allocated, use the {!Bigarray} module
   instead, or use other libraries that provide safe abstractions.  The
   {!Bigarray} module ensures that the data lives as long as the bigarray is
   alive, and that it is freed when no longer accessible by OCaml code.
   Like raw pointers, the data contained in a bigarray can be accessed by
   other code without copying.

   Other libraries that provide higher-level functionality for manipulating
   foreign data include ocaml-ctypes and cstruct.

   Pointers have a phantom type parameter.  This is intended to distinguish
   pointers pointing to different types.  The functions in this module
   are blind to this type parameter.

   Pointers are {e highly} unsafe.  There is no check (and indeed no way to
   check) that a pointer actually points to the location in memory that it
   claims to.   It is the responsibilty of any code that uses
   pointers to ensure that such use is safe, even given invalid or malicious
   input.  If you fail to do so, you will get a crash at best.  If the
   process is processing untrusted data, a misuse of pointers is likely to
   result in corruption of memory that allows for an attacker to execute
   arbitrary code.

   @since 4.04.0
*)

type 'a t
(** The type of raw pointers *)

val offset : nativeint -> 'a t -> 'a t [@@inline]
(** [offset n ptr] creates a new pointer with offset [n] from [ptr]. *)

external cast_ptr : 'a t -> 'b t = "%identity"
(** Cast pointers with different type parameters. *)

external to_nativeint : 'a t -> nativeint = "%identity"
(** Get the address pointed to as a number *)

external of_nativeint : nativeint -> 'a t = "%identity"
(** Convert a nativeint to a pointer *)

external load8 : 'a t -> char = "%load8"
(** Load a byte from the pointer.  The result is placed in an OCaml [char] *)

external aligned_load16 : 'a t -> int = "%aligned_load16"
(** Load 16 bits from a pointer.  The pointer must point to an even address,
    but this is not checked. *)

external aligned_load32 : 'a t -> int32 = "%aligned_load32"
(** Load 32 bits from a pointer.  The pointer must point to an address that
    is divisible by 4, but this is not checked. *)

external aligned_load64 : 'a t -> int64 = "%aligned_load64"
(** Load 64 bits from a pointer.  The pointer must point to an address that
    is divisible by 8, but this is not checked. *)

external aligned_loadnative : 'a t -> nativeint = "%aligned_loadnative"
(** Load a machine word from a pointer.  The pointer must point to an
    address that is a multiple of the size of a machine word in bytes,
    but this is not checked. *)

external unaligned_load16 : 'a t -> int = "%unaligned_load16"
(** Load 16 bits from a pointer.  The pointer need not be aligned. *)

external unaligned_load32 : 'a t -> int32 = "%unaligned_load32"
(** Load 32 bits from a pointer.  The pointer need not be aligned. *)

external unaligned_load64 : 'a t -> int64 = "%unaligned_load64"
(** Load 64 bits from a pointer.  The pointer need not be aligned. *)

external unaligned_loadnative : 'a t -> nativeint = "%unaligned_loadnative"
(** Load a machine word from a pointer.  The pointer need not be aligned. *)
