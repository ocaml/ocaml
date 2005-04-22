(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val ( + ) :
  [| int -> int -> int | float -> float -> float | int32 -> int32 -> int32
   | int64 -> int64 -> int64 | nativeint -> nativeint -> nativeint |]
val ( - ) :
  [| int -> int -> int | float -> float -> float | int32 -> int32 -> int32
   | int64 -> int64 -> int64 | nativeint -> nativeint -> nativeint |]
val ( * ) :
  [| int -> int -> int | float -> float -> float | int32 -> int32 -> int32
   | int64 -> int64 -> int64 | nativeint -> nativeint -> nativeint |]
val ( / ) :
  [| int -> int -> int | float -> float -> float | int32 -> int32 -> int32
   | int64 -> int64 -> int64 | nativeint -> nativeint -> nativeint |]
