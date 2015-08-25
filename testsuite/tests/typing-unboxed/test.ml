(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


external a : (int [@untagged]) -> unit = "a"
external b : (int32 [@unboxed]) -> unit = "b"
external c : (int64 [@unboxed]) -> unit = "c"
external d : (nativeint [@unboxed]) -> unit = "d"
external e : (float [@unboxed]) -> unit = "e"

type t = private int

external f : (t [@untagged]) -> unit = "f"

module M : sig
  external a : int -> (int [@untagged]) = "a"
  external b : (int [@untagged]) -> int = "b"
end = struct
  external a : int -> (int [@untagged]) = "a"
  external b : (int [@untagged]) -> int = "b"
end;;

(* Bad: attributes not reported in the interface *)

module Bad1 : sig
  external f : int -> int = "f"
end = struct
  external f : int -> (int [@untagged]) = "f"
end;;

module Bad2 : sig
  external f : int -> int = "a"
end = struct
  external f : (int [@untagged]) -> int = "f"
end;;

module Bad3 : sig
  external f : float -> float = "f"
end = struct
  external f : float -> (float [@unboxed]) = "f"
end;;

module Bad4 : sig
  external f : float -> float = "a"
end = struct
  external f : (float [@unboxed]) -> float = "f"
end;;

(* Bad: attributes in the interface but not in the implementation *)

module Bad5 : sig
  external f : int -> (int [@untagged]) = "f"
end = struct
  external f : int -> int = "f"
end;;

module Bad6 : sig
  external f : (int [@untagged]) -> int = "f"
end = struct
  external f : int -> int = "a"
end;;

module Bad7 : sig
  external f : float -> (float [@unboxed]) = "f"
end = struct
  external f : float -> float = "f"
end;;

module Bad8 : sig
  external f : (float [@unboxed]) -> float = "f"
end = struct
  external f : float -> float = "a"
end;;

(* Bad: unboxed or untagged with the wrong type *)

external g : (float [@untagged]) -> float = "g";;
external h : (int [@unboxed]) -> float = "h";;

(* This should be rejected, but it is quite complicated to do
   in the current state of things *)

external i : int -> float [@unboxed] = "i";;
external j : int -> (float [@unboxed]) * float = "j";;
external k : int -> (float [@unboxd]) = "k";;
