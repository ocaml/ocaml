(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Simplification of operations on boxed integers (nativeint, Int32, Int64). *)

module Simplify_boxed_nativeint : Simplify_boxed_integer_ops_intf.S
  with type t := Nativeint.t

module Simplify_boxed_int32 : Simplify_boxed_integer_ops_intf.S
  with type t := Int32.t

module Simplify_boxed_int64 : Simplify_boxed_integer_ops_intf.S
  with type t := Int64.t
