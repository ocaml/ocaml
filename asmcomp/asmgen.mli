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

(* From lambda to assembly code *)

module type S = sig
module Emit : sig
  val begin_assembly: unit -> unit
  val end_assembly: unit -> unit
end
module Cmmgen : Cmmgen.S
module Emitaux : Emitaux.S

val compile_implementation_flambda :
    ?toplevel:(string -> bool) ->
    source_provenance:Timings.source_provenance ->
    string ->
    backend:(module Backend_intf.S) ->
    Format.formatter -> Flambda.program -> unit

val compile_implementation_clambda :
    ?toplevel:(string -> bool) ->
    source_provenance:Timings.source_provenance ->
    string ->
    Format.formatter -> Lambda.program -> unit

val compile_phrase :
    Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit


val compile_unit:
  source_provenance:Timings.source_provenance ->
  string(*prefixname*) ->
  string(*asm file*) -> bool(*keep asm*) ->
  string(*obj file*) -> (unit -> unit) -> unit
end

module Make (B : Native_backend_intf.S) : S with module Emit = B.Emit
