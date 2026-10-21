(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type pr = Format.formatter -> unit

(** Utility functions *)
module Pp: sig
  type conv
  val json: conv
  val sexp: conv

(*   val int: conv -> int printer *)
  val bool: conv -> bool -> pr
  val item: conv -> key:string -> pr -> pr
  val list: conv -> pr list -> pr
  val tuple: inline:bool -> conv -> pr list -> pr
  val record: conv -> pr list -> pr
end


type t = {
  name:string;
  make:
    'a. ?color:Misc.Color.setting -> version:Diagnostic_validation.version
    -> device:Log.Device.t -> 'a Diagnostic.t -> 'a Log.t;
}


(** {1:diagnostic_backends_streaming  Streaming backends} *)

(** Directly print to the low-level device *)
val fmt: t

(** Prefix the printed contents with the name of the key *)
val fmt_with_fields:t

type 'a printer = Format.formatter -> 'a -> unit
type extension_printer =
  { extension: 'b. 'b Diagnostic.extension -> 'b printer option}
val add_extension: extension_printer -> unit
(** Add a printer for a Diagnostic extension to the [fmt] and fmt_with_fields
    printer.*)

(** {1:diagnostic_backend_structured Structured backends} *)
val json: t
val sexp: t
