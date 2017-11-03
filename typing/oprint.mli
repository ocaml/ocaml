(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Outcometree.Decorated

val ellipsis: string ref
val out_value : (formatter -> out_value ext -> unit) ref
val out_type : (formatter -> out_type ext -> unit) ref
val out_class_type : (formatter -> out_class_type ext -> unit) ref
val out_module_type : (formatter -> out_module_type ext -> unit) ref
val out_sig_item : (formatter -> out_sig_item ext -> unit) ref
val out_signature : (formatter -> out_sig_item ext list -> unit) ref
val out_type_extension : (formatter -> out_type_extension ext -> unit) ref
val out_phrase : (formatter -> out_phrase -> unit) ref

val parenthesized_ident : string -> bool
