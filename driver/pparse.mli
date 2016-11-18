(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt       *)
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

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

val preprocess : string -> string
val remove_preprocessed : string -> unit

type 'a ast_kind =
| Structure : Parsetree.structure ast_kind
| Signature : Parsetree.signature ast_kind

val read_ast : 'a ast_kind -> string -> 'a
val write_ast : 'a ast_kind -> string -> 'a -> unit

val file : formatter -> tool_name:string -> string ->
  (Lexing.lexbuf -> 'a) -> 'a ast_kind -> 'a

val apply_rewriters: ?restore:bool -> tool_name:string ->
  'a ast_kind -> 'a -> 'a
  (** If [restore = true] (the default), cookies set by external
      rewriters will be kept for later calls. *)

val apply_rewriters_str:
  ?restore:bool -> tool_name:string -> Parsetree.structure ->
  Parsetree.structure
val apply_rewriters_sig:
  ?restore:bool -> tool_name:string -> Parsetree.signature ->
  Parsetree.signature

val report_error : formatter -> error -> unit


val parse_implementation:
  formatter -> tool_name:string -> string -> Parsetree.structure
val parse_interface:
  formatter -> tool_name:string -> string -> Parsetree.signature

(* [call_external_preprocessor sourcefile pp] *)
val call_external_preprocessor : string -> string -> string
val open_and_check_magic : string -> string -> in_channel * bool

module ImplementationHooks : Misc.HookSig with type t = Parsetree.structure
module InterfaceHooks : Misc.HookSig with type t = Parsetree.signature
