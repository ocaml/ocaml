(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Gabriel Radanne                             *)
(*                                                                        *)
(*   Copyright 2018 Gabriel Radanne                                       *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Common compilation pipeline between bytecode and native. *)

(** {2 Initialization} *)

type info = {
  target : Unit_info.t;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}
(** Information needed to compile a file. *)

val with_info :
  native:bool ->
  tool_name:string ->
  dump_ext:string ->
  Unit_info.t ->
  (info -> 'a) -> 'a
(** [with_info ~native ~tool_name ~dump_ext unit_info k] invokes its
    continuation [k] with an [info] structure passed as input, after
    initializing various global variables. This info structure and the
    initialized global state are not valid anymore after the continuation
    returns.

   Due to current implementation limitations in the compiler, it is
   unsafe to try to compile several distinct compilation units by
   calling [with_info] several times.
*)

(** {2 Interfaces} *)

val parse_intf : info -> Parsetree.signature
(** [parse_intf info] parses an interface (usually an [.mli] file). *)

val typecheck_intf :
  info -> Parsetree.signature -> Misc.alerts * Typedtree.signature
(** [typecheck_intf info parsetree] typechecks an interface and returns
    the typedtree of the associated signature.
*)

val emit_signature : info -> Misc.alerts -> Typedtree.signature -> unit
(** [emit_signature info parsetree typedtree] emits the [.cmi] file
    containing the given signature.
*)

val interface : info -> unit
(** The complete compilation pipeline for interfaces. *)

(** {2 Implementations} *)

val parse_impl : info -> Parsetree.structure
(** [parse_impl info] parses an implementation (usually an [.ml] file). *)

val typecheck_impl : info -> Parsetree.structure -> Typedtree.implementation
(** [typecheck_impl info parsetree] typechecks an implementation and returns
    the typedtree of the associated module, its public interface, and a
    coercion against that public interface.
*)

val implementation :
  info -> backend:(info -> Typedtree.implementation -> unit) -> unit
(** The complete compilation pipeline for implementations. *)
