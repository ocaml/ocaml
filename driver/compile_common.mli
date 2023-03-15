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
  source_file : string  (** the source file read from the CLI*);
  human_source_file : string
  (** the original source file written by a human.
      In particular, generated code should point to its real source file
      by using line directives *);
  module_name : string;
  output_prefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}
(** Information needed to compile a file. *)

val with_info :
  native:bool ->
  tool_name:string ->
  source_file:string ->
  output_prefix:string ->
  dump_ext:string ->
  (info -> 'a) -> 'a
(** [with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k]
   invokes its continuation [k] with an [info] structure built from
   its input, after initializing various global variables. This info
   structure and the initialized global state are not valid anymore
   after the continuation returns.

   Due to current implementation limitations in the compiler, it is
   unsafe to try to compile several distinct compilation units by
   calling [with_info] several times.
*)

(** {2 Interfaces} *)

val parse_intf : info -> Parsetree.signature
(** [parse_intf info] parses an interface (usually an [.mli] file). *)

val intf_human_source : Parsetree.signature -> info -> info
(** [intf_human_source ast info ] updates the information to reflect the
    human source file recorded in the parsetree (taking in account line
    directives).
*)

val typecheck_intf : info -> Parsetree.signature -> Typedtree.signature
(** [typecheck_intf info parsetree] typechecks an interface and returns
    the typedtree of the associated signature.
*)

val emit_signature : info -> Parsetree.signature -> Typedtree.signature -> unit
(** [emit_signature info parsetree typedtree] emits the [.cmi] file
    containing the given signature.
*)

val interface : info -> unit
(** The complete compilation pipeline for interfaces. *)

(** {2 Implementations} *)

val parse_impl : info -> Parsetree.structure
(** [parse_impl info] parses an implementation (usually an [.ml] file). *)

val impl_human_source : Parsetree.structure -> info -> info
(** [intf_human_source ast info ] updates the information to reflect the
    human source file recorded in the parsetree (taking in account line
    directives).
*)

val typecheck_impl : info -> Parsetree.structure -> Typedtree.implementation
(** [typecheck_impl info parsetree] typechecks an implementation and returns
    the typedtree of the associated module, its public interface, and a
    coercion against that public interface.
*)

val implementation :
  info -> backend:(info -> Typedtree.implementation -> unit) -> unit
(** The complete compilation pipeline for implementations. *)

(** {2 Build artifacts} *)

val cmo : info -> string
val cmx : info -> string
val obj : info -> string
val annot : info -> string
(** Return the filename of some compiler build artifacts associated
    with the file being compiled.
*)
