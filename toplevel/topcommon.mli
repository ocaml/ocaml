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

(** This module provides common implementations for internals of [Toploop], for
    bytecode and native code (see [Topeval] for the diverging parts of the
    implementation).

    You should not use it directly, refer to the functions in [Toploop] instead.
*)

(**/**)

(* Definitions for the interactive toplevel loop that are common between
   bytecode and native *)

open Format

(* Set the load paths, before running anything *)

val set_paths : ?auto_include:Load_path.auto_include_callback -> unit -> unit

(* Add directories listed in OCAMLTOP_INCLUDE_PATH to the end of the search
   path *)

val update_search_path_from_env : unit -> unit

(* Management and helpers for the execution *)

val toplevel_env : Env.t ref
        (* Typing environment for the toplevel *)
val initialize_toplevel_env : unit -> unit
        (* Initialize the typing environment for the toplevel *)
val preprocess_phrase :
      formatter -> Parsetree.toplevel_phrase ->  Parsetree.toplevel_phrase
        (* Preprocess the given toplevel phrase using regular and ppx
           preprocessors. Return the updated phrase. *)
val record_backtrace : unit -> unit


(* Printing of values *)

val find_eval_phrase :
  Typedtree.structure ->
    (Typedtree.expression * Typedtree.attributes * Location.t) option

val max_printer_depth: int ref
val max_printer_steps: int ref

val print_out_value :
  (formatter -> Outcometree.out_value -> unit) ref
val print_out_type :
  (formatter -> Outcometree.out_type -> unit) ref
val print_out_class_type :
  (formatter -> Outcometree.out_class_type -> unit) ref
val print_out_module_type :
  (formatter -> Outcometree.out_module_type -> unit) ref
val print_out_type_extension :
  (formatter -> Outcometree.out_type_extension -> unit) ref
val print_out_sig_item :
  (formatter -> Outcometree.out_sig_item -> unit) ref
val print_out_signature :
  (formatter -> Outcometree.out_sig_item list -> unit) ref
val print_out_phrase :
  (formatter -> Outcometree.out_phrase -> unit) ref


exception Undefined_global of string

module type EVAL_BASE = sig

  (* Return the value referred to by a base ident
     @raise [Undefined_global] if not found *)
  val eval_ident: Ident.t -> Obj.t

end


module MakeEvalPrinter (_ : EVAL_BASE) : sig

  val eval_address: Env.address -> Obj.t
    (* Used for printers *)

  val eval_module_path: Env.t -> Path.t -> Obj.t
  val eval_value_path: Env.t -> Path.t -> Obj.t
  val eval_extension_path: Env.t -> Path.t -> Obj.t
  val eval_class_path: Env.t -> Path.t -> Obj.t
    (* Return the toplevel object referred to by the given path *)

  module Printer: Genprintval.S with type t = Obj.t

  val print_value: Env.t -> Printer.t -> formatter -> Types.type_expr -> unit

  val print_untyped_exception: formatter -> Printer.t -> unit

  val print_exception_outcome : formatter -> exn -> unit
    (* Print an exception resulting from the evaluation of user code. *)

  val outval_of_value:
    Env.t -> Printer.t -> Types.type_expr -> Outcometree.out_value

  type ('a, 'b) gen_printer =
    | Zero of 'b
    | Succ of ('a -> ('a, 'b) gen_printer)

  val install_printer :
    Path.t -> Types.type_expr -> (formatter -> Printer.t -> unit) -> unit
  val install_generic_printer :
    Path.t -> Path.t ->
    (int -> (int -> Printer.t -> Outcometree.out_value,
            Printer.t-> Outcometree.out_value) gen_printer) -> unit
  val install_generic_printer' :
    Path.t -> Path.t -> (formatter -> Printer.t -> unit,
                         formatter -> Printer.t -> unit) gen_printer -> unit
  val remove_printer : Path.t -> unit

end


(* Interface with toplevel directives *)

type directive_fun =
  | Directive_none of (unit -> unit)
  | Directive_string of (string -> unit)
  | Directive_int of (int -> unit)
  | Directive_ident of (Longident.t -> unit)
  | Directive_bool of (bool -> unit)

type directive_info = {
  section: string;
  doc: string;
}

(* Add toplevel directive and its documentation.
   @since 4.03 *)
val add_directive : string -> directive_fun -> directive_info -> unit

val get_directive : string -> directive_fun option

val get_directive_info : string -> directive_info option

val all_directive_names : unit -> string list

val try_run_directive :
  formatter -> string -> Parsetree.directive_argument option -> bool

val[@deprecated] directive_table : (string, directive_fun) Hashtbl.t
  (* @deprecated please use [add_directive] instead of inserting
     in this table directly. *)

val[@deprecated] directive_info_table : (string, directive_info) Hashtbl.t
  (* @deprecated please use [add_directive] instead of inserting
     in this table directly. *)

(* Hooks for external parsers and printers *)

val parse_toplevel_phrase : (Lexing.lexbuf -> Parsetree.toplevel_phrase) ref
val parse_use_file : (Lexing.lexbuf -> Parsetree.toplevel_phrase list) ref
val print_location : formatter -> Location.t -> unit
val print_error : formatter -> Location.error -> unit
val print_warning : Location.t -> formatter -> Warnings.t -> unit
val input_name : string ref

(* Hooks for external line editor *)

(* Phrase buffer that stores the last toplevel phrase (see
   [Location.input_phrase_buffer]). *)
val phrase_buffer : Buffer.t

val first_line : bool ref

val got_eof : bool ref

val read_interactive_input : (string -> bytes -> int -> int * bool) ref

(* Hooks *)

val toplevel_startup_hook : (unit -> unit) ref

type event = ..
type event +=
  | Startup
  | After_setup
  (* Just after the setup, when the toplevel is ready to evaluate user
     input. This happens before the toplevel has evaluated any kind of
     user input, in particular this happens before loading the
     [.ocamlinit] file. *)

val add_hook : (event -> unit) -> unit
(* Add a function that will be called at key points of the toplevel
   initialization process. *)

val run_hooks : event -> unit
(* Run all the registered hooks. *)

(* Misc *)

val override_sys_argv : string array -> unit
(* [override_sys_argv args] replaces the contents of [Sys.argv] by [args]
   and reset [Arg.current] to [0].

   This is called by [run_script] so that [Sys.argv] represents
   "script.ml args..." instead of the full command line:
   "ocamlrun unix.cma ... script.ml args...". *)

(** [is_command_like_name s] is [true] if [s] is an implicit basename with no
    file extension and which doesn't begin with a hyphen. Basically, if it looks
    like a sub-command name (e.g. ocaml help). *)
val is_command_like_name : string -> bool

(**/**)

(* internal functions used by [Topeval] *)

type evaluation_outcome = Result of Obj.t | Exception of exn

val backtrace: string option ref

val parse_mod_use_file:
  string -> Lexing.lexbuf -> Parsetree.toplevel_phrase list

val refill_lexbuf: bytes -> int -> int
