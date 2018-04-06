(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interface for ocamltest's configuration module *)

val arch : string
(** Architecture for the native compiler, "none" if it is disabled *)

val afl_instrument : bool
(** Whether AFL support has been enabled in the compiler *)

val shared_libraries : bool
(** [true] if shared libraries are supported, [false] otherwise *)

val libunix : bool
(** [true] for unix, [false] for win32unix *)

val system : string
(** The content of the SYSTEM Make variable *)

val c_preprocessor : string
(** Command to use to invoke the C preprocessor *)

val ocamlc_default_flags : string
(** Flags passed by default to ocamlc.byte and ocamlc.opt *)

val ocamlopt_default_flags : string
(** Flags passed by default to ocamlopt.byte and ocamlopt.opt *)

val ocamlsrcdir : string
(** The absolute path of the directory containing the sources of OCaml *)

val flambda : bool
(** Whether flambda has been enabled at configure time *)

val spacetime : bool
(** Whether Spacetime profiling has been enabled at configure time *)

val safe_string : bool
(** Whether the compiler was configured with -safe-string *)

val flat_float_array : bool
(* Whether the compiler was configured with -flat-float-array *)

val ocamldoc : bool
(** Whether ocamldoc has been enabled at configure time *)

val ocamldebug : bool
(** Whether ocamldebug has been enabled at configure time *)
