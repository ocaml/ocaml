(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the class language *)

open Types
open Ctype

val class_types:
        Env.t -> class_type -> class_type -> class_match_failure list
val class_type_declarations:
  loc:Location.t ->
  Env.t -> class_type_declaration -> class_type_declaration ->
  class_match_failure list
val class_declarations:
  Env.t -> class_declaration -> class_declaration ->
  class_match_failure list

val report_error :
  Out_type.type_or_scheme -> class_match_failure list Format_doc.format_printer
val report_error_doc :
  Out_type.type_or_scheme -> class_match_failure list Format_doc.printer
