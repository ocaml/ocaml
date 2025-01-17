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
open Diagnostic
open Compiler_diagnostic
include New_record(V)
    (struct
      let name = "toplevel"
      let description = "OCaml Toplevel diagnostic"
      let update = v1
    end)
    ()
let output = new_field_opt v1 "output" doc
let backtrace = new_field_opt v1 "backtrace" doc
let compiler = new_field_opt v1 "compiler" Compiler_diagnostic.raw_type
let errors = new_field_opt v1 "errors" ldoc
let trace = new_field_opt v1 "trace" ldoc
let () = seal v1

let separate_new_message = Location.separate_new_message
