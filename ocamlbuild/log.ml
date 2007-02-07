(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Nicolas Pouillard *)
open My_std

module Debug = struct
let mode _ = true
end
include Debug

let level = ref 1

let classic_display = ref false
let log_file = ref (lazy None)

let internal_display = lazy begin
  let mode =
    if !classic_display || !*My_unix.is_degraded || !level <= 0 || not (My_unix.stdout_isatty ()) then
      `Classic
    else
      `Sophisticated
  in
  Display.create ~mode ?log_file:!*(!log_file) ~log_level:!level ()
end

let raw_dprintf log_level = Display.dprintf ~log_level !*internal_display

let dprintf log_level fmt = raw_dprintf log_level ("@[<2>"^^fmt^^"@]@.")
let eprintf fmt = dprintf (-1) fmt

let update () = Display.update !*internal_display
let event ?pretend x = Display.event !*internal_display ?pretend x
let display x = Display.display !*internal_display x

let finish ?how () =
  if Lazy.lazy_is_val internal_display then
    Display.finish ?how !*internal_display

(*let () = My_unix.at_exit_once finish*)
