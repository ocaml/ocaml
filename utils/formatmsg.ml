(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Formatter used for printing the compiler messages *)

open Format

let fmt = ref std_formatter

let open_hbox () = pp_open_hbox !fmt ()
let open_vbox n = pp_open_vbox !fmt n
let open_hvbox n = pp_open_hvbox !fmt n
let open_hovbox n = pp_open_hovbox !fmt n
let open_box n = pp_open_box !fmt n
let close_box () = pp_close_box !fmt ()
let print_string s = pp_print_string !fmt s
let print_as n s = pp_print_as !fmt n s
let print_int n = pp_print_int !fmt n
let print_float f = pp_print_float !fmt f
let print_char c = pp_print_char !fmt c
let print_bool b = pp_print_bool !fmt b
let print_break n1 n2 = pp_print_break !fmt n1 n2
let print_cut () = pp_print_cut !fmt ()
let print_space () = pp_print_space !fmt ()
let force_newline () = pp_force_newline !fmt ()
let print_flush () = pp_print_flush !fmt ()
let print_newline () = pp_print_newline !fmt ()
let printf f = fprintf !fmt f

let set_output f = fmt := f

let with_output_to f fn =
  let oldf = !fmt in
  fmt := f;
  try
    fn ();
    fmt := oldf
  with x ->
    fmt := oldf;
    raise x

  
