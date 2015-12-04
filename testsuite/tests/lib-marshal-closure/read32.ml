(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                         Benoit Vaugon, ENSTA                        *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let fname = "data";;

let f () = [ 1; 2; 3; 4 ];;

try
  let ic = open_in_bin fname in
  let g : unit -> float list = Marshal.from_channel ic in
  close_in ic;
  Printf.printf "%f\n" (List.nth (g ()) 0);
with
| Sys_error _ ->
  Printf.printf "Do not import %S, ok\n" fname;
| Failure msg when String.sub msg 0 32 = "input_value: unknown code module" ->
  Printf.printf "Incompatible closure, ok\n";
;;

let oc = open_out_bin fname in
Marshal.to_channel oc f [ Marshal.Closures ];
close_out oc;;
