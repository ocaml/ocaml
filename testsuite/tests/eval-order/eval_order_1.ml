(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                    Pierre Chambart, OCamlPro                        *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


let pr x = Printf.printf "%s\n%!" x

let f x =
  ignore (pr "3 (f)");
  fun y -> ()

let g x y = ()

let () =
  f (pr "2") (pr "1");
  g (pr "5") (pr "4")

let () =
  let a = pr "6" and b = pr "7" in
  ()
