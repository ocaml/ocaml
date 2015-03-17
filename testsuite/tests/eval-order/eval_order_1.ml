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
  ignore (pr "f");
  fun y -> ()

let g x y = ()

let () =
  (* f (pr "1") (pr "2"); *) (* TO fix ? *)
  g (pr "3") (pr "4")
