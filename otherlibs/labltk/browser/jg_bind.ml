(*************************************************************************)
(*                                                                       *)
(*                         OCaml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tk

let enter_focus w =
  bind w ~events:[`Enter] ~action:(fun _ -> Focus.set w)

let escape_destroy ?destroy:tl w =
  let tl = match tl with Some w -> w | None -> w in
  bind w ~events:[`KeyPressDetail "Escape"] ~action:(fun _ -> destroy tl)

let return_invoke w ~button =
  bind w ~events:[`KeyPressDetail "Return"]
    ~action:(fun _ -> Button.invoke button)
