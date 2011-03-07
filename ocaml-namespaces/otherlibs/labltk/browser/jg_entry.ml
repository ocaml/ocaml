(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
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

let create ?command ?width ?textvariable parent =
  let ew = Entry.create parent ?width ?textvariable in
  Jg_bind.enter_focus ew;
  begin match command with Some command ->
    bind ew ~events:[`KeyPressDetail "Return"]
      ~action:(fun _ -> command (Entry.get ew))
  | None -> ()
  end;
  ew
