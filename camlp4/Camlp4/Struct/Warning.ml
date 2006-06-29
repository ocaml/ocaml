(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
module Make (Loc : Sig.Loc.S) : Sig.Warning.S with module Loc = Loc = struct
  module Loc = Loc;
  open Format;
  type t = Loc.t -> string -> unit;
  value default loc txt = eprintf "<W> %a: %s@." Loc.print loc txt;
  value current = ref default;
  value print loc txt = current.val loc txt;
end;
