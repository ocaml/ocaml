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

(** Internal signature for sematantic actions of grammars,
    not for the casual user. These functions are unsafe. *)
module type S = sig
  type  t     = 'abstract;

  value mk    : 'a ->  t;
  value get   :  t -> 'a;
  value getf  :  t -> ('a -> 'b);
  value getf2 :  t -> ('a -> 'b -> 'c);
end;
