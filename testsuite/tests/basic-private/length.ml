(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*              Pierre Weis, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*

A testbed file for private type abbreviation definitions.

We define a Length module to implement positive integers.

*)

type t = int;;

let make x =
  if x >= 0 then x else
  failwith (Printf.sprintf "cannot build negative length : %i" x)
;;

external from : t -> int = "%identity";;
