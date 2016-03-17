(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CSE for Sparc *)

open Mach
open CSEgen

class cse = object

inherit cse_generic (* as super *)

method! is_cheap_operation op =
  match op with
  | Iconst_int n | Iconst_blockheader n -> n <= 4095n && n >= -4096n
  | _ -> false

end

let fundecl f =
  (new cse)#fundecl f
