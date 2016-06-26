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

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

module Make (Proc : Proc_intf.S) : sig
val fundecl: (Proc.addressing_mode, Proc.specific_operation) Mach.fundecl -> (Proc.addressing_mode, Proc.specific_operation) Mach.fundecl
end
