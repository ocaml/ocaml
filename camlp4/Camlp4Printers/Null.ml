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
 * - Nicolas Pouillard: initial version
 *)

open Camlp4;

module Id = struct
  value name = "Camlp4Printers.Null";
  value version = "$Id$";
end;

module Make (Syntax : Sig.Syntax.S) = struct
  include Syntax;
  
  value print_interf ?input_file:(_) ?output_file:(_) _ = ();
  value print_implem ?input_file:(_) ?output_file:(_) _ = ();
end;

let module M = Register.Printer Id Make in ();
