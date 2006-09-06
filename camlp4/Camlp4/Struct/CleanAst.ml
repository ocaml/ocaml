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

(** This module is suppose to contain nils elimination. *)
module Make (Ast : Sig.Camlp4Ast.S) = struct
  class clean_ast = object (self)
    inherit Ast.map as super;
    method ctyp =
      fun
      [ <:ctyp< $t$; $ <:ctyp<>> $ >> | 
        <:ctyp< $ <:ctyp<>> $; $t$ >> |
        <:ctyp< $t$ | $ <:ctyp<>> $ >> | 
        <:ctyp< $ <:ctyp<>> $ | $t$ >> -> self#ctyp t
      | t -> super#ctyp t ];
  end;
end;
