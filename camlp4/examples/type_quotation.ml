(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

open Camlp4.PreCast;

value rec mk_tuple _loc t n =
  if n <= 1 then t else <:ctyp< $t$ * $mk_tuple _loc t (n - 1)$ >>;

value ctyp_eoi = Gram.Entry.mk "ctyp eoi";

EXTEND Gram
  ctyp_eoi: [[ t = Syntax.ctyp; `EOI -> t ]];
END;

value exp _loc _ s =
  Scanf.sscanf s " %d | %[^!]" begin fun n s ->
    let t = Syntax.Gram.parse_string ctyp_eoi _loc(* not accurate *) s in
    <:ctyp< $tup:mk_tuple _loc t n$ >>
  end;

Quotation.add "power" Quotation.DynAst.ctyp_tag exp;
