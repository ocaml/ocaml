open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

open PreCast;
let module Gram = MakeGram Lexer in
let module M1 = OCamlInitSyntax.Make Ast Gram Quotation in
let module M2 = Camlp4OCamlRevisedParser.Make M1 in
let module M3 = Camlp4QuotationCommon.Make M2 Syntax.AntiquotSyntax in ();
