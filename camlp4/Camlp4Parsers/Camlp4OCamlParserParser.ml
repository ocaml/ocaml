(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 1998-2006 Institut National de Recherche en Informatique et   *)
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


open Camlp4;

module Id : Sig.Id = struct
  value name = "Camlp4OCamlParserParser";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  module M = Camlp4OCamlRevisedParserParser.Make Syntax;
  open M;

  Gram.Entry.clear stream_expr;
  Gram.Entry.clear stream_begin;
  Gram.Entry.clear stream_end;
  Gram.Entry.clear stream_quot;
  Gram.Entry.clear parser_case_list;

  EXTEND Gram
    stream_expr:
      [ [ e = expr LEVEL "top" -> e ] ]
    ;
    stream_begin:
      [ [ "[<" -> () ] ]
    ;
    stream_end:
      [ [ ">]" -> () ] ]
    ;
    stream_quot:
      [ [ "'" -> () ] ]
    ;
    parser_case_list:
      [ [ OPT "|"; pcl = LIST1 parser_case SEP "|" -> pcl ] ]
    ;
  END;
end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
