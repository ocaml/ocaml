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
open Camlp4;                                       (* -*- camlp4r -*- *)

module Id = struct
  value name = "Camlp4AstLoader";
  value version = Sys.ocaml_version;
end;

module Make (Ast : Camlp4.Sig.Ast) = struct
  module Ast = Ast;

  value parse ast_magic ?directive_handler:(_) _loc strm =
    let str =
      let buf = Buffer.create 2047 in
      let () = Stream.iter (Buffer.add_char buf) strm
      in Buffer.contents buf in
    let magic_len = String.length ast_magic in
    let buffer = String.create magic_len in
    do {
      String.blit str 0 buffer 0 magic_len;
      if buffer = ast_magic then ()
      else failwith (Format.sprintf "Bad magic: %S vs %S" buffer ast_magic);
      Marshal.from_string str magic_len;
    };

  open Camlp4.PreCast;
  value parse_implem = parse Camlp4_config.camlp4_ast_impl_magic_number;
  value parse_interf = parse Camlp4_config.camlp4_ast_intf_magic_number;

end;

let module M = Camlp4.Register.Parser Id Make in ();
