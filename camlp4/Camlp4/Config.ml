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
(* camlp4r *)

value standard_library_default = Camlp4_config.libdir;

value ocaml_standard_library =
  try Sys.getenv "OCAMLLIB"
  with [ Not_found ->
    try Sys.getenv "CAMLLIB"
    with [ Not_found ->
      standard_library_default ] ];

value camlp4_standard_library =
  try Sys.getenv "CAMLP4LIB"
  with [ Not_found -> 
          Filename.concat (try Sys.getenv "OCAMLLIB"
                           with [ Not_found -> 
                             try Sys.getenv "CAMLLIB"
                             with [ Not_found -> 
                               standard_library_default]])
                          "camlp4"];

value version = Sys.ocaml_version;
value program_name = ref "camlp4";
value constructors_arity = ref True;
value unsafe             = ref False;
value verbose            = ref False;
value quotations         = ref True;
value inter_phrases      = ref None;
value camlp4_ast_impl_magic_number = "Camlp42006M001";
value camlp4_ast_intf_magic_number = "Camlp42006N001";
value ocaml_ast_intf_magic_number = Camlp4_config.ast_intf_magic_number;
value ocaml_ast_impl_magic_number = Camlp4_config.ast_impl_magic_number;
