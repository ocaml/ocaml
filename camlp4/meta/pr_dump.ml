(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value open_out_file () =
  match Pcaml.output_file.val with
  [ Some f -> open_out_bin f
  | None -> do { set_binary_mode_out stdout True; stdout } ]
;

value interf ast =
  let pt = Ast2pt.interf (List.map fst ast) in
  let oc = open_out_file () in
  let fname = Pcaml.input_file.val in
  do {
    output_string oc Config.ast_intf_magic_number;
    output_value oc (if fname = "-" then "" else fname);
    output_value oc pt;
    flush oc;
    match Pcaml.output_file.val with
    [ Some _ -> close_out oc
    | None -> () ]
  }
;

value implem ast =
  let pt = Ast2pt.implem (List.map fst ast) in
  let oc = open_out_file () in
  let fname = Pcaml.input_file.val in
  do {
    output_string oc Config.ast_impl_magic_number;
    output_value oc (if fname = "-" then "" else fname);
    output_value oc pt;
    flush oc;
    match Pcaml.output_file.val with
    [ Some _ -> close_out oc
    | None -> () ]
  }
;

Pcaml.print_interf.val := interf;
Pcaml.print_implem.val := implem;
