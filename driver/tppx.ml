(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            John Whitington                             *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* See driver/pparse.ml for similarities with plain PPX infrastructure *)

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

(* For now, we do not add a separate magic number for the Typed AST *)
let magic = Config.ast_impl_magic_number

let apply_rewriter fn_in tppx =
  let fn_out = Filename.temp_file "camltppx" "" in
  let comm =
    Printf.sprintf "%s %s %s" tppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    prerr_endline "Cannot run command";
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then
    begin
      prerr_endline "File does not exist";
    raise (Error (WrongMagic comm));
    end;
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    prerr_endline "magic is wrong";
    raise (Error (WrongMagic comm));
  end;
  fn_out

let write_tstr fn tstr =
  let oc = open_out_bin fn in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc tstr;
  close_out oc

let read_tstr fn =
  let ic = open_in_bin fn in
    try
      let buffer = really_input_string ic (String.length magic) in
      assert (buffer = magic); (* already checked by apply_rewriter *)
      Location.input_name := (input_value ic : string);
      let tstr = (input_value ic : Typedtree.structure) in
      close_in ic;
      Misc.remove_file fn;
      tstr
    with exn ->
       close_in ic;
       Misc.remove_file fn;
       raise exn

let rewrite ppxs tstr =
  let fn = Filename.temp_file "camltppx" "" in
  write_tstr fn tstr;
  let fn = List.fold_left apply_rewriter fn (List.rev ppxs) in
  read_tstr fn

let process_all tstr =
  match !Clflags.all_tppx with
  | [] -> tstr
  | tppxs -> rewrite tppxs tstr

