(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Infrastructure to support user-defined printers in toplevels and debugger *)

let printertypes_str = {|
  type 'a printer_type_new = Format.formatter -> 'a -> unit
  type 'a printer_type_old = 'a -> unit
|}

let printertypes_sig_ast =
  Parse.interface (Lexing.from_string printertypes_str)

let printertypes_sig env =
  (Typemod.transl_signature env printertypes_sig_ast).sig_type

let printer_types sign =
  match sign with
  | [Types.Sig_type(new_name,_,_,_); Types.Sig_type(old_name,_,_,_)] ->
    (Path.Pident new_name, Path.Pident old_name)
  | _ -> assert false

let env_with_printer_types env =
  let tmp_sign = printertypes_sig env in
  let tmp_env = Env.add_signature tmp_sign env in
  let printer_type_new, printer_type_old = printer_types tmp_sign in
  (tmp_env, printer_type_new, printer_type_old)  
