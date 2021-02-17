(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let raw_clambda_dump_if ppf
      ((ulambda, _, structured_constants) : Clambda.with_constants) =
  if !Clflags.dump_rawclambda || !Clflags.dump_clambda then
    begin
      Format.fprintf ppf "@.clambda:@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun { Clambda. symbol; definition; _ } ->
          Format.fprintf ppf "%s:@ %a@."
            symbol
            Printclambda.structured_constant definition)
        structured_constants
    end;
  if !Clflags.dump_cmm then Format.fprintf ppf "@.cmm:@."

let lambda_to_clambda ~backend ~prefixname:_ ~ppf_dump
      (lambda : Lambda.program) =
  let clambda =
    Closure.intro ~backend ~size:lambda.main_module_block_size lambda.code
  in
  let provenance : Clambda.usymbol_provenance =
    { original_idents = [];
      module_path =
        Path.Pident (Ident.create_persistent (Compilenv.current_unit_name ()));
    }
  in
  let preallocated_block =
    Clambda.{
      symbol = Compilenv.make_symbol None;
      exported = true;
      tag = 0;
      fields = List.init lambda.main_module_block_size (fun _ -> None);
      provenance = Some provenance;
    }
  in
  let constants = Compilenv.structured_constants () in
  Compilenv.clear_structured_constants ();
  let clambda_and_constants =
    clambda, [preallocated_block], constants
  in
  raw_clambda_dump_if ppf_dump clambda_and_constants;
  clambda_and_constants
