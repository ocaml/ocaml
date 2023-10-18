(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Compile_common

let tool_name = "ocamlc"

let with_info =
  Compile_common.with_info ~native:false ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

(** Bytecode compilation backend for .ml files. *)

let to_bytecode i Typedtree.{impl_structure = structure; impl_coercion = coercion; _} =
  (structure, coercion)
  |> Profile.(record transl)
    (Translmod.transl_implementation (Unit_info.modname i.target))
  |> Profile.(record ~accumulate:true generate)
    (fun { Lambda.code = lambda; required_globals } ->
       lambda
       |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
       |> Simplif.simplify_lambda
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
       |> Bytegen.compile_implementation (Unit_info.modname i.target)
       |> print_if i.ppf_dump Clflags.dump_instr Printinstr.instrlist
       |> fun bytecode -> bytecode, required_globals
    )

let emit_bytecode i (bytecode, required_globals) =
  let cmo = Unit_info.cmo i.target in
  let oc = open_out_bin (Unit_info.Artifact.filename cmo) in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () ->
       Misc.remove_file (Unit_info.Artifact.filename cmo)
    )
    (fun () ->
       bytecode
       |> Profile.(record ~accumulate:true generate)
         (Emitcode.to_file oc cmo ~required_globals);
    )

let implementation ~start_from ~source_file ~output_prefix =
  let backend info typed =
    let bytecode = to_bytecode info typed in
    emit_bytecode info bytecode
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
  match (start_from : Clflags.Compiler_pass.t) with
  | Parsing -> Compile_common.implementation info ~backend
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
