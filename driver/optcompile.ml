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

(** The batch compiler *)

open Misc
open Compile_common

let tool_name = "ocamlopt"

let interface = Compile_common.interface ~tool_name

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

let flambda i backend typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda i.modulename)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda i.sourcefile
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, size), lam) ->
      Middle_end.middle_end
        ~ppf_dump:i.ppf_dump
        ~prefixname:i.outputprefix
        ~size
        ~filename:i.sourcefile
        ~module_ident
        ~backend
        ~module_initializer:lam)
    |> Asmgen.compile_implementation_flambda
      i.outputprefix ~unit_name:module_ident ~required_globals ~backend
        ~ppf_dump:i.ppf_dump;
    Compilenv.save_unit_info (cmx i))

let clambda i typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.modulename)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda i.sourcefile program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation_clambda
         i.outputprefix ~unit_name:program.Lambda.module_ident
         ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

let implementation ~backend =
  Compile_common.implementation ~tool_name
    ~native:true ~backend:(fun info typed ->
      Compilenv.reset ?packname:!Clflags.for_package info.modulename;
      if Config.flambda
      then flambda info backend typed
      else clambda info typed
    )
