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

(* Native toplevel dynamic loading interface *)

open Config
open Misc
open Topcommon

type[@warning "-37"] res = Ok of Obj.t | Err of string

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"

let lookup sym =
  Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym

let need_symbol sym =
  Option.is_none (Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym)

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry))
         with exn -> Exception exn)
  with
    | Exception _ as r -> r
    | Result r ->
        match Obj.magic r with
          | Ok x -> Result x
          | Err s -> fatal_error ("Toploop.dll_run " ^ s)

(* CR-soon trefis for mshinwell: copy/pasted from Optmain. Should it be shared
   or?
   mshinwell: It should be shared, but after 4.03. *)
module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let load ppf phrase_name program =
  let dll =
    if !Clflags.keep_asm_file then phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  let middle_end =
    if Config.flambda then Flambda_middle_end.lambda_to_clambda
    else Closure_middle_end.lambda_to_clambda
  in
  Asmgen.compile_implementation ~toplevel:need_symbol
    ~backend ~prefixname:filename
    ~middle_end ~ppf_dump:ppf program;
  Asmlink.call_linker_shared [filename ^ ext_obj] dll;
  Sys.remove (filename ^ ext_obj);

  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  match
    Fun.protect
      ~finally:(fun () ->
          (try Sys.remove dll with Sys_error _ -> ()))
            (* note: under windows, cannot remove a loaded dll
               (should remember the handles, close them in at_exit, and then
               remove files) *)
      (fun () -> dll_run dll phrase_name)
  with
  | res -> res
  | exception x ->
      record_backtrace ();
      Exception x

type lookup_fn = string -> Obj.t option
type load_fn =
  Format.formatter -> string -> Lambda.program -> Topcommon.evaluation_outcome
type assembler = {mutable lookup: lookup_fn; mutable load: load_fn}

let fns = {lookup; load}

let load ppf = fns.load ppf

let lookup sym = fns.lookup sym

let register_loader ~lookup ~load =
  fns.lookup <- lookup;
  fns.load <- load
