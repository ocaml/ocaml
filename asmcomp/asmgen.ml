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

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Format
open Config
open Clflags
open Misc
open Cmm

type error = Assembler_error of string

exception Error of error

let liveness ppf phrase =
  Liveness.fundecl ppf phrase; phrase

let dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase; phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let flambda_raw_clambda_dump_if ppf
      ({ Flambda_to_clambda. expr = ulambda; preallocated_blocks = _;
        structured_constants; exported = _; } as input) =
  if !dump_rawclambda then
    begin
      Format.fprintf ppf "@.clambda (before Un_anf):@.";
      Printclambda.clambda ppf ulambda;
      Symbol.Map.iter (fun sym cst ->
          Format.fprintf ppf "%a:@ %a@."
            Symbol.print sym
            Printclambda.structured_constant cst)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@.";
  input

type clambda_and_constants =
  Clambda.ulambda *
  Clambda.preallocated_block list *
  Clambda.preallocated_constant list

let raw_clambda_dump_if ppf
      ((ulambda, _, structured_constants):clambda_and_constants) =
  if !dump_rawclambda then
    begin
      Format.fprintf ppf "@.clambda (before Un_anf):@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun {Clambda.symbol; definition} ->
          Format.fprintf ppf "%s:@ %a@."
            symbol
            Printclambda.structured_constant definition)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@."

let rec regalloc ppf round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf dump_live "Liveness analysis" fd;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences ppf ();
  if !dump_prefer then Printmach.preferences ppf ();
  Coloring.allocate_registers();
  dump_if ppf dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl ppf newfd; regalloc ppf (round + 1) newfd
  end else newfd

let (++) x f = f x

let compile_fundecl (ppf : formatter) fd_cmm =
  Proc.init ();
  Reg.reset();
  let build = Compilenv.current_build () in
  fd_cmm
  ++ Timings.(accumulate_time (Selection build)) Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  ++ Timings.(accumulate_time (Comballoc build)) Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ Timings.(accumulate_time (CSE build)) CSE.fundecl
  ++ pass_dump_if ppf dump_cse "After CSE"
  ++ Timings.(accumulate_time (Liveness build)) (liveness ppf)
  ++ Timings.(accumulate_time (Deadcode build)) Deadcode.fundecl
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Timings.(accumulate_time (Spill build)) Spill.fundecl
  ++ Timings.(accumulate_time (Liveness build)) (liveness ppf)
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Timings.(accumulate_time (Split build)) Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ Timings.(accumulate_time (Liveness build)) (liveness ppf)
  ++ Timings.(accumulate_time (Regalloc build)) (regalloc ppf 1)
  ++ Timings.(accumulate_time (Linearize build)) Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Timings.(accumulate_time (Scheduling build)) Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ Timings.(accumulate_time (Emit build)) Emit.fundecl

let compile_phrase ppf p =
  if !dump_cmm then fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ppf fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ppf f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ppf ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_unit ~source_provenance _output_prefix asm_filename keep_asm
      obj_filename gen =
  let create_asm = keep_asm || not !Emitaux.binary_backend_available in
  Emitaux.create_asm_file := create_asm;
  try
    if create_asm then Emitaux.output_channel := open_out asm_filename;
    begin try
      gen ();
      if create_asm then close_out !Emitaux.output_channel;
    with exn when create_asm ->
      close_out !Emitaux.output_channel;
      if not keep_asm then remove_file asm_filename;
      raise exn
    end;
    let assemble_result =
      Timings.(time (Assemble source_provenance))
        (Proc.assemble_file asm_filename) obj_filename
    in
    if assemble_result <> 0
    then raise(Error(Assembler_error asm_filename));
    if create_asm && not keep_asm then remove_file asm_filename
  with exn ->
    remove_file obj_filename;
    raise exn

let set_export_info (ulambda, prealloc, structured_constants, export) =
  Compilenv.set_export_info export;
  (ulambda, prealloc, structured_constants)

let end_gen_implementation ?toplevel ~source_provenance ppf
    (clambda:clambda_and_constants) =
  Emit.begin_assembly ();
  clambda
  ++ Timings.(time (Cmm source_provenance)) Cmmgen.compunit
  ++ Timings.(time (Compile_phrases source_provenance))
       (List.iter (compile_phrase ppf))
  ++ (fun () -> ());
  (match toplevel with None -> () | Some f -> compile_genfuns ppf f);

  (* We add explicit references to external primitive symbols.  This
     is to ensure that the object files that define these symbols,
     when part of a C library, won't be discarded by the linker.
     This is important if a module that uses such a symbol is later
     dynlinked. *)

  compile_phrase ppf
    (Cmmgen.reference_symbols
       (List.filter (fun s -> s <> "" && s.[0] <> '%')
          (List.map Primitive.native_name !Translmod.primitive_declarations))
    );
  Emit.end_assembly ()

let flambda_gen_implementation ?toplevel ~source_provenance ~backend ppf
    (program:Flambda.program) =
  let export = Build_export_info.build_export_info ~backend program in
  let (clambda, preallocated, constants) =
    Timings.time (Flambda_pass ("backend", source_provenance)) (fun () ->
      (program, export)
      ++ Flambda_to_clambda.convert
      ++ flambda_raw_clambda_dump_if ppf
      ++ (fun { Flambda_to_clambda. expr; preallocated_blocks;
                structured_constants; exported; } ->
             (* "init_code" following the name used in
                [Cmmgen.compunit_and_constants]. *)
           Un_anf.apply expr ~what:"init_code", preallocated_blocks,
           structured_constants, exported)
      ++ set_export_info) ()
  in
  let constants =
    List.map (fun (symbol, definition) ->
        { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
          exported = true;
          definition })
      (Symbol.Map.bindings constants)
  in
  end_gen_implementation ?toplevel ~source_provenance ppf
    (clambda, preallocated, constants)

let lambda_gen_implementation ?toplevel ~source_provenance ppf
    (lambda:Lambda.program) =
  let clambda = Closure.intro lambda.main_module_block_size lambda.code in
  let preallocated_block =
    Clambda.{
      symbol = Compilenv.make_symbol None;
      exported = true;
      tag = 0;
      size = lambda.main_module_block_size;
    }
  in
  let clambda_and_constants =
    clambda, [preallocated_block], []
  in
  raw_clambda_dump_if ppf clambda_and_constants;
  end_gen_implementation ?toplevel ~source_provenance ppf clambda_and_constants

let compile_implementation_gen ?toplevel ~source_provenance prefixname
    ppf gen_implementation program =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  compile_unit ~source_provenance prefixname asmfile !keep_asm_file
      (prefixname ^ ext_obj) (fun () ->
        gen_implementation ?toplevel ~source_provenance ppf program)

let compile_implementation_clambda ?toplevel ~source_provenance prefixname
    ppf (program:Lambda.program) =
  compile_implementation_gen ?toplevel ~source_provenance prefixname
    ppf lambda_gen_implementation program

let compile_implementation_flambda ?toplevel ~source_provenance prefixname
    ~backend ppf (program:Flambda.program) =
  compile_implementation_gen ?toplevel ~source_provenance prefixname
    ppf (flambda_gen_implementation ~backend) program

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %a"
        Location.print_filename file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
