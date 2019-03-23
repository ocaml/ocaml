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

let liveness phrase = Liveness.fundecl phrase; phrase

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
  if !dump_rawclambda || !dump_clambda then
    begin
      Format.fprintf ppf "@.clambda:@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun {Clambda.symbol; definition} ->
          Format.fprintf ppf "%s:@ %a@."
            symbol
            Printclambda.structured_constant definition)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@."

let rec regalloc ~ppf_dump round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf_dump dump_live "Liveness analysis" fd;
  if !use_linscan then begin
    (* Linear Scan *)
    Interval.build_intervals fd;
    if !dump_interval then Printmach.intervals ppf_dump ();
    Linscan.allocate_registers()
  end else begin
    (* Graph Coloring *)
    Interf.build_graph fd;
    if !dump_interf then Printmach.interferences ppf_dump ();
    if !dump_prefer then Printmach.preferences ppf_dump ();
    Coloring.allocate_registers()
  end;
  dump_if ppf_dump dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf_dump dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl newfd; regalloc ~ppf_dump (round + 1) newfd
  end else newfd

let (++) x f = f x

let compile_fundecl ~ppf_dump fd_cmm =
  Proc.init ();
  Cmmgen.reset ();
  Reg.reset();
  fd_cmm
  ++ Profile.record ~accumulate:true "selection" Selection.fundecl
  ++ pass_dump_if ppf_dump dump_selection "After instruction selection"
  ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
  ++ pass_dump_if ppf_dump dump_combine "After allocation combining"
  ++ Profile.record ~accumulate:true "cse" CSE.fundecl
  ++ pass_dump_if ppf_dump dump_cse "After CSE"
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ Profile.record ~accumulate:true "deadcode" Deadcode.fundecl
  ++ pass_dump_if ppf_dump dump_live "Liveness analysis"
  ++ Profile.record ~accumulate:true "spill" Spill.fundecl
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ pass_dump_if ppf_dump dump_spill "After spilling"
  ++ Profile.record ~accumulate:true "split" Split.fundecl
  ++ pass_dump_if ppf_dump dump_split "After live range splitting"
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ Profile.record ~accumulate:true "regalloc" (regalloc ~ppf_dump 1)
  ++ Profile.record ~accumulate:true "available_regs" Available_regs.fundecl
  ++ Profile.record ~accumulate:true "linearize" Linearize.fundecl
  ++ pass_dump_linear_if ppf_dump dump_linear "Linearized code"
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if ppf_dump dump_scheduling "After instruction scheduling"
  ++ Profile.record ~accumulate:true "emit" Emit.fundecl

let compile_phrase ~ppf_dump p =
  if !dump_cmm then fprintf ppf_dump "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ~ppf_dump fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ~ppf_dump f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ~ppf_dump ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_unit _output_prefix asm_filename keep_asm
      obj_filename gen =
  let create_asm = keep_asm || not !Emitaux.binary_backend_available in
  Emitaux.create_asm_file := create_asm;
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
       if create_asm then Emitaux.output_channel := open_out asm_filename;
       Misc.try_finally gen
         ~always:(fun () ->
             if create_asm then close_out !Emitaux.output_channel)
         ~exceptionally:(fun () ->
             if create_asm && not keep_asm then remove_file asm_filename);
       let assemble_result =
         Profile.record "assemble"
           (Proc.assemble_file asm_filename) obj_filename
       in
       if assemble_result <> 0
       then raise(Error(Assembler_error asm_filename));
       if create_asm && not keep_asm then remove_file asm_filename
    )

let set_export_info (ulambda, prealloc, structured_constants, export) =
  Compilenv.set_export_info export;
  (ulambda, prealloc, structured_constants)

let end_gen_implementation ?toplevel ~ppf_dump
    (clambda:clambda_and_constants) =
  Emit.begin_assembly ();
  clambda
  ++ Profile.record "cmm" (Cmmgen.compunit ~ppf_dump)
  ++ Profile.record "compile_phrases" (List.iter (compile_phrase ~ppf_dump))
  ++ (fun () -> ());
  (match toplevel with None -> () | Some f -> compile_genfuns ~ppf_dump f);

  (* We add explicit references to external primitive symbols.  This
     is to ensure that the object files that define these symbols,
     when part of a C library, won't be discarded by the linker.
     This is important if a module that uses such a symbol is later
     dynlinked. *)

  compile_phrase ~ppf_dump
    (Cmmgen.reference_symbols
       (List.filter (fun s -> s <> "" && s.[0] <> '%')
          (List.map Primitive.native_name !Translmod.primitive_declarations))
    );
  Emit.end_assembly ()

let flambda_gen_implementation ?toplevel ~backend ~ppf_dump
    (program:Flambda.program) =
  let export = Build_export_info.build_transient ~backend program in
  let (clambda, preallocated, constants) =
    Profile.record_call "backend" (fun () ->
      (program, export)
      ++ Flambda_to_clambda.convert
      ++ flambda_raw_clambda_dump_if ppf_dump
      ++ (fun { Flambda_to_clambda. expr; preallocated_blocks;
                structured_constants; exported; } ->
             (* "init_code" following the name used in
                [Cmmgen.compunit_and_constants]. *)
           Un_anf.apply ~ppf_dump expr ~what:"init_code", preallocated_blocks,
           structured_constants, exported)
      ++ set_export_info)
  in
  let constants =
    List.map (fun (symbol, definition) ->
        { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
          exported = true;
          definition;
          provenance = None;
        })
      (Symbol.Map.bindings constants)
  in
  end_gen_implementation ?toplevel ~ppf_dump
    (clambda, preallocated, constants)

let lambda_gen_implementation ?toplevel ~ppf_dump
    (lambda:Lambda.program) =
  let clambda = Closure.intro lambda.main_module_block_size lambda.code in
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
  let clambda_and_constants =
    clambda, [preallocated_block], Compilenv.structured_constants ()
  in
  Compilenv.clear_structured_constants ();
  raw_clambda_dump_if ppf_dump clambda_and_constants;
  end_gen_implementation ?toplevel ~ppf_dump clambda_and_constants

let compile_implementation_gen ?toplevel prefixname
    ~required_globals ~ppf_dump gen_implementation program =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  compile_unit prefixname asmfile !keep_asm_file
      (prefixname ^ ext_obj) (fun () ->
        Ident.Set.iter Compilenv.require_global required_globals;
        gen_implementation ?toplevel ~ppf_dump program)

let compile_implementation_clambda ?toplevel prefixname
    ~ppf_dump (program:Lambda.program) =
  compile_implementation_gen ?toplevel prefixname
    ~required_globals:program.Lambda.required_globals
    ~ppf_dump lambda_gen_implementation program

let compile_implementation_flambda ?toplevel prefixname
    ~required_globals ~backend ~ppf_dump (program:Flambda.program) =
  compile_implementation_gen ?toplevel prefixname
    ~required_globals ~ppf_dump (flambda_gen_implementation ~backend) program

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
