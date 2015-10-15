(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* From lambda to assembly code *)

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

let clambda_dump_if ppf
      ({ Flambda_to_clambda. expr = ulambda; preallocated_blocks = _;
        structured_constants; exported = _; } as input) =
  if !dump_clambda then
    begin
      Format.fprintf ppf "@.clambda:@.";
      Printclambda.clambda ppf ulambda;
      (* List.iter (fun (lbls,cst) -> *)
      (*     let lbl = match lbls with *)
      (*       | [] -> assert false *)
      (*       | (lbl, _) :: _ -> lbl in *)
      (*     Format.fprintf ppf "%s: %a@." lbl *)
      (*       Printclambda.structured_constant cst) *)
      (*   structured_constants *)
      Symbol.Map.iter (fun sym cst ->
          Format.fprintf ppf "%a:@ %a@."
            Symbol.print sym
            Printclambda.structured_constant cst)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@.";
  input

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
  fd_cmm
  ++ Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  ++ Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ CSE.fundecl
  ++ pass_dump_if ppf dump_cse "After CSE"
  ++ liveness ppf
  ++ Deadcode.fundecl
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Spill.fundecl
  ++ liveness ppf
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ liveness ppf
  ++ Timings.(accumulate_time Regalloc (regalloc ppf 1))
  ++ Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ Emit.fundecl

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

let prep_flambda_for_export ppf flam ~backend =
  let kind = Flambda_invariants.Lifted in
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.Starting Lift_constants:@."
  end;
  let program = Lift_constants.lift_constants flam ~backend in
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.After Lift_constants (before invariants):@ %a@."
      Flambda.print_program program
  end;
  Flambda_invariants.check_exn ~kind program;
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.After Lift_constants:@ %a@."
      Flambda.print_program program
  end;
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.Starting Share_constants:@."
  end;
  let program = Share_constants.share_constants program in
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.After Share_constants (before invariants):@ %a@."
      Flambda.print_program program
  end;
  Flambda_invariants.check_exn ~kind program;
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.After Share_constants:@ %a@."
      Flambda.print_program program
  end;
  (* let program = Inline_and_simplify.run ~never_inline:true ~backend program in *)
  (* Flambda_invariants.check_exn ~kind program; *)
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "@.After Inline_and_simplify:@ %a@."
      Flambda.print_program program
  end;
  let export = Build_export_info.build_export_info ~backend program in
  (* Compilenv.set_export_info export; *)
  (* if !Clflags.dump_flambda *)
  (* then begin *)
  (*   Format.fprintf ppf "After Build_export_info:@ %a@." *)
  (*     Flambda.print_program program *)
  (* end; *)
  Flambda_invariants.check_exn ~kind program;
(*
  Symbol.Map.iter (fun _ set_of_closures ->
      let var = Variable.create "dummy" in
      let expr : Flambda.t =
        Let (var, Set_of_closures set_of_closures, Var var)
      in
      Flambda_invariants.check_exn ~kind ~cmxfile:true expr)
    lifted_constants.Lift_constants.set_of_closures_map;
*)
  program, export

let compile_unit ~sourcefile _output_prefix asm_filename keep_asm obj_filename gen =
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
    Timings.(start (Assemble sourcefile));
    if Proc.assemble_file asm_filename obj_filename <> 0
    then raise(Error(Assembler_error asm_filename));
    Timings.(stop (Assemble sourcefile));
    if create_asm && not keep_asm then remove_file asm_filename
  with exn ->
    remove_file obj_filename;
    raise exn

let set_export_info (ulambda, prealloc, structured_constants, export) =
  Compilenv.set_export_info export;
  (ulambda, prealloc, structured_constants)

let gen_implementation ?toplevel ~sourcefile ~backend ppf flam =
  Emit.begin_assembly ();
  Timings.(start (Flambda_backend sourcefile));
  prep_flambda_for_export ppf flam ~backend
  ++ Flambda_to_clambda.convert
  ++ clambda_dump_if ppf
  (* CR mshinwell: this is ugly *)
  ++ (fun { Flambda_to_clambda. expr; preallocated_blocks;
          structured_constants; exported; } ->
        Un_anf.apply expr, preallocated_blocks, structured_constants, exported)
  ++ set_export_info
  ++ Timings.(stop_id (Flambda_backend sourcefile))
  ++ Timings.(start_id (Cmm sourcefile))
  ++ Cmmgen.compunit_and_constants
  ++ Timings.(stop_id (Cmm sourcefile))
  ++ Timings.(start_id (Compile_phrases sourcefile))
  ++ List.iter (compile_phrase ppf)
  ++ Timings.(stop_id (Compile_phrases sourcefile))
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

let compile_implementation ?toplevel ~sourcefile prefixname ~backend ppf
      flam =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  compile_unit ~sourcefile prefixname asmfile !keep_asm_file (prefixname ^ ext_obj)
    (fun () ->
       gen_implementation ?toplevel ~sourcefile ~backend ppf flam)

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
