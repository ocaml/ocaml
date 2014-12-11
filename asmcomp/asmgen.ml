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

let clambda_dump_if ppf ulambda =
  if !dump_clambda then
    begin
      Printclambda.clambda ppf ulambda;
      List.iter (fun (lbls,cst) ->
          let lbl = match lbls with
            | [] -> assert false
            | (lbl, _) :: _ -> lbl in
          Format.fprintf ppf "%s: %a@." lbl
            Printclambda.structured_constant cst)
        (Compilenv.structured_constants ())
    end;
  ulambda

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
  ++ regalloc ppf 1
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

let flambda ppf (size, lam) =
  let current_compilation_unit = Compilenv.current_unit () in
  let dump_and_check s flam =
    if !Clflags.dump_flambda
    then Format.fprintf ppf "%s:@ %a@." s Printflambda.flambda flam;
    try Flambdacheck.check ~current_compilation_unit flam
    with e ->
      Format.fprintf ppf "%a@."
        Printflambda.flambda flam;
      raise e in
  let _, flam =
    Flambdagen.intro
      ~current_compilation_unit
      ~current_unit_id:(Compilenv.current_unit_id ())
      ~symbol_for_global':Compilenv.symbol_for_global'
      lam in
  dump_and_check "flambdagen" flam;
  let rec loop rounds flam =
    if rounds <= 0 then flam
    else
      let flam = Flambdasimplify.lift_lets flam in
      let flam = Flambdasimplify.simplify flam in
      let flam = Flambda_ref_to_variables.eliminate_ref flam in
      loop (rounds - 1) flam in
  let flam = loop !Clflags.simplify_rounds flam in
  dump_and_check "flambdasimplify" flam;
  let fl_sym =
    Flambdasym.convert ~compilation_unit:current_compilation_unit flam in
  let fl,const,export = fl_sym in
  Compilenv.set_export_info export;
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "flambdasym@ %a@." Printflambda.flambda fl;
    Symbol.SymbolMap.iter (fun sym lam ->
        Format.fprintf ppf "sym: %a@ %a@."
          Symbol.print sym
          Printflambda.flambda lam)
      const
  end;
  Flambdacheck.check ~current_compilation_unit ~flambdasym:true fl;
  Symbol.SymbolMap.iter (fun _ lam ->
      Flambdacheck.check ~current_compilation_unit
        ~flambdasym:true ~cmxfile:true lam)
    const;
  Clambdagen.convert fl_sym

let compile_unit asm_filename keep_asm obj_filename gen =
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
    if Proc.assemble_file asm_filename obj_filename <> 0
    then raise(Error(Assembler_error asm_filename));
    if create_asm && not keep_asm then remove_file asm_filename
  with exn ->
    remove_file obj_filename;
    raise exn

let gen_implementation ?toplevel ppf (size, lam) =
  Emit.begin_assembly ();
  let clambda = flambda ppf (size, lam) in
  (* Closure.intro size lam *)
  clambda
  ++ clambda_dump_if ppf
  ++ Cmmgen.compunit size
  ++ List.iter (compile_phrase ppf) ++ (fun () -> ());
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

(*
  (* Flambdareachability.test ~current_compilation_unit flam; *)

  (* keep passes linked in the cma *)
  ignore (Flambdasimplify.passes);
  ignore (Flambdaspec.passes);
  ignore (Flambdamovelets.passes);
  ignore (Flambdaifstaticraise.passes);

  let run_pass flambda pass =
    let flambda = pass.Flambdapasses.pass flambda current_compilation_unit in
    dump_and_check pass.Flambdapasses.name flambda;
    flambda
  in
  let before = Flambdapasses.before_passes () in
  let loop = Flambdapasses.loop_passes () in
  let after = Flambdapasses.after_passes () in

  if !Clflags.dump_flambda
  then Format.fprintf ppf "@.flambda before@.";

  let flam = List.fold_left run_pass flam before in

  let flam = ref flam in
  for i = 1 to !Clflags.simplify_rounds do
    if !Clflags.dump_flambda
    then Format.fprintf ppf "@.flambda round %i@." i;
    flam := List.fold_left run_pass !flam loop
  done;

  if !Clflags.dump_flambda
  then Format.fprintf ppf "@.flambda after@.";

  let flam = List.fold_left run_pass !flam after in

  let flam =
    if Clflags.experiments
    then begin
      let flam = Flambdareachability.test ~current_compilation_unit flam in
      dump_and_check "clean unreachable calls" flam;
      let flam = Flambdamovelets.move_lets flam in
      let flam = Flambdamovelets.remove_trivial_lets flam in
      dump_and_check "move lets" flam;
      flam
    end
    else flam in

  let fl_sym =
    Flambdasym.convert ~compilation_unit:current_compilation_unit flam in
  let fl,const,export = fl_sym in
  Compilenv.set_export_info export;
  if !Clflags.dump_flambda
  then begin
    Format.fprintf ppf "flambdasym@ %a@." Printflambda.flambda fl;
    Symbol.SymbolMap.iter (fun sym lam ->
        Format.fprintf ppf "sym: %a@ %a@."
          Symbol.print sym
          Printflambda.flambda lam)
      const
  end;
  Flambdacheck.check ~current_compilation_unit ~flambdasym:true fl;
  Symbol.SymbolMap.iter (fun _ lam ->
      Flambdacheck.check ~current_compilation_unit
        ~flambdasym:true ~cmxfile:true lam)
    const;
  Clambdagen.convert fl_sym
*)

let compile_implementation ?toplevel prefixname ppf (size, lam) =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  compile_unit asmfile !keep_asm_file (prefixname ^ ext_obj)
    (fun () -> gen_implementation ?toplevel ppf (size, lam))

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
