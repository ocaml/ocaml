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

(* $Id$ *)

(* From lambda to assembly code *)

open Format
open Config
open Clflags
open Misc
open Cmm

type error = Assembler_error of string

exception Error of error

let liveness phrase =
  Liveness.fundecl phrase; phrase

let dump_if flag message phrase =
  if !flag then Printmach.phase message Format.err_formatter phrase

let pass_dump_if flag message phrase =
  dump_if flag message phrase; phrase

let pass_dump_linear_if flag message phrase =
  if !flag then fprintf Format.err_formatter "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let rec regalloc round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if dump_live "Liveness analysis" fd;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences Format.err_formatter ();
  if !dump_prefer then Printmach.preferences Format.err_formatter ();
  Coloring.allocate_registers();
  dump_if dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl newfd; regalloc (round + 1) newfd
  end else newfd

let (++) x f = f x

let compile_fundecl fd_cmm =
  Reg.reset();
  fd_cmm
  ++ Selection.fundecl
  ++ pass_dump_if dump_selection "After instruction selection"
  ++ Comballoc.fundecl
  ++ pass_dump_if dump_combine "After allocation combining"
  ++ liveness
  ++ pass_dump_if dump_live "Liveness analysis"
  ++ Spill.fundecl
  ++ liveness
  ++ pass_dump_if dump_spill "After spilling"
  ++ Split.fundecl
  ++ pass_dump_if dump_split "After live range splitting"
  ++ liveness
  ++ regalloc 1
  ++ Linearize.fundecl
  ++ pass_dump_linear_if dump_linear "Linearized code"
  ++ Scheduling.fundecl
  ++ pass_dump_linear_if dump_scheduling "After instruction scheduling"
  ++ Emit.fundecl

let compile_phrase p =
  if !dump_cmm then eprintf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_implementation ?toplevel prefixname (size, lam) =
  let asmfile =
    if !keep_asm_file
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm in
  let oc = open_out asmfile in
  begin try
    Emitaux.output_channel := oc;
    Emit.begin_assembly();
    Closure.intro size lam
    ++ Cmmgen.compunit size
    ++ List.iter compile_phrase ++ (fun () -> ());
    (match toplevel with None -> () | Some f -> compile_genfuns f);

    (* We add explicit references to external primitive symbols.  This
       is to ensure that the object files that define these symbols,
       when part of a C library, won't be discarded by the linker.
       This is important if a module that uses such a symbol is later
       dynlinked. *)

    compile_phrase
      (Cmmgen.reference_symbols
         (List.filter (fun s -> s <> "" && s.[0] <> '%')
            (List.map Primitive.native_name !Translmod.primitive_declarations))
      );

    Emit.end_assembly();
    close_out oc
  with x ->
    close_out oc;
    if !keep_asm_file then () else remove_file asmfile;
    raise x
  end;
  if Proc.assemble_file asmfile (prefixname ^ ext_obj) <> 0
  then raise(Error(Assembler_error asmfile));
  if !keep_asm_file then () else remove_file asmfile

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %s" file
