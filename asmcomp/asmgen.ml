(* From lambda to assembly code *)

open Format
open Clflags
open Misc
open Cmm

type error = Assembler_error of string

exception Error of error

let rec regalloc fd =
  if !dump_live then Printmach.phase "Liveness analysis" fd;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences();
  if !dump_prefer then Printmach.preferences();
  Coloring.allocate_registers();
  if !dump_regalloc then
    Printmach.phase "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  if !dump_reload then
    Printmach.phase "After insertion of reloading code" newfd;
  if redo_regalloc 
  then begin Reg.reinit(); Liveness.fundecl newfd; regalloc newfd end
  else newfd

let compile_fundecl fd_cmm =
  Reg.reset();
  let fd_sel = Selection.fundecl fd_cmm in
  if !dump_selection then
    Printmach.phase "After instruction selection" fd_sel;
  Liveness.fundecl fd_sel;
  if !dump_live then Printmach.phase "Liveness analysis" fd_sel;
  let fd_spill = Spill.fundecl fd_sel in
  Liveness.fundecl fd_spill;
  if !dump_spill then
    Printmach.phase "After spilling" fd_spill;
  let fd_split = Split.fundecl fd_spill in
  Liveness.fundecl fd_split;
  if !dump_split then
    Printmach.phase "After live range splitting" fd_split;
  let fd_reload = regalloc fd_split in
  let fd_linear = Linearize.fundecl fd_reload in
  if !dump_linear then begin
    print_string "*** Linearized code"; print_newline();
    Printlinear.fundecl fd_linear; print_newline()
  end;
  Emit.fundecl fd_linear

let compile_phrase p =
  if !dump_cmm then begin Printcmm.phrase p; print_newline() end;
  match p with
    Cfunction fd -> compile_fundecl fd
  | Cdata dl -> Emit.data dl

let compile_implementation prefixname lam =
  let asmfile =
    if !assembler_only then prefixname ^ ".s" else temp_file "camlasm" ".s" in
  let oc = open_out asmfile in
  begin try
    Emitaux.output_channel := oc;
    Emit.begin_assembly();
    List.iter compile_phrase (Cmmgen.compunit (Closure.intro lam));
    Emit.end_assembly();
    close_out oc
  with x ->
    close_out oc;
    if !assembler_only then () else remove_file asmfile;
    raise x
  end;
  if !assembler_only then () else begin
    if Proc.assemble_file asmfile (prefixname ^ ".o") <> 0
    then raise(Error(Assembler_error asmfile))
    else remove_file asmfile
  end

(* Error report *)

let report_error = function
    Assembler_error file ->
      print_string "Assembler error, input left in file ";
      print_string file
