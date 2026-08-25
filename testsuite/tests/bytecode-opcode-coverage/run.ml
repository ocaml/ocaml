(* TEST
 include ocamlcommon;
 include ocamlbytecomp;
 include unix;
 hasunix;
 flags = "-w -a";
 readonly_files = "test.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "test.ml run.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Read our own bytecode executable's CODE section and record which opcodes
   appear.  Print the sorted names of opcodes that were never emitted. *)

let inputu ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (b4 lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1

(* Number of extra 32-bit words to skip after each opcode.
   If new opcodes are added, the tables below will need updating.
   The assertion on opFIRST_UNIMPLEMENTED_OP will trip if this happens;
   consult the op_shapes table in tools/dumpobj.ml for the new shapes. *)
let extra_words =
  let open Opcodes in
  assert (opFIRST_UNIMPLEMENTED_OP = 153);
  let t = Array.make opFIRST_UNIMPLEMENTED_OP 0 in
  List.iter (fun op -> t.(op) <- 1) [
    opACC; opPUSHACC; opPOP; opASSIGN; opENVACC; opPUSHENVACC;
    opPUSH_RETADDR; opAPPLY; opAPPTERM1; opAPPTERM2; opAPPTERM3;
    opRETURN; opGRAB;
    opOFFSETCLOSURE; opPUSHOFFSETCLOSURE;
    opGETGLOBAL; opPUSHGETGLOBAL; opSETGLOBAL;
    opATOM; opPUSHATOM;
    opMAKEBLOCK1; opMAKEBLOCK2; opMAKEBLOCK3; opMAKEFLOATBLOCK;
    opGETFIELD; opGETFLOATFIELD; opSETFIELD; opSETFLOATFIELD;
    opBRANCH; opBRANCHIF; opBRANCHIFNOT; opPUSHTRAP;
    opC_CALL1; opC_CALL2; opC_CALL3; opC_CALL4; opC_CALL5;
    opCONSTINT; opPUSHCONSTINT;
    opOFFSETINT; opOFFSETREF;
    opRESUMETERM; opREPERFORMTERM;
  ];
  List.iter (fun op -> t.(op) <- 2) [
    opAPPTERM; opCLOSURE; opMAKEBLOCK;
    opGETGLOBALFIELD; opPUSHGETGLOBALFIELD;
    opC_CALLN;
    opBEQ; opBNEQ; opBLTINT; opBLEINT; opBGTINT; opBGEINT;
    opBULTINT; opBUGEINT;
    opGETPUBMET;
  ];
  t

let () =
  let seen = Array.make Opcodes.opFIRST_UNIMPLEMENTED_OP false in
  let ic = open_in_bin Sys.argv.(0) in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let toc = Bytesections.read_toc ic in
    let code_size =
      Bytesections.seek_section toc ic Bytesections.Name.CODE
    in
    let stop = pos_in ic + code_size in
    while pos_in ic < stop do
      let op = inputu ic in
      if op >= 0 && op < Opcodes.opFIRST_UNIMPLEMENTED_OP then begin
        seen.(op) <- true;
        let skip =
          if op = Opcodes.opSWITCH then
            let n = inputu ic in
            (n land 0xFFFF) + (n lsr 16)
          else if op = Opcodes.opCLOSUREREC then
            let nfuncs = inputu ic in
            ignore (inputu ic);  (* nvars *)
            nfuncs
          else
            extra_words.(op)
        in
        for _ = 1 to skip do ignore (inputu ic) done
      end
    done);
  let unseen = ref [] in
  for i = 0 to Opcodes.opFIRST_UNIMPLEMENTED_OP - 1 do
    if not seen.(i) then
      unseen := Opnames.names_of_instructions.(i) :: !unseen
  done;
  List.iter print_endline (List.sort String.compare !unseen)
