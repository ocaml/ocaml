(* Disassembler for executable and .zo object files *)

open Obj
open Printf
open Config
open Asttypes
open Lambda
open Emitcode
open Opcodes
open Opnames

(* Read signed and unsigned integers *)

let inputu ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (b4 lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1

let inputs ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  let b4' = if b4 >= 128 then b4-256 else b4 in
  (b4' lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1

(* Global variables *)

type global_table_entry =
    Empty
  | Global of Ident.t
  | Constant of obj

let start = ref 0                             (* Position of beg. of code *)
let reloc = ref ([] : (reloc_info * int) list)  (* Relocation table *)
let globals = ref ([||] : global_table_entry array) (* Global List.map *)
let objfile = ref false               (* true if dumping a .zo *)

(* Print a structured constant *)

let rec print_struct_const = function
    Const_base(Const_int i) ->
      printf "%d" i
  | Const_base(Const_float f) ->
      printf "%s" f
  | Const_base(Const_string s) ->
      printf "\"%s\"" (String.escaped s)
  | Const_base(Const_char c) ->
      printf "'%s'" (Char.escaped c)
  | Const_block(tag, args) ->
      printf "<%d>" tag;
      begin match args with
        [] -> ()
      | [a1] ->
          printf "("; print_struct_const a1; printf ")"
      | a1::al ->
          printf "("; print_struct_const a1;
          List.iter (fun a -> printf ", "; print_struct_const a) al;
          printf ")"
      end

(* Print an obj *)

let rec print_obj x =
  if Obj.is_block x then begin
    match Obj.tag x with
      253 ->                            (* string *)
        printf "\"%s\"" (String.escaped (Obj.magic x : string))
    | 254 ->                            (* float *)
        printf "%.12g" (Obj.magic x : float)
    | _ ->
        printf "<%d>" (Obj.tag x);
        begin match Obj.size x with
          0 -> ()
        | 1 ->
            printf "("; print_obj (Obj.field x 0); printf ")"
        | n ->
            printf "("; print_obj (Obj.field x 0);
            for i = 1 to n - 1 do
              printf ", "; print_obj (Obj.field x i)
            done;
            printf ")"
        end
  end else
    printf "%d" (Obj.magic x : int)

(* Current position in input file *)

let currpos ic =
  pos_in ic - !start

(* Access in the relocation table *)

let rec rassoc key = function
    [] -> raise Not_found
  | (a,b) :: l -> if b = key then a else rassoc key l

let find_reloc ic =
  rassoc (pos_in ic - !start) !reloc

(* Symbolic printing of global names, etc *)

let print_getglobal_name ic =
  if !objfile then begin
    begin try
      match find_reloc ic with
          Reloc_getglobal id -> print_string (Ident.name id)
        | Reloc_literal sc -> print_struct_const sc
        | _ -> print_string "<wrong reloc>"
    with Not_found ->
      print_string "<no reloc>"
    end;
    inputu ic; ()
  end
  else begin
    let n = inputu ic in
    if n >= Array.length !globals
    then print_string "<global table overflow>"
    else match !globals.(n) with
           Global id -> print_string(Ident.name id)
         | Constant obj -> print_obj obj
         | _ -> print_string "???"
  end

let print_setglobal_name ic =
  if !objfile then begin
    begin try
      match find_reloc ic with
        Reloc_setglobal id -> print_string (Ident.name id)
      | _ -> print_string "<wrong reloc>"
    with Not_found ->
      print_string "<no reloc>"
    end;
    inputu ic; ()
  end
  else begin
    let n = inputu ic in
    if n >= Array.length !globals
    then print_string "<global table overflow>"
    else match !globals.(n) with
           Global id -> print_string(Ident.name id)
         | _ -> print_string "???"
  end

let print_primitive ic =
  if !objfile then begin
    begin try
      match find_reloc ic with
        Reloc_primitive s -> print_string s
      | _ -> print_string "<wrong reloc>"
    with Not_found ->
      print_string "<no reloc>"
    end;
    inputu ic; ()
  end
  else begin
    let n = inputu ic in
    if n >= Array.length Runtimedef.builtin_primitives
    then print_string(string_of_int n)
    else print_string(Runtimedef.builtin_primitives.(n))
  end

(* Disassemble one instruction *)

let print_instr ic =
  print_int (currpos ic); print_string "\t";
  let op = inputu ic in
  print_string
    (if op >= Array.length names_of_instructions then "???"
     else names_of_instructions.(op));
  print_string " ";
  (* One unsigned int *)
  if op == opATOM or op == opPUSHATOM 
  or op == opMAKEBLOCK1 or op == opMAKEBLOCK2 or op == opMAKEBLOCK3
  or op == opACC or op == opPUSHACC or op == opPOP or op == opASSIGN
  or op == opENVACC or op == opPUSHENVACC
  or op == opAPPLY or op == opAPPTERM1 or op == opAPPTERM2 or op == opAPPTERM3
  or op == opRETURN or op == opGRAB or op == opGETFIELD or op == opSETFIELD
  or op == opDUMMY then
    (print_int (inputu ic))
  (* One signed int *)
  else if op == opCONSTINT or op == opPUSHCONSTINT
  or op == opOFFSETINT or op == opOFFSETREF then
    (print_int (inputs ic))
  (* Two unsigned constants *)
  else if op == opAPPTERM or op == opMAKEBLOCK then
    (print_int (inputu ic); print_string ", "; print_int(inputu ic))
  (* One displacement *)
  else if op == opPUSH_RETADDR or op == opBRANCH or op == opBRANCHIF
  or op == opBRANCHIFNOT or op == opPUSHTRAP then
    (let p = currpos ic in print_int (p + inputs ic))
  (* One size, one displacement *)
  else if op == opCLOSURE or op == opCLOSUREREC then
    (print_int (inputu ic); print_string ", ";
     let p = currpos ic in print_int (p + inputs ic))
  (* getglobal *)
  else if op == opGETGLOBAL or op == opPUSHGETGLOBAL then
    (print_getglobal_name ic)
  (* getglobal + unsigned *)
  else if op == opGETGLOBALFIELD or op == opPUSHGETGLOBALFIELD then
    (print_getglobal_name ic; print_string ", "; print_int (inputu ic))
  (* setglobal *)
  else if op == opSETGLOBAL then
    (print_setglobal_name ic)
  (* primitive *)
  else if op == opC_CALL1 or op == opC_CALL2
       or op == opC_CALL3 or op == opC_CALL4 then
    (print_primitive ic)
  (* unsigned + primitive *)
  else if op == opC_CALLN then
    (print_int(inputu ic); print_string ", "; print_primitive ic)
  (* switch *)
  else if op == opSWITCH then
    (let n = inputu ic in
     let orig = currpos ic in
     for i = 0 to n-1 do
       print_string "\n\t"; print_int i; print_string " -> ";
       print_int(orig + inputs ic)
     done)
  (* translate *)
  else if op == opTRANSLATE then
    (let n = inputu ic in
     for i = 0 to n-1 do
       print_string "\n\t"; print_int(inputu ic)
     done)
  (* default *)
  else ();
  print_string "\n"

(* Disassemble a block of code *)

let print_code ic len =
  start := pos_in ic;
  let stop = !start + len in
  while pos_in ic < stop do print_instr ic done

(* Dump relocation info *)

let print_reloc (info, pos) =
  printf "\t%d\t(%d)\t" pos (pos/4);
  match info with
    Reloc_literal sc -> print_struct_const sc; printf "\n"
  | Reloc_getglobal id -> printf "require\t%s\n" (Ident.name id)
  | Reloc_setglobal id -> printf "provide\t%s\n" (Ident.name id)
  | Reloc_primitive s -> printf "prim\t%s\n" s

(* Print a .zo file *)

let dump_obj filename ic =
  let buffer = String.create (String.length cmo_magic_number) in
  really_input ic buffer 0 (String.length cmo_magic_number);
  if buffer <> cmo_magic_number then begin
    prerr_endline "Not an object file"; exit 2
  end;
  let cu_pos = input_binary_int ic in
  seek_in ic cu_pos;
  let cu = (input_value ic : compilation_unit) in
  reloc := cu.cu_reloc;
  seek_in ic cu.cu_pos;
  print_code ic cu.cu_codesize

(* Print an executable file *)

exception Not_exec

let dump_exe ic =
  seek_in ic (in_channel_length ic - 12);
  if (let buff = String.create 12 in input ic buff 0 12; buff)
     <> exec_magic_number
  then raise Not_exec;
  let trailer_pos = in_channel_length ic - 28 in
  seek_in ic trailer_pos;
  let code_size = input_binary_int ic in
  let data_size = input_binary_int ic in
  let symbol_size = input_binary_int ic in
  let debug_size = input_binary_int ic in
  seek_in ic (trailer_pos - debug_size - symbol_size - data_size);
  let init_data = (input_value ic : obj array) in
  globals := Array.new (Array.length init_data) Empty;
  for i = 0 to Array.length init_data - 1 do
    !globals.(i) <- Constant (init_data.(i))
  done;
  if symbol_size > 0 then begin
    let (_, sym_table) = (input_value ic : int * (Ident.t, int) Tbl.t) in
    Tbl.iter (fun id pos -> !globals.(pos) <- Global id) sym_table
  end;
  seek_in ic
    (trailer_pos - debug_size - symbol_size - data_size - code_size);
  print_code ic code_size

let main() =
  for i = 1 to Array.length Sys.argv - 1 do
    let ic = open_in_bin Sys.argv.(i) in
    begin try
      objfile := false; dump_exe ic
    with Not_exec ->
      objfile := true; seek_in ic 0; dump_obj (Sys.argv.(i)) ic
    end;
    close_in ic
  done;
  exit 0

let _ = Printexc.catch main (); exit 0
