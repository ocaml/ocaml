(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of the HP PA-RICS processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Registers available for register allocation *)

(* Register map:
    %r0                         always zero
    %r1                         temporary, target of ADDIL
    %r2                         return address
    %r3                         allocation pointer
    %r4                         allocation limit
    %r5                         trap pointer
    %r6 - %r26                  general purpose
    %r27                        global pointer
    %r28 - %r29                 general purpose, C function results
    %r30                        stack pointer
    %r31                        temporary, used by BLE

    %fr0 - %fr3                 float status info
    %fr4 - %fr30                general purpose
    %fr31                       temporary *)

let int_reg_name = [|
  (* 0-4 *)   "%r6"; "%r7"; "%r8"; "%r9"; "%r10"; 
  (* 5-10 *)  "%r11"; "%r12"; "%r13"; "%r14"; "%r15"; "%r16";
  (* 11-16 *) "%r17"; "%r18"; "%r19"; "%r20"; "%r21"; "%r22"; 
  (* 17-20 *) "%r23"; "%r24"; "%r25"; "%r26";
  (* 21-22 *) "%r28"; "%r29"
|]
  
let float_reg_name = [|
  (* 100-105 *) "%fr4"; "%fr5"; "%fr6"; "%fr7"; "%fr8"; "%fr9";
  (* 106-111 *) "%fr10"; "%fr11"; "%fr12"; "%fr13"; "%fr14"; "%fr15";
  (* 112-117 *) "%fr16"; "%fr17"; "%fr18"; "%fr19"; "%fr20"; "%fr21";
  (* 118-123 *) "%fr22"; "%fr23"; "%fr24"; "%fr25"; "%fr26"; "%fr27"; 
  (* 124-126 *) "%fr28"; "%fr29"; "%fr30"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 23; 27 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 23 Reg.dummy in
  for i = 0 to 22 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.create 27 Reg.dummy in
  for i = 0 to 26 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg (Array.sub hard_float_reg 0 27)
  (* No need to include the left/right parts of float registers *)

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Exceptions raised to signal cases not handled here *)

exception Use_default

(* Recognition of addressing modes *)

let select_addressing = function
    Cconst_symbol s ->
      (Ibased(s, 0), Ctuple [])
  | Cop(Cadda, [Cconst_symbol s; Cconst_int n]) ->
      (Ibased(s, n), Ctuple [])
  | Cop(Cadda, [arg; Cconst_int n]) ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

(* Instruction selection *)

let shiftadd = function
    2 -> Ishift1add
  | 4 -> Ishift2add
  | 8 -> Ishift3add
  | _ -> fatal_error "Proc_hppa.shiftadd"

let select_oper op args =
  match (op, args) with
  (* Recognize shift-add operations. *)
    ((Caddi|Cadda),
     [arg2; Cop(Clsl, [arg1; Cconst_int(1|2|3 as shift)])]) ->
      (Ispecific(shiftadd(1 lsl shift)), [arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [arg1; Cconst_int(2|4|8 as mult)])]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg1])]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  | (Caddi, [Cop(Clsl, [arg1; Cconst_int(1|2|3 as shift)]); arg2]) ->
      (Ispecific(shiftadd(1 lsl shift)), [arg1; arg2])
  | (Caddi, [Cop(Cmuli, [arg1; Cconst_int(2|4|8 as mult)]); arg2]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  | (Caddi, [Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg1]); arg2]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  (* Prevent the recognition of some immediate arithmetic operations *)
  (* Cmuli : -> Ilsl if power of 2
     Cdivi, Cmodi : only if power of 2
     Cand, Cor, Cxor : never *)
  | (Cmuli, ([arg1; Cconst_int n] as args)) ->
      let l = Misc.log2 n in
      if n = 1 lsl l 
      then (Iintop_imm(Ilsl, l), [arg1])
      else (Iintop Imul, args)
  | (Cmuli, ([Cconst_int n; arg1] as args)) ->
      let l = Misc.log2 n in
      if n = 1 lsl l
      then (Iintop_imm(Ilsl, l), [arg1])
      else (Iintop Imul, args)
  | (Cmuli, args) -> (Iintop Imul, args)
  | (Cdivi, [arg1; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Idiv, n), [arg1])
  | (Cdivi, args) -> (Iintop Idiv, args)
  | (Cmodi, [arg1; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Imod, n), [arg1])
  | (Cmodi, args) -> (Iintop Imod, args)
  | (Cand, args) -> (Iintop Iand, args)
  | (Cor, args) -> (Iintop Ior, args)
  | (Cxor, args) -> (Iintop Ixor, args)
  | _ -> raise Use_default

let select_store addr exp = raise Use_default

let pseudoregs_for_operation op arg res =
  match op with
    Iintop(Idiv | Imod) ->       (* handled via calls to millicode *)
      ([|phys_reg 20; phys_reg 19|], [|phys_reg 22|], true)
      (* %r26, %r25, %r29 *)
  | _ ->
      raise Use_default

let is_immediate n = (n < 16) & (n >= -16) (* 5 bits *)

let word_addressed = false

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int >= last_int then begin
          loc.(i) <- phys_reg !int;
          decr int
        end else begin
          ofs := !ofs + size_int;
          loc.(i) <- stack_slot (make_stack !ofs) ty
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          ofs := Misc.align (!ofs + size_float) 8;
          loc.(i) <- stack_slot (make_stack !ofs) Float
        end
  done;
  (loc, Misc.align !ofs 8)         (* Keep stack 8-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

(* Arguments and results: %r26-%r19, %fr4-%fr11. *)

let loc_arguments arg =
  calling_conventions 20 13 100 107 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions  20 13 100 107 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions  20 13 100 107 not_supported res in loc

(* Calling C functions:
    when all arguments are integers, use %r26 - %r23, 
    then -52(%r30), -56(%r30), etc.
    When some arguments are floats, we handle a couple of cases by hand
    and fail otherwise. *)

let loc_external_arguments arg =
  match List.map register_class (Array.to_list arg) with
    [1] -> ([| phys_reg 101 |], 56)           (* %fr5 *)
  | [1; 1] -> ([| phys_reg 101; phys_reg 103 |], 56) (* %fr5, %fr7 *)
  | [1; 0] -> ([| phys_reg 101; phys_reg 18 |], 56) (* %fr5, %r24 *)
  | [0; 1] -> ([| phys_reg 20; phys_reg 103 |], 56) (* %r26, %fr7 *)
  | _ ->
    let loc = Array.create (Array.length arg) Reg.dummy in
    let int = ref 20 in
    let ofs = ref 48 in
    for i = 0 to Array.length arg - 1 do
      match arg.(i).typ with
        Int | Addr as ty ->
          if !int >= 17 then begin
            loc.(i) <- phys_reg (!int);
            decr int
          end else begin
            ofs := !ofs + 4;
            loc.(i) <- stack_slot (Outgoing !ofs) ty
          end
      | Float ->
          fatal_error "Proc.external_calling_conventions: cannot call"
    done;
    (loc, Misc.align !ofs 8)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 21 21 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 20        (* %r26 *)

(* Registers destroyed by operations *)

let destroyed_at_c_call = (* %r3 - %r18, %fr12 - %fr21 preserved *)
  Array.of_list(List.map phys_reg
    [13;14;15;16;17;18;19;20;21;22;
     100;101;102;103;104;105;106;107;118;119;120;121;122;123;124;125;126])

let destroyed_by_millicode = (* %r25, %r26, %r28, %r29 -- more? *)
  [| phys_reg 19; phys_reg 20; phys_reg 21; phys_reg 22 |]

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Iintop(Idiv | Imod)) -> destroyed_by_millicode
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 16
  | Iintop(Idiv | Imod) -> 19
  | _ -> 23

let max_register_pressure = function
    Iextcall(_, _) -> [| 16; 19 |]
  | Iintop(Idiv | Imod) -> [| 19; 27 |]
  | _ -> [| 23; 27 |]

(* Reloading *)

let reload_test makereg tst args = raise Use_default
let reload_operation makereg op args res = raise Use_default

(* Latencies (in cycles). Roughly based on the ``Mustang'' chips. *)

let need_scheduling = true

let oper_latency = function
    Ireload -> 2
  | Iload(_, _) -> 2
  | Iconst_float _ -> 2                 (* turned into a load *)
  | Iintop Imul -> 2                    (* ends up with a load *)
  | Iaddf | Isubf | Imulf -> 3
  | Idivf -> 12
  | _ -> 1

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler and the archiver *)

let assemble_file infile outfile =
  Sys.command ("/bin/as -o " ^ outfile ^ " " ^ infile)

let create_archive archive file_list =
  Misc.remove_file archive;
      Sys.command ("ar rc " ^ archive ^ " " ^ String.concat " " file_list ^
                   " && ranlib " ^ archive)
