(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of the Power PC *)

open Misc
open Cmm
open Reg
open Arch
open Mach

let powerpc =
  match Config.model with
    "ppc" -> true
  | "rs6000" -> false
  | _ -> fatal_error "wrong $(MODEL)"

(* Exceptions raised to signal cases not handled here *)

exception Use_default

(* Recognition of addressing modes *)

type addressing_expr =
    Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
    Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | exp ->
      (Alinear exp, 0)

let select_addressing exp =
  match select_addr exp with
    (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])

(* Instruction selection *)

let select_logical op = function
    [arg; Cconst_int n] when n >= 0 & n <= 0xFFFF ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when n >= 0 & n <= 0xFFFF ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

let select_oper op args =
  match (op, args) with
  (* Prevent the recognition of (x / cst) and (x % cst) when cst is not
     a power of 2, which do not correspond to an instruction. *)
    (Cdivi, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Idiv, n), [arg])
  | (Cdivi, _) -> 
      (Iintop Idiv, args)
  | (Cmodi, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Imod, n), [arg])
  | (Cmodi, _) ->
      (Iintop Imod, args)
  (* The and, or and xor instructions have a different range of immediate
     operands than the other instructions *)
  | (Cand, _) -> select_logical Iand args
  | (Cor, _) -> select_logical Ior args
  | (Cxor, _) -> select_logical Ixor args
  (* intoffloat goes through a library function on the RS6000 *)
  | (Cintoffloat, _) when not powerpc ->
      (Iextcall("itrunc", false), args)
  (* Recognize mult-add and mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2]); arg3]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2])]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2]); arg3]) ->
      (Ispecific Imultsubf, [arg1; arg2; arg3])
  | _ ->
      raise Use_default

let select_store addr exp = raise Use_default

let select_push exp = fatal_error "Proc: select_push"

let pseudoregs_for_operation op arg res = raise Use_default

let is_immediate n = (n <= 32767) & (n >= -32768)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    0                   temporary, null register for some operations
    1                   stack pointer
    2                   pointer to table of constants
    3 - 10              function arguments and results
    11 - 12             general purpose
    13 - 27             general purpose, preserved by C
    28                  temporary
    29                  trap pointer
    30                  allocation limit
    31                  allocation pointer
  Floating-point register map:
    0                   temporary
    1 - 13              function arguments and results
    14 - 31             general purpose, preserved by C
*)

let int_reg_name = [|
  "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; 
  "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18"; "19"; "20";
  "21"; "22"; "23"; "24"; "25"; "26"; "27"
|]
  
let float_reg_name = [|
  "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";
  "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16";
  "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24";
  "25"; "26"; "27"; "28"; "29"; "30"; "31" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 25; 31 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 25 Reg.dummy in
  for i = 0 to 24 do v.(i) <- Reg.at_location Int (Reg i) done; v

let hard_float_reg =
  let v = Array.create 31 Reg.dummy in
  for i = 0 to 30 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done; v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 24 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, (if !ofs > 24 then Misc.align !ofs 8 else 0))
  (* Keep stack 8-aligned and with a free 24 byte linkage area at the bottom *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 7 100 112 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 100 112 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 112 not_supported res in loc

(* For C calling conventions: use GPR 3-10 and FPR 1-13 just like ML calling
   conventions, but always reserve stack space for all arguments.
   Also, using a float register automatically reserves two int registers. *)

let external_conventions first_int last_int first_float last_float arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 56 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) Float;
          ofs := !ofs + size_float
        end;
        int := !int + 2
  done;
  (loc, Misc.align !ofs 8) (* Keep stack 8-aligned *)

let loc_external_arguments arg = external_conventions 0 7 100 112 arg

let extcall_use_push = false

(* Results are in GPR 3 and FPR 1 *)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

(* Exceptions are in GPR 3 *)

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 9;
     100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111; 112])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 15
  | _ -> 25

let max_register_pressure = function
    Iextcall(_, _) -> [| 15; 18 |]
  | _ -> [| 25; 30 |]

(* Reloading *)

let reload_test makereg tst args = raise Use_default
let reload_operation makereg op args res = raise Use_default

(* Latencies (in cycles). 
   Based on the Motorola 601, with some poetic license. *)

let need_scheduling = true

let oper_latency = function
    Ireload -> 2
  | Iload(_, _) -> 2
  | Iconst_float _ -> 2 (* turned into a load *)
  | Iconst_symbol _ -> 2 (* turned into a load *)
  | Iintop Imul -> 9
  | Iintop_imm(Imul, _) -> 5
  | Iintop(Idiv | Imod) -> 36
  | Iaddf | Isubf -> 4
  | Imulf -> 5
  | Idivf -> 31
  | Ispecific(Imultaddf | Imultsubf) -> 5
  | _ -> 1

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler and the archiver *)

let assemble_file infile outfile =
  let proc = if powerpc then "ppc" else "pwr" in
  Sys.command ("as -u -m " ^ proc ^ " -o " ^ outfile ^ " " ^ infile)

let create_archive archive file_list =
  Misc.remove_file archive;
  Sys.command ("ar rc " ^ archive ^ " " ^ String.concat " " file_list)
