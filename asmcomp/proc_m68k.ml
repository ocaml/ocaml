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

(* Description of the Motorola 68020 processor *)

open Misc
open Arch
open Format
open Cmm
open Reg
open Mach

(* Registers available for register allocation *)

(* Register map:
    A0 - A6     0-6     address registers (A2-A6 callee-save)
    A7                  stack pointer
    D0 - D4     7-11    data registers (D2 - D7 callee-save)
    D5                  temporary
    D6                  allocation pointer
    D7                  trap pointer
    FP0 - FP7   12-19   floating-point registers (FP2 - FP7 callee-save)
*)

let register_names =
  [| "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6";
     "d0"; "d1"; "d2"; "d3"; "d4";
     "fp0"; "fp1"; "fp2"; "fp3"; "fp4"; "fp5"; "fp6"; "fp7" |]

let num_register_classes = 3

let register_class r =
  match r.typ with
    Addr -> 0
  | Int -> 1
  | Float -> 2

let num_available_registers = [| 7; 5; 8 |]

let first_available_register = [| 0; 7; 12 |]

let register_name r = register_names.(r)

(* There is no scheduling, so just pack registers. *)

let rotate_registers = false

(* Representation of hard registers by pseudo-registers *)

let all_phys_regs =
  let v = Array.create 20 Reg.dummy in
  for i = 0 to 6 do v.(i) <- Reg.at_location Addr (Reg i) done;
  for i = 7 to 11 do v.(i) <- Reg.at_location Int (Reg i) done;
  for i = 12 to 19 do v.(i) <- Reg.at_location Float (Reg i) done;
  v

let phys_reg n = all_phys_regs.(n)

let stack_slot slot ty = Reg.at_location ty (Stack slot)

let reg_A0 = phys_reg 0
let reg_FP0 = phys_reg 12

(* Exceptions raised to signal cases not handled here *)

exception Use_default

(* Instruction selection *)

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol s ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Csubi | Csuba), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int(1|2|3 as shift)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int(2|4|8 as mult)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)
    
let select_addressing exp =
  match select_addr exp with
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])
  | (Ascale(e, scale), d) ->
      (Iscaled(scale, d), e)
  | (Ascaledadd(e1, e2, scale), d) ->
      (Iindexed2scaled(scale, d), Ctuple[e1; e2])

(* Selection of immediate shifts *)

let select_shift op args =
  match args with
    [arg1; Cconst_int n] when n >= 1 && n <= 8 -> (Iintop_imm(op, n), [arg1])
  | _ -> (Iintop op, args)

(* Main instruction selection functions *)

let select_oper op args =
  match op with
  (* Recognize the LEA instruction *)
    Cadda | Csuba ->
      begin match select_addressing (Cop(op, args)) with
        (Iindexed d, _) -> raise Use_default
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize immediate shifts only if 1 <= count <= 8 *)
  | Clsl -> select_shift Ilsl args
  | Clsr -> select_shift Ilsr args
  | Casr -> select_shift Iasr args
  (* Recognize store instructions *)
  | Cstore ->
      begin match args with
        [loc; Cconst_int n] ->
          let (addr, arg) = select_addressing loc in
          (Ispecific(Istore_int(n, addr)), [arg])
      | [loc; Cconst_pointer n] ->
          let (addr, arg) = select_addressing loc in
          (Ispecific(Istore_int(n, addr)), [arg])
      | [loc; Cconst_symbol s] ->
          let (addr, arg) = select_addressing loc in
          (Ispecific(Istore_symbol(s, addr)), [arg])
      | _ ->
          raise Use_default
      end
  | _ -> raise Use_default

let select_store addr exp =
  match exp with
    Cconst_int n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Istore_symbol(s, addr)), Ctuple [])
  | _ -> raise Use_default

let select_push exp =
  match exp with
    Cconst_int n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Ipush_symbol s), Ctuple [])
  | Cop(Cload ty, [loc]) when ty = typ_float ->
      let (addr, arg) = select_addressing loc in
      (Ispecific(Ipush_load_float addr), arg)
  | Cop(Cload ty, [loc]) when ty = typ_addr or ty = typ_int ->
      let (addr, arg) = select_addressing loc in
      (Ispecific(Ipush_load addr), arg)
  | _ -> (Ispecific(Ipush), exp)

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations *)
    Iintop(Iadd | Isub | Imul | Idiv | Imod | Ilsl | Ilsr | Iasr) |
    Iaddf | Isubf | Imulf | Idivf ->
      ([|res.(0); arg.(1)|], res, false)
  (* Two-address binary operations, forcing the second argument to be
     in a data register *)
  | Iintop(Iand | Ior | Ixor) ->
      let newarg1 = Reg.create Int in
      ([|res.(0); newarg1|], res, false)
  (* Two-address unary operations *)
  | Iintop_imm((Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor |
                Ilsl | Ilsr | Iasr), _) ->
      (res, res, false)
  (* Other instructions are regular *)
  | _ -> raise Use_default

let is_immediate (n: int) = true

let word_addressed = false

(* Calling conventions *)

let calling_conventions first_addr last_addr first_float last_float
                        make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let addr = ref first_addr in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      (Addr | Int) as ty ->
        if !addr <= last_addr then begin
          loc.(i) <- phys_reg !addr;
          incr addr
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_addr
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
  (loc, !ofs)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 5 12 18 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 5 12 18 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 12 18 not_supported res in loc
let extcall_use_push = true
let loc_external_arguments arg =
  fatal_error "Proc.loc_external_arguments"
let loc_external_results res =
  let (loc, ofs) = calling_conventions 7 7 12 12 not_supported res in loc

let loc_exn_bucket = reg_A0

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  Array.of_list(List.map phys_reg [0; 1; 7; 8; 12; 13])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Iintoffloat) -> [| reg_FP0 |]
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure op = 5

let max_register_pressure = function
    Iextcall(_, _) -> [| 5; 3; 6 |]
  | Iintoffloat -> [| 7; 5; 7 |]
  | _ -> num_available_registers

(* Reloading of instruction arguments, storing of instruction results. *)

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

let reload_test makereg round tst arg =
  match tst with
    Iinttest _ | Ifloattest _ ->
      (* The second argument can be on stack *)
      [| makereg arg.(0); arg.(1) |]
  | _ ->
      (* The argument can be on stack *)
      arg

let reload_operation makereg round op arg res =
  match op with
    Imove | Ireload | Ispill | 
    Iintop_imm((Iadd | Isub | Iand | Ior | Ixor |
                Icomp _ | Ilsl | Ilsr | Iasr), _) |
    Ifloatofint | Iintoffloat | Ispecific(Ipush) ->
      (* The argument(s) can be either in register or on stack *)
      (arg, res)
  | Iintop(Iadd | Isub | Iand | Ior | Ixor | Icomp _) ->
      (* One of the two arguments can reside in the stack *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); makereg arg.(1)|], res)
      else (arg, res)
  | Iintop(Ilsl | Ilsr | Iasr) ->
      (* The first argument and result can reside in the stack *)
      ([|arg.(0); makereg arg.(1)|], res)
  | Iintop(Imul | Idiv | Imod) | Iaddf | Isubf | Imulf | Idivf ->
      (* The second argument can reside in the stack *)
      let r = makereg arg.(0) in ([|r; arg.(1)|], [|r|])
  | _ -> (* Other operations: all args and results in registers *)
      raise Use_default

(* Scheduling is turned off. *)

let need_scheduling = false

let oper_latency _ = 0

(* Layout of the stack frame *)

let num_stack_slots = [| 0; 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Sys.command ("as -o " ^ outfile ^ " " ^ infile)

