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

(* Description of the Intel 386 processor *)

open Misc
open Arch
open Format
open Cmm
open Reg
open Mach

(* Registers available for register allocation *)

(* Register map:
    eax         0               eax - edi: function arguments and results
    ebx         1               eax: C function results
    ecx         2               ebx, esi, edi, ebp: preserved by C
    edx         3
    esi         4
    edi         5
    ebp         6

    f0 - f3     101-104         function arguments and results
                                f0: C function results
                                not preserved by C

   The other 4 floating-point registers are treated as a stack.
   We use the pseudo-register %tos (100) to represent the top of that stack. *)

let int_reg_name =
  [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi"; "%ebp" |]

let float_reg_name =
  [| "%tos"; "%st(0)"; "%st(1)"; "%st(2)"; "%st(3)";
     "%st(4)"; "%st(5)"; "%st(6)"; "%st(7)" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 7; 4 |]

let first_available_register = [| 0; 101 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

(* There is little scheduling, and some operations are more efficient when
   %eax or %st(0) are arguments *)

let rotate_registers = false

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.new 7 Reg.dummy in
  for i = 0 to 6 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.new 5 Reg.dummy in
  for i = 0 to 4 do v.(i) <- Reg.at_location Float (Reg(i + 100)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

let eax = phys_reg 0
let ecx = phys_reg 2
let edx = phys_reg 3
let tos = phys_reg 100

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

(* Estimate number of float temporaries needed to evaluate expression
   (Ershov's algorithm) *)

let rec float_needs = function
    Cop((Caddf | Csubf | Cmulf | Cdivf), [arg1; arg2]) ->
      let n1 = float_needs arg1 in
      let n2 = float_needs arg2 in
      if n1 = n2 then 1 + n1 else if n1 > n2 then n1 else n2
  | _ ->
      1

(* Recognize float arithmetic with mem *)

let select_floatarith regular_op reversed_op mem_op mem_rev_op args =
  match args with
    [arg1; Cop(Cload _, [loc2])] ->
      let (addr, arg2) = select_addressing loc2 in
      (Ispecific(Ifloatarithmem(mem_op, addr)), [arg1; arg2])
  | [Cop(Cload _, [loc1]); arg2] ->
      let (addr, arg1) = select_addressing loc1 in
      (Ispecific(Ifloatarithmem(mem_rev_op, addr)), [arg2; arg1])
  | [arg1; arg2] ->
      (* Evaluate bigger subexpression first to minimize stack usage.
         Because of right-to-left evaluation, rightmost arg is evaluated
         first *)
      if float_needs arg1 <= float_needs arg2
      then (regular_op, [arg1; arg2])
      else (reversed_op, [arg2; arg1])
  | _ ->
      fatal_error "Proc_i386: select_floatarith"

(* Main instruction selection functions *)

exception Use_default

let select_oper op args =
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Cadda | Csubi | Csuba ->
      begin match select_addressing (Cop(op, args)) with
        (Iindexed d, _) -> raise Use_default
      | (Iindexed2 0, _) -> raise Use_default
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize (x / cst) and (x % cst) only if cst is a power of 2. *)
  | Cdivi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Idiv, n), [arg1])
      | _ -> (Iintop Idiv, args)
      end
  | Cmodi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Imod, n), [arg1])
      | _ -> (Iintop Imod, args)
      end
  (* Recognize float arithmetic with memory.
     In passing, apply Ershov's algorithm to reduce stack usage *)
  | Caddf ->
      select_floatarith Iaddf Iaddf Ifloatadd Ifloatadd args
  | Csubf ->
      select_floatarith Isubf (Ispecific Isubfrev) Ifloatsub Ifloatsubrev args
  | Cmulf ->
      select_floatarith Imulf Imulf Ifloatmul Ifloatmul args
  | Cdivf ->
      select_floatarith Idivf (Ispecific Idivfrev) Ifloatdiv Ifloatdivrev args
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
      | [loc; Cop(Caddi, [Cop(Cload _, [loc']); Cconst_int n])]
        when loc = loc' ->
          let (addr, arg) = select_addressing loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
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

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor) ->
      ([|res.(0); arg.(1)|], res, false)
  (* Two-address unary operations *)
  | Iintop_imm((Iadd|Isub|Imul|Idiv|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _) ->
      (res, res, false)
  (* For shifts with variable shift count, second arg must be in ecx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); ecx|], res, false)
  (* For div and mod, first arg must be in eax, edx is clobbered,
     and result is in eax or edx respectively.
     Keep it simple, just force second argument in ecx. *)
  | Iintop(Idiv) ->
      ([| eax; ecx |], [| eax |], true)
  | Iintop(Imod) ->
      ([| eax; ecx |], [| edx |], true)
  (* For mod with immediate operand, arg must not be in eax.
     Keep it simple, force it in edx. *)
  | Iintop_imm(Imod, _) ->
      ([| edx |], [| edx |], true)
  (* For floating-point operations, the result is always left at the
     top of the floating-point stack *)
  | Iconst_float _ | Iaddf | Isubf | Imulf | Idivf | Ifloatofint |
    Ispecific(Isubfrev | Idivfrev | Ifloatarithmem(_, _)) ->
      (arg, [| tos |], false)           (* don't move it immediately *)
  (* Same for a floating-point load *)
  | Iload(Word, addr) when res.(0).typ = Float ->
      (arg, [| tos |], false)
  (* For storing a byte, the argument must be in eax...edx.
     For storing a halfword, any reg is ok.
     Keep it simple, just force it to be in edx in both cases. *)
  | Istore(Word, addr) -> raise Use_default
  | Istore(chunk, addr) ->
      let newarg = Array.copy arg in
      newarg.(0) <- edx;
      (newarg, res, false)
  (* Other instructions are more or less regular *)
  | _ -> raise Use_default

let is_immediate (n: int) = true

let word_addressed = false

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
  let loc = Array.new (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
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
  (loc, !ofs)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 5 101 104 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 5 101 104 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 101 104 not_supported res in loc
let loc_external_arguments arg =
  calling_conventions 0 (-1) 101 100 outgoing arg
let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 101 101 not_supported res in loc

let loc_exn_bucket = eax

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* ebx, esi, edi, ebp preserved *)
  Array.of_list(List.map phys_reg [0;2;3;100;101;102;103;104])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Iintop(Idiv | Imod)) -> [| eax; edx |]
  | Iop(Iintop_imm(Imod, _)) -> [| eax |]
  | Iop(Ialloc _) -> [| eax |]
  | Iop(Iintop(Icomp _) | Iintop_imm(Icomp _, _)) -> [| eax |]
  | Iop(Iintoffloat) -> [| eax |]
  | Iifthenelse(Ifloattest(_, _), _, _) -> [| eax |]
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure op = 4

let max_register_pressure = function
    Iextcall(_, _) -> [| 4; 0 |]
  | Iintop(Idiv | Imod) -> [| 5; 4 |]
  | Ialloc _ | Iintop(Icomp _) | Iintop_imm(Icomp _, _) |
    Iintoffloat -> [| 6; 4 |]
  | _ -> [|7; 4|]

(* Reloading of instruction arguments, storing of instruction results *)

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

let reload_test makereg tst arg =
  match tst with
    Iinttest cmp ->
      if stackp arg.(0) & stackp arg.(1)
      then [| makereg arg.(0); arg.(1) |]
      else arg
  | _ -> arg

let reload_operation makereg op arg res =
  match op with
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor|Icomp _|Icheckbound) ->
      (* One of the two arguments can reside in the stack *)
      if stackp arg.(0) & stackp arg.(1)
      then ([|arg.(0); makereg arg.(1)|], res)
      else (arg, res)
  | Iintop(Ilsl|Ilsr|Iasr) | Iintop_imm(_, _) | Ifloatofint | Iintoffloat |
    Iaddf | Isubf | Imulf | Idivf ->
      (* The argument(s) can be either in register or on stack *)
      (arg, res)
  | Ispecific(Ifloatarithmem(_, _)) ->
      (* First arg can be either in register or on stack, but remaining
         arguments must be in registers *)
      let newarg = Array.new (Array.length arg) arg.(0) in
      for i = 1 to Array.length arg - 1 do newarg.(i) <- makereg arg.(i) done;
      (newarg, res)
  | _ -> (* Other operations: all args and results in registers *)
      raise Use_default

(* Scheduling is turned off because our model does not fit the 486
   nor Pentium very well. In particular, it messes up with the
   float reg stack. *)

let need_scheduling = false

let oper_latency _ = 0

(* Layout of the stack frame *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Sys.command ("as -o " ^ outfile ^ " " ^ infile)

(* Calling the archiver *)

let create_archive archive file_list =
  Misc.remove_file archive;
  Sys.command ("ar rc " ^ archive ^ " " ^ String.concat " " file_list ^
               " && ranlib " ^ archive)
