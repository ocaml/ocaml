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

(* Description of the Sparc processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Exceptions raised to signal cases not handled here *)

exception Use_default

(* Recognition of addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
    Cconst_symbol s ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
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
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])

(* Instruction selection *)

let select_oper op args = raise Use_default

let select_store addr exp = raise Use_default

let pseudoregs_for_operation op arg res = raise Use_default

let is_immediate n = (n <= 4095) & (n >= -4096)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map:
    %l0 - %l7   0 - 7       general purpose, preserved by C
    %o0 - %o5   8 - 13      function results, C functions args / res
    %i0 - %i5   14 - 19     function arguments, preserved by C
    %g2 - %g3   20 - 21     general purpose

    %g1, %g4                temporary
    %g5                     exception pointer
    %g6                     allocation pointer
    %g7                     allocation limit
    %g0                     always zero

    %f0 - %f10  100 - 105   function arguments and results
    %f12 - %f28 106 - 114   general purpose
    %f30                    temporary *)

let int_reg_name = [|
  (* 0-7 *)    "%l0"; "%l1"; "%l2"; "%l3"; "%l4"; "%l5"; "%l6"; "%l7";
  (* 8-13 *)   "%o0"; "%o1"; "%o2"; "%o3"; "%o4"; "%o5";
  (* 14-19 *)  "%i0"; "%i1"; "%i2"; "%i3"; "%i4"; "%i5";
  (* 20-21 *)  "%g2"; "%g3"
|]
  
let float_reg_name = [|
  (* 100-105 *) "%f0"; "%f2"; "%f4"; "%f6"; "%f8"; "%f10";
  (* 106-109 *) "%f12"; "%f14"; "%f16"; "%f18";
  (* 110-114 *) "%f20"; "%f22"; "%f24"; "%f26"; "%f28";
  (* Odd parts of register pairs *)
  (* 115-120 *) "%f1"; "%f3"; "%f5"; "%f7"; "%f9"; "%f11";
  (* 121-124 *) "%f13"; "%f15"; "%f17"; "%f19";
  (* 125-129 *) "%f21"; "%f23"; "%f25"; "%f27"; "%f29"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 22; 15 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.new 22 Reg.dummy in
  for i = 0 to 21 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.new 30 Reg.dummy in
  for i = 0 to 29 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg (Array.sub hard_float_reg 0 15)
  (* No need to include the odd parts of float register pairs *)

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

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
  (loc, Misc.align !ofs 8)         (* Keep stack 8-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 14 19 100 105 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 14 19 100 105 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 8 13 100 105 not_supported res in loc

(* On the Sparc, all arguments to C functions, even floating-point arguments,
   are passed in %o..%o5, then on the stack *)

let loc_external_arguments arg =
  let loc = Array.new (Array.length arg) Reg.dummy in
  let reg = ref 8 (* %o0 *) in
  let ofs = ref (-4) in              (* start at sp + 92 = sp + 96 - 4 *)
  for i = 0 to Array.length arg - 1 do
    if !reg <= 13 (* %o5 *) then begin
      loc.(i) <- phys_reg !reg;
      match arg.(i).typ with
        Int | Addr -> incr reg
      | Float -> reg := !reg + 2
    end else begin
      loc.(i) <- stack_slot (outgoing !ofs) arg.(i).typ;
      ofs := !ofs + size_component arg.(i).typ
    end
  done;
  (loc, Misc.align (!ofs + 4) 8)     (* Keep stack 8-aligned *)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 8 8 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 8         (* $o0 *)

(* Registers destroyed by operations *)

let destroyed_at_c_call = (* %l0-%l7, %i0-%i5 preserved *)
  Array.of_list(List.map phys_reg [8; 9; 10; 11; 12; 13; 20; 21])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 0
  | _ -> 15

let max_register_pressure = function
    Iextcall(_, _) -> [| 14; 0 |]
  | _ -> [| 22; 15 |]

(* Reloading *)

let reload_test makereg tst args = raise Use_default
let reload_operation makereg op args res = raise Use_default

(* Latencies (in cycles). Wild guesses. *)

let need_scheduling = true

let oper_latency = function
    Ireload -> 3
  | Iload(_, _) -> 3
  | Iconst_float _ -> 3 (* turned into a load *)
  | Iintop Imul -> 10
  | Iintop_imm(Imul, _) -> 10
  | Iintop(Idiv | Imod) -> 20
  | Iintop_imm((Idiv | Imod), _) -> 20
  | Iaddf | Isubf -> 3
  | Imulf -> 5
  | Idivf -> 15
  | _ -> 1

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler and the archiver *)

let assemble_file infile outfile =
  Sys.command ("as -o " ^ outfile ^ " " ^ infile)

let create_archive archive file_list =
  Misc.remove_file archive;
  match Config.system with
    "sunos" ->
      Sys.command ("ar rc " ^ archive ^ " " ^ String.concat " " file_list ^
                   " && ranlib " ^ archive)
  | "solaris" ->
      Sys.command ("ar rc " ^ archive ^ " " ^ String.concat " " file_list)
  | _ ->
      fatal_error "Proc_sparc.create_archive"


