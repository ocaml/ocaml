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

(* Transformation of Mach code into a list of pseudo-instructions. *)

open Reg
open Mach

type label = int

let label_counter = ref 99

let new_label() = incr label_counter; !label_counter

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    live: Reg.Set.t }

and instruction_desc =
    Lend
  | Lop of operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lsetuptrap of label
  | Lpushtrap
  | Lpoptrap
  | Lraise

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool }

(* Invert a test *)

let invert_integer_test = function
    Isigned cmp -> Isigned(Cmm.negate_comparison cmp)
  | Iunsigned cmp -> Iunsigned(Cmm.negate_comparison cmp)

let invert_test = function
    Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest(cmp) -> Iinttest(invert_integer_test cmp)
  | Iinttest_imm(cmp, n) -> Iinttest_imm(invert_integer_test cmp, n)
  | Ifloattest cmp -> Ifloattest(Cmm.negate_comparison cmp)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

(* The "end" instruction *)

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = [||];
    live = Reg.Set.empty }

(* Cons an instruction (live empty) *)

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r; live = Reg.Set.empty }

(* Cons a simple instruction (arg, res, live empty) *)

let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = [||]; live = Reg.Set.empty }

(* Build an instruction with arg, res, live taken from
   the given Mach.instruction *)

let copy_instr d i n =
  { desc = d; next = n;
    arg = i.Mach.arg; res = i.Mach.res; live = i.Mach.live }

(* Label the beginning of the given instruction sequence.
   If the sequence starts with a branch, jump over it. *)

let get_label n =
  match n.desc with
    Lbranch lbl -> (lbl, n)
  | Llabel lbl -> (lbl, n)
  | _ -> let lbl = new_label() in (lbl, cons_instr (Llabel lbl) n)

(* Discard all instructions up to the next label.
   This function is to be called before adding a non-terminating 
   instruction. *)

let rec discard_dead_code n =
  match n.desc with
    Lend -> n
  | Llabel _ -> n
  | _ -> discard_dead_code n.next

(* Add a branch in front of a continuation.
   Discard dead code in the continuation.
   Does not insert anything if we're just falling through. *)

let add_branch lbl n =
  let n1 = discard_dead_code n in
  match n1.desc with
    Llabel lbl1 when lbl1 = lbl -> n1
  | _ -> cons_instr (Lbranch lbl) n1

(* Current label for exit handler *)

let exit_label = ref 99

(* Linearize an instruction [i]: add it in front of the continuation [n] *)

let rec linear i n =
  match i.Mach.desc with
    Iend -> n
  | Iop(Itailcall_ind | Itailcall_imm _ as op) ->
      copy_instr (Lop op) i (discard_dead_code n)
  | Iop(Imove | Ireload | Ispill)
    when i.Mach.arg.(0).loc = i.Mach.res.(0).loc ->
      linear i.Mach.next n
  | Iop op ->
      copy_instr (Lop op) i (linear i.Mach.next n)
  | Ireturn ->
      let n1 = copy_instr Lreturn i (discard_dead_code n) in
      if !Proc.contains_calls
      then cons_instr Lreloadretaddr n1
      else n1
  | Iifthenelse(test, ifso, ifnot) ->
      let n1 = linear i.Mach.next n in
      begin match (ifso.Mach.desc, ifnot.Mach.desc) with
        Iexit, _ ->
          copy_instr (Lcondbranch(test, !exit_label)) i
            (linear ifnot n1)
      | _,  Iexit ->
          copy_instr (Lcondbranch(invert_test test, !exit_label)) i
            (linear ifso n1)
      | Iend, _ ->
          let (lbl_end, n2) = get_label n1 in
          copy_instr (Lcondbranch(test, lbl_end)) i
            (linear ifnot n2)
      | _,  Iend ->
          let (lbl_end, n2) = get_label n1 in
          copy_instr (Lcondbranch(invert_test test, lbl_end)) i
            (linear ifso n2)
      | _, _ ->
        (* Should attempt branch prediction here *)
          let (lbl_end, n2) = get_label n1 in
          let (lbl_else, nelse) = get_label (linear ifnot n2) in
          copy_instr (Lcondbranch(invert_test test, lbl_else)) i
            (linear ifso (add_branch lbl_end nelse))
      end
  | Iswitch(index, cases) ->
      let lbl_cases = Array.new (Array.length cases) 0 in
      let (lbl_end, n1) = get_label(linear i.Mach.next n) in
      let n2 = ref (discard_dead_code n1) in
      for i = Array.length cases - 1 downto 0 do
        let (lbl_case, ncase) =
                get_label(linear cases.(i) (add_branch lbl_end !n2)) in
        lbl_cases.(i) <- lbl_case;
        n2 := discard_dead_code ncase
      done;
      (* Switches with 1 and 2 branches have been eliminated earlier.
         Here, we do something for switches with 3 branches. *)
      if Array.length index = 3 then begin
        let fallthrough_lbl =
          match !n2.desc with Llabel lbl -> lbl | _ -> -1 in
        let find_label n =
          let lbl = lbl_cases.(index.(n)) in
          if lbl = fallthrough_lbl then None else Some lbl in
        copy_instr (Lcondbranch3(find_label 0, find_label 1, find_label 2))
                   i !n2
      end else
        copy_instr (Lswitch(Array.map (fun n -> lbl_cases.(n)) index)) i !n2
  | Iloop body ->
      let lbl_head = new_label() in
      let n1 = linear i.Mach.next n in
      let n2 = linear body (cons_instr (Lbranch lbl_head) n1) in
      cons_instr (Llabel lbl_head) n2
  | Icatch(body, handler) ->
      let (lbl_end, n1) = get_label(linear i.Mach.next n) in
      let (lbl_handler, n2) = get_label(linear handler n1) in
      let saved_exit_label = !exit_label in
      exit_label := lbl_handler;
      let n3 = linear body (add_branch lbl_end n2) in
      exit_label := saved_exit_label;
      n3
  | Iexit ->
      add_branch !exit_label (linear i.Mach.next n)
  | Itrywith(body, handler) ->
      let (lbl_join, n1) = get_label (linear i.Mach.next n) in
      let (lbl_body, n2) =
        get_label (cons_instr Lpushtrap
                    (linear body (cons_instr Lpoptrap n1))) in
      cons_instr (Lsetuptrap lbl_body)
        (linear handler (add_branch lbl_join n2))
  | Iraise ->
      copy_instr Lraise i (discard_dead_code n)

let fundecl f =
  { fun_name = f.Mach.fun_name;
    fun_body = linear f.Mach.fun_body end_instr;
    fun_fast = f.Mach.fun_fast }

