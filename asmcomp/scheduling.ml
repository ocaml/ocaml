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

(* Instruction scheduling *)

open Misc
open Reg
open Mach
open Linearize

(* Determine whether an operation ends a basic block or not *)

let in_basic_block = function
    Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _ -> false
  | Iextcall(_, _) -> false
  | Istackoffset _ -> false
  | Istore(_, _) -> false
  | Ialloc _ -> false
  | op -> Proc.oper_latency op >= 0
    (* The processor description can return a latency of -1 to signal
       a specific instruction that terminates a basic block, e.g.
       Istore_symbol for the I386. *)

(* Estimate the delay needed to evaluate an instruction. *)

let instr_latency instr =
  match instr.desc with
    Lop op -> Proc.oper_latency op
  | _ -> fatal_error "Scheduling.instr_latency"

(* Representation of the code DAG. *)

type code_dag_node =
  { instr: instruction;                 (* The instruction *)
    delay: int;                         (* How many cycles it needs *)
    mutable sons: (code_dag_node * int) list;
                                        (* Instructions that depend on it *)
    mutable date: int;                  (* Start date *)
    mutable length: int;                (* Length of longest path to result *)
    mutable ancestors: int;             (* Number of ancestors *)
    mutable emitted_ancestors: int }    (* Number of emitted ancestors *)

let dummy_node =
  { instr = end_instr; delay = 0; sons = []; date = 0;
    length = -1; ancestors = 0; emitted_ancestors = 0 }

(* The code dag itself is represented by two tables from registers to nodes:
   - "results" maps registers to the instructions that produced them;
   - "uses" maps registers to the instructions that use them. *)

let code_results = (Hashtbl.new 31 : (location, code_dag_node) Hashtbl.t)
let code_uses = (Hashtbl.new 31 : (location, code_dag_node) Hashtbl.t)

let clear_code_dag () =
  Hashtbl.clear code_results;
  Hashtbl.clear code_uses

(* Add an instruction to the code DAG *)

let add_edge ancestor son delay =
  ancestor.sons <- (son, delay) :: ancestor.sons;
  son.ancestors <- son.ancestors + 1

let add_instruction ready_queue instr =
  let delay = instr_latency instr in
  let node =
    { instr = instr;
      delay = delay;
      sons = [];
      date = 0;
      length = -1;
      ancestors = 0;
      emitted_ancestors = 0 } in
  (* Add edges from all instructions that define one of the registers used *)
  for i = 0 to Array.length instr.arg - 1 do
    try
      let ancestor = Hashtbl.find code_results instr.arg.(i).loc in
      add_edge ancestor node ancestor.delay
    with Not_found ->
      ()
  done;
  (* Also add edges from all instructions that use one of the results
     of this instruction, so that evaluation order is preserved. *)
  for i = 0 to Array.length instr.res - 1 do
    let ancestors = Hashtbl.find_all code_uses instr.res.(i).loc in
    List.iter (fun ancestor -> add_edge ancestor node 0) ancestors
  done;
  (* Also add edges from all instructions that have already defined one
     of the results of this instruction, so that evaluation order
     is preserved. *)
  for i = 0 to Array.length instr.res - 1 do
    try
      let ancestor = Hashtbl.find code_results instr.res.(i).loc in
      add_edge ancestor node 0
    with Not_found ->
      ()
  done;
  (* Remember the registers used and produced by this instruction *)
  for i = 0 to Array.length instr.res - 1 do
    Hashtbl.add code_results instr.res.(i).loc node
  done;
  for i = 0 to Array.length instr.arg - 1 do
    Hashtbl.add code_uses instr.arg.(i).loc node
  done;
  (* If this is a root instruction (all arguments already computed),
     add it to the ready queue *)
  if node.ancestors = 0 then node :: ready_queue else ready_queue

(* Compute length of longest path to a result.
   For leafs of the DAG, see whether their result is used in the instruction
   immediately following the basic block (a "critical" output). *)

let is_critical critical_outputs results =
  try
    for i = 0 to Array.length results - 1 do
      let r = results.(i).loc in
      for j = 0 to Array.length critical_outputs - 1 do
        if critical_outputs.(j).loc = r then raise Exit
      done
    done;
    false
  with Exit ->
    true

let rec longest_path critical_outputs node =
  if node.length < 0 then begin
    match node.sons with
      [] ->
        node.length <-
          if is_critical critical_outputs node.instr.res
          then node.delay
          else 0
    | sons ->
        node.length <- 
          List.fold_left
            (fun len (son, delay) ->
              max len (longest_path critical_outputs son + delay))
            0 sons
  end;
  node.length

(* Given a list of instructions with estimated start date, choose one
   that we can start (start date <= current date) and that has
   maximal distance to result. If we can't find any, return None. *)

let extract_ready_instr date queue =
  let rec extract best = function
    [] ->
      if best == dummy_node then None else Some best
  | instr :: rem ->
      let new_best =
        if instr.date <= date & instr.length > best.length
        then instr else best in
      extract new_best rem in
  extract dummy_node queue

(* Remove an instruction from the ready queue *)

let rec remove_instr node = function
    [] -> []
  | instr :: rem ->
      if instr == node then rem else instr :: remove_instr node rem

(* Print the dag *)

(****
open Format

let printed = ref ([] : (code_dag_node * int) list)
let print_counter = ref 0

let rec print_node n =
  try
    List.assq n !printed
  with Not_found ->
    let i = !print_counter in
    incr print_counter;
    printed := (n, i) :: !printed;
    let num_sons =
      List.map (fun (son, delay) -> (print_node son, delay)) n.sons in
    print_int i; print_string ": "; 
    let (Lop op) = n.instr.desc in
      Printmach.operation op n.instr.arg n.instr.res; print_newline();
    print_string "  Distance to output: ";
    print_int n.length; print_newline();
    print_string "  Sons: ";
    List.iter 
      (fun (son, delay) ->
          print_int son; print_string "/"; print_int delay; print_space())
      num_sons;
    print_newline();
    i
***)

(* Schedule a basic block, adding its instructions in front of the given
   instruction sequence *)

let rec reschedule ready_queue date cont =
  match ready_queue with
    [] -> cont
  | _ ->
      (* Find "most ready" instruction in queue *)
      match extract_ready_instr date ready_queue with
        None ->
          (* Try again, one cycle later *)
          reschedule ready_queue (date + 1) cont
      | Some node ->
          (* Update the start date and number of ancestors emitted of
             all descendents of this node. Enter those that become ready
             in the queue. *)
          let new_queue = ref (remove_instr node ready_queue) in
          List.iter
            (fun (son, delay) ->
              let completion_date = date + delay in
              if son.date < completion_date then son.date <- completion_date;
              son.emitted_ancestors <- son.emitted_ancestors + 1;
              if son.emitted_ancestors = son.ancestors then
                new_queue := son :: !new_queue)
            node.sons;
          instr_cons node.instr.desc node.instr.arg node.instr.res
            (reschedule !new_queue (date + 1) cont)

(* Schedule basic blocks in an instruction sequence *)

let rec schedule i =
  match i.desc with
    Lend -> i
  | Lop op when in_basic_block op ->
      clear_code_dag();
      schedule_block [] i
  | op ->
      { desc = op; arg = i.arg; res = i.res; live = i.live;
        next = schedule i.next }

and schedule_block ready_queue i =
  match i.desc with
    Lop op when in_basic_block op ->
      schedule_block (add_instruction ready_queue i) i.next
  | _ ->
      let critical_outputs =
        match i.desc with
          Lop(Icall_ind | Itailcall_ind) -> [| i.arg.(0) |]
        | Lop(Icall_imm _ | Itailcall_imm _ | Iextcall(_, _)) -> [||]
        | _ -> i.arg in
      List.iter (longest_path critical_outputs) ready_queue;
(***
      print_string "******"; print_newline();
      printed := []; print_counter := 0;
      List.iter print_node ready_queue;
***)
      reschedule ready_queue 0 (schedule i)

(* Entry point *)
(* Don't bother to schedule for initialization code and the like. *)

let fundecl f =
  if Proc.need_scheduling & f.fun_fast then begin
    let new_body = schedule f.fun_body in
    clear_code_dag();
    { fun_name = f.fun_name;
      fun_body = new_body;
      fun_fast = f.fun_fast }
  end else
    f
