(* "Sequentialization": from C-- to sequences of pseudo-instructions
   with pseudo-registers. *)

open Misc
open Cmm
open Reg
open Selection
open Mach

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if String.length rv.(i).name > 0 then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  if Array.length rv = 1 then
    rv.(0).name <- Ident.name id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).name <- Ident.name id ^ "#" ^ string_of_int i
    done

(* Buffering of instruction sequences *)

type instruction_sequence = instruction ref

let new_sequence() = ref dummy_instr

let insert desc arg res seq =
  seq := instr_cons desc arg res !seq

let extract_sequence seq =
  let rec extract res i =
    if i == dummy_instr
    then res
    else extract (instr_cons i.desc i.arg i.res res) i.next in
  extract (end_instr()) !seq

(* Insert a sequence of moves from one pseudoreg set to another. *)

let insert_moves src dst seq =
  for i = 0 to Array.length src - 1 do
    if src.(i).stamp <> dst.(i).stamp then
      insert (Iop Imove) [|src.(i)|] [|dst.(i)|] seq
  done

(* Insert moves and stackstores for function arguments and function results *)

let insert_move_args arg loc stacksize seq =
  if stacksize <> 0 then insert (Iop(Istackoffset stacksize)) [||] [||] seq;
  insert_moves arg loc seq

let insert_move_results loc res stacksize seq =
  if stacksize <> 0 then insert(Iop(Istackoffset(-stacksize))) [||] [||] seq;
  insert_moves loc res seq

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join r1 seq1 r2 seq2 =
  if Array.length r1 = 0 then r2
  else if Array.length r2 = 0 then r1
  else begin insert_moves r2 r1 seq2; r1 end

(* Same, for N branches *)

let join_array rs =
  let dest = ref [||] in
  for i = 0 to Array.length rs - 1 do
    let (r, s) = rs.(i) in
    if Array.length r > 0 then dest := r
  done;
  if Array.length !dest > 0 then
    for i = 0 to Array.length rs - 1 do
      let (r, s) = rs.(i) in
      if Array.length r > 0 then insert_moves r !dest s
    done;
  !dest

(* Add the instructions for the given expression
   at the end of the given sequence *)

let rec emit_expr env exp seq =
  match exp with
    Sconst c ->
      let ty =
        match c with
          Const_int n -> typ_int
        | Const_float f -> typ_float
        | Const_symbol s -> typ_addr
        | Const_pointer n -> typ_addr in
      let r = Reg.newv ty in
      insert (Iop(Iconstant c)) [||] r seq;
      r
  | Svar v ->
      begin try
        Tbl.find v env
      with Not_found ->
        fatal_error("Sequence.emit_expr: unbound var " ^ Ident.name v)
      end
  | Slet(v, e1, e2) ->
      emit_expr (emit_let env v e1 seq) e2 seq
  | Sassign(v, e1) ->
      let rv =
        try
          Tbl.find v env
        with Not_found ->
          fatal_error ("Sequence.emit_expr: unbound var " ^ Ident.name v) in
      let r1 = emit_expr env e1 seq in
      insert_moves r1 rv seq;
      [||]
  | Stuple(ev, perm) ->
      let rv = Array.new (Array.length ev) [||] in
      List.iter (fun i -> rv.(i) <- emit_expr env ev.(i) seq) perm;
      Array.concat(Array.to_list rv)
  | Sop(Icall_ind, e1, ty) ->
      Proc.contains_calls := true;
      let r1 = emit_expr env e1 seq in
      let rarg = Array.sub r1 1 (Array.length r1 - 1) in
      let rd = Reg.newv ty in
      let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
      let loc_res = Proc.loc_results rd in
      insert_move_args rarg loc_arg stack_ofs seq;
      insert (Iop Icall_ind) (Array.append [|r1.(0)|] loc_arg) loc_res seq;
      insert_move_results loc_res rd stack_ofs seq;
      rd
  | Sop(Icall_imm lbl, e1, ty) ->
      Proc.contains_calls := true;
      let r1 = emit_expr env e1 seq in
      let rd = Reg.newv ty in
      let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
      let loc_res = Proc.loc_results rd in
      insert_move_args r1 loc_arg stack_ofs seq;
      insert (Iop(Icall_imm lbl)) loc_arg loc_res seq;
      insert_move_results loc_res rd stack_ofs seq;
      rd
  | Sop(Iextcall lbl, e1, ty) ->
      Proc.contains_calls := true;
      let r1 = emit_expr env e1 seq in
      let rd = Reg.newv ty in
      let (loc_arg, stack_ofs) = Proc.loc_external_arguments r1 in
      let loc_res = Proc.loc_external_results rd in
      insert_move_args r1 loc_arg stack_ofs seq;
      insert (Iop(Iextcall lbl)) loc_arg loc_res seq;
      insert_move_results loc_res rd stack_ofs seq;
      rd
  | Sop(Iload(Word, addr), e1, ty) ->
      let r1 = emit_expr env e1 seq in
      let rd = Reg.newv ty in
      let a = ref addr in
      for i = 0 to Array.length ty - 1 do
        insert(Iop(Iload(Word, !a))) r1 [|rd.(i)|] seq;
        a := Arch.offset_addressing !a (size_component ty.(i))
      done;
      rd
  | Sop(Istore(Word, addr), e1, _) ->
      let r1 = emit_expr env e1 seq in
      let na = Arch.num_args_addressing addr in
      let ra = Array.sub r1 0 na in
      let a = ref addr in
      for i = na to Array.length r1 - 1 do
        insert(Iop(Istore(Word, !a))) (Array.append [|r1.(i)|] ra) [||] seq;
        a := Arch.offset_addressing !a (size_component r1.(i).typ)
      done;
      [||]
  | Sop(Ialloc _, e1, _) ->
      Proc.contains_calls := true;
      let r1 = emit_expr env e1 seq in
      let rd = Reg.newv typ_addr in
      insert (Iop(Ialloc(Cmm.size_machtype(Array.map (fun r -> r.typ) r1))))
             [||] rd seq;
      let a =
        ref (Arch.offset_addressing Arch.identity_addressing
                                    (-Arch.size_int)) in
      for i = 0 to Array.length r1 - 1 do
        insert(Iop(Istore(Word, !a))) [|r1.(i); rd.(0)|] [||] seq;
        a := Arch.offset_addressing !a (size_component r1.(i).typ)
      done;
      rd
  | Sop(op, e1, ty) ->
      begin match op with
        Imodify -> Proc.contains_calls := true | _ -> ()
      end;
      let r1 = emit_expr env e1 seq in
      let rd = Reg.newv ty in
      begin try
        (* Offer the processor description an opportunity to insert moves
           before and after the operation, i.e. for two-address instructions,
           or instructions using dedicated registers. *)
        let (rsrc, rdst) = Proc.pseudoregs_for_operation op r1 rd in
        insert_moves r1 rsrc seq;
        insert (Iop op) rsrc rdst seq;
        insert_moves rdst rd seq
      with Proc.Use_default ->
        (* Assume no constraints on arg and res registers *)
        insert (Iop op) r1 rd seq
      end;
      rd
  | Sproj(e1, ofs, len) ->
      let r1 = emit_expr env e1 seq in
      Array.sub r1 ofs len
  | Ssequence(e1, e2) ->
      emit_expr env e1 seq;
      emit_expr env e2 seq
  | Sifthenelse(cond, earg, eif, eelse) ->
      let rarg = emit_expr env earg seq in
      let (rif, sif) = emit_sequence env eif in
      let (relse, selse) = emit_sequence env eelse in
      let r = join rif sif relse selse in
      insert (Iifthenelse(cond, extract_sequence sif, extract_sequence selse))
             rarg [||] seq;
      r
  | Sswitch(esel, index, ecases) ->
      let rsel = emit_expr env esel seq in
      let rscases = Array.map (emit_sequence env) ecases in
      let r = join_array rscases in
      insert (Iswitch(index,
                      Array.map (fun (r, s) -> extract_sequence s) rscases))
             rsel [||] seq;
      r
  | Sloop(ebody) ->
      let (rarg, sbody) = emit_sequence env ebody in
      insert (Iloop(extract_sequence sbody)) [||] [||] seq;
      [||]
  | Scatch(e1, e2) ->
      let (r1, s1) = emit_sequence env e1 in
      let (r2, s2) = emit_sequence env e2 in
      let r = join r1 s1 r2 s2 in
      insert (Icatch(extract_sequence s1, extract_sequence s2)) [||] [||] seq;
      r
  | Sexit ->
      insert Iexit [||] [||] seq;
      [||]
  | Strywith(e1, v, e2) ->
      let (r1, s1) = emit_sequence env e1 in
      let rv = Reg.newv typ_addr in
      let (r2, s2) = emit_sequence (Tbl.add v rv env) e2 in
      let r = join r1 s1 r2 s2 in
      insert
        (Itrywith(extract_sequence s1,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                     (extract_sequence s2)))
        [||] [||] seq;
      r
  | Sraise e1 ->
      let r1 = emit_expr env e1 seq in
      insert Iraise r1 [||] seq;
      [||]

and emit_sequence env exp =
  let seq = new_sequence() in
  let r = emit_expr env exp seq in
  (r, seq)

and emit_let env v e1 seq =
  let r1 = emit_expr env e1 seq in
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    Tbl.add v r1 env
  end else begin
    let rv = Array.new (Array.length r1) Reg.dummy in
    for i = 0 to Array.length r1 - 1 do rv.(i) <- Reg.new r1.(i).typ done;
    name_regs v rv;
    insert_moves r1 rv seq;
    Tbl.add v rv env
  end

(* Same, but in tail position *)

let emit_return env exp seq =
  let r = emit_expr env exp seq in
  let loc = Proc.loc_results r in
  insert_moves r loc seq;
  insert Ireturn loc [||] seq

let rec emit_tail env exp seq =
  match exp with
    Slet(v, e1, e2) ->
      emit_tail (emit_let env v e1 seq) e2 seq
  | Sop(Icall_ind, e1, ty) ->
      let r1 = emit_expr env e1 seq in
      let rarg = Array.sub r1 1 (Array.length r1 - 1) in
      let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
      if stack_ofs <> 0 then
        emit_return env exp seq
      else begin
        insert_moves rarg loc_arg seq;
        insert (Iop Itailcall_ind) (Array.append [|r1.(0)|] loc_arg) [||] seq
      end
  | Sop(Icall_imm lbl, e1, ty) ->
      let r1 = emit_expr env e1 seq in
      let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
      if stack_ofs <> 0 then
        emit_return env exp seq
      else begin
        insert_moves r1 loc_arg seq;
        insert (Iop(Itailcall_imm lbl)) loc_arg [||] seq
      end
  | Ssequence(e1, e2) ->
      emit_expr env e1 seq;
      emit_tail env e2 seq
  | Sifthenelse(cond, earg, eif, eelse) ->
      let rarg = emit_expr env earg seq in
      insert (Iifthenelse(cond, emit_tail_sequence env eif,
                                emit_tail_sequence env eelse))
             rarg [||] seq
  | Sswitch(esel, index, ecases) ->
      let rsel = emit_expr env esel seq in
      insert (Iswitch(index, Array.map (emit_tail_sequence env) ecases))
             rsel [||] seq
  | Scatch(e1, e2) ->
      insert (Icatch(emit_tail_sequence env e1, emit_tail_sequence env e2))
             [||] [||] seq
  | Sexit ->
      insert Iexit [||] [||] seq
  | Sraise e1 ->
      let r1 = emit_expr env e1 seq in
      let rd = [|Proc.loc_exn_bucket|] in
      insert (Iop Imove) r1 rd seq;
      insert Iraise rd [||] seq
  | _ ->
      emit_return env exp seq

and emit_tail_sequence env exp =
  let seq = new_sequence() in
  emit_tail env exp seq;
  extract_sequence seq

(* Sequentialization of a function definition *)

let fundecl f =
  Proc.contains_calls := false;
  let rargs =
    List.map
      (fun (id, ty) -> let r = Reg.newv ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters rarg in
  let env =
    List.fold_right2
      (fun (id, ty) r env -> Tbl.add id r env)
      f.Cmm.fun_args rargs Tbl.empty in
  let seq = new_sequence() in
  insert_moves loc_arg rarg seq;
  emit_tail env (Selection.expression f.Cmm.fun_body) seq;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = extract_sequence seq }
