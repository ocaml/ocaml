(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Misc
open Cmm
open Reg
open Mach

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply ty -> ty
  | Cextcall(s, ty, alloc) -> ty
  | Cload ty -> ty
  | Cloadchunk c -> typ_int
  | Calloc -> typ_addr
  | Cstore -> typ_void
  | Cstorechunk c -> typ_void
  | Cmodify -> typ_void
  | Caddi | Csubi | Cmuli | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Cadda | Csuba -> typ_addr
  | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise -> typ_void
  | Ccheckbound -> typ_void
  | _ -> fatal_error "Selection.oper_result_type"

(* Infer the size in bytes of the result of a simple expression *)

let rec size_expr env = function
    Cconst_int _ -> Arch.size_int
  | Cconst_symbol _ | Cconst_pointer _ -> Arch.size_addr
  | Cconst_float _ -> Arch.size_float
  | Cvar v ->
      let r =
        try
          Tbl.find v env
        with Not_found ->
          fatal_error("Selection.emit_expr: unbound var " ^ Ident.name v) in
      size_machtype (Array.map (fun r -> r.typ) r)
  | Ctuple el ->
      List.fold_right (fun e sz -> size_expr env e + sz) el 0
  | Cop(op, args) ->
      size_machtype(oper_result_type op)
  | _ ->
      fatal_error "Selection.size_expr"

(* Says if an operation is "cheap". A "cheap" operation is an operation
   without side-effects and whose execution can be delayed until its value
   is really needed. In the case of e.g. an [alloc] instruction,
   the non-cheap parts of arguments are computed in right-to-left order
   first, then the block is allocated, then the cheap parts are evaluated
   and stored. *)

let cheap_operation = function
    (* The following may have side effects *)
    Capply _ | Cextcall(_, _, _) | Calloc | Cstore | Cstorechunk _ | 
    Cmodify | Craise -> false
    (* The following are expensive to compute, better start them early *)
  | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat -> false
    (* The remaining operations are cheap *)
  | _ -> true

(* Default instruction selection for operators *)

let rec sel_operation op args =
  match (op, args) with
    (Capply ty, Cconst_symbol s :: rem) -> (Icall_imm s, rem)
  | (Capply ty, _) -> (Icall_ind, args)
  | (Cextcall(s, ty, alloc), _) -> (Iextcall(s, alloc), args)
  | (Cload ty, [arg]) ->
      let (addr, eloc) = Proc.select_addressing arg in
      (Iload(Word, addr), [eloc])
  | (Cloadchunk chunk, [arg]) ->
      let (addr, eloc) = Proc.select_addressing arg in
      (Iload(chunk, addr), [eloc])
  | (Cstore, arg1 :: rem) ->
      let (addr, eloc) = Proc.select_addressing arg1 in
      (Istore(Word, addr), eloc :: rem)
  | (Cstorechunk chunk, arg1 :: rem) ->
      let (addr, eloc) = Proc.select_addressing arg1 in
      (Istore(chunk, addr), eloc :: rem)
  | (Calloc, _) -> (Ialloc 0, args)
  | (Cmodify, _) -> (Imodify, args)
  | (Caddi, _) -> sel_arith_comm Iadd args
  | (Csubi, _) -> sel_arith Isub args
  | (Cmuli, _) -> sel_arith_comm Imul args
  | (Cdivi, _) -> sel_arith Idiv args
  | (Cmodi, _) -> sel_arith_comm Imod args
  | (Cand, _) -> sel_arith_comm Iand args
  | (Cor, _) -> sel_arith_comm Ior args
  | (Cxor, _) -> sel_arith_comm Ixor args
  | (Clsl, _) -> sel_arith Ilsl args
  | (Clsr, _) -> sel_arith Ilsr args
  | (Casr, _) -> sel_arith Iasr args
  | (Ccmpi comp, _) -> sel_arith_comp (Isigned comp) args
  | (Cadda, _) -> sel_arith_comm Iadd args
  | (Csuba, _) -> sel_arith Isub args
  | (Ccmpa comp, _) -> sel_arith_comp (Iunsigned comp) args
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)  
  | (Cmulf, _) -> (Imulf, args)  
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound, _) -> sel_arith Icheckbound args
  | _ -> fatal_error "Selection.sel_oper"

and sel_arith_comm op = function
    [arg; Cconst_int n] when Proc.is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when Proc.is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when Proc.is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_pointer n; arg] when Proc.is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

and sel_arith op = function
    [arg; Cconst_int n] when Proc.is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when Proc.is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

and sel_arith_comp cmp = function
    [arg; Cconst_int n] when Proc.is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [arg; Cconst_pointer n] when Proc.is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int n; arg] when Proc.is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | [Cconst_pointer n; arg] when Proc.is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

and swap_intcomp = function
    Isigned cmp -> Isigned(swap_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_comparison cmp)

(* Instruction selection for conditionals *)

let sel_condition = function
    Cop(Ccmpi cmp, [arg1; Cconst_int n]) when Proc.is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int n; arg2]) when Proc.is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_pointer n]) when Proc.is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_pointer n; arg2]) when Proc.is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args) ->
      (Ifloattest cmp, Ctuple args)
  | Cop(Cand, [arg; Cconst_int 1]) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

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

let insert_move src dst seq =
  if src.stamp <> dst.stamp then
    insert (Iop Imove) [|src|] [|dst|] seq

let insert_moves src dst seq =
  for i = 0 to Array.length src - 1 do
    insert_move src.(i) dst.(i) seq
  done

(* Insert moves and stack offsets for function arguments and results *)

let insert_move_args arg loc stacksize seq =
  if stacksize <> 0 then insert (Iop(Istackoffset stacksize)) [||] [||] seq;
  insert_moves arg loc seq

let insert_move_results loc res stacksize seq =
  if stacksize <> 0 then insert(Iop(Istackoffset(-stacksize))) [||] [||] seq;
  insert_moves loc res seq

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join r1 seq1 r2 seq2 =
  let l1 = Array.length r1 and l2 = Array.length r2 in
  if l1 = 0 then r2
  else if l2 = 0 then r1
  else begin
    let r = Array.new l1 Reg.dummy in
    for i = 0 to l1-1 do
      if String.length r1.(i).name = 0 then begin
        r.(i) <- r1.(i);
        insert_move r2.(i) r1.(i) seq2
      end else if String.length r2.(i).name = 0 then begin
        r.(i) <- r2.(i);
        insert_move r1.(i) r2.(i) seq1
      end else begin
        r.(i) <- Reg.new r1.(i).typ;
        insert_move r1.(i) r.(i) seq1;
        insert_move r2.(i) r.(i) seq2
      end
    done;
    r
  end

(* Same, for N branches *)

let join_array rs =
  let some_res = ref [||] in
  for i = 0 to Array.length rs - 1 do
    let (r, s) = rs.(i) in
    if Array.length r > 0 then some_res := r
  done;
  let size_res = Array.length !some_res in
  if size_res = 0 then [||] else begin
    let res = Array.new size_res Reg.dummy in
    for i = 0 to size_res - 1 do
      res.(i) <- Reg.new (!some_res).(i).typ
    done;
    for i = 0 to Array.length rs - 1 do
      let (r, s) = rs.(i) in
      if Array.length r > 0 then insert_moves r res s
    done;
    res
  end

(* Add the instructions for the given expression
   at the end of the given sequence *)

let rec emit_expr env exp seq =
  match exp with
    Cconst_int n ->
      let r = Reg.newv typ_int in
      insert (Iop(Iconst_int n)) [||] r seq;
      r
  | Cconst_float n ->
      let r = Reg.newv typ_float in
      insert (Iop(Iconst_float n)) [||] r seq;
      r
  | Cconst_symbol n ->
      let r = Reg.newv typ_addr in
      insert (Iop(Iconst_symbol n)) [||] r seq;
      r
  | Cconst_pointer n ->
      let r = Reg.newv typ_addr in
      insert (Iop(Iconst_int n)) [||] r seq;
      r
  | Cvar v ->
      begin try
        Tbl.find v env
      with Not_found ->
        fatal_error("Selection.emit_expr: unbound var " ^ Ident.name v)
      end
  | Clet(v, e1, e2) ->
      emit_expr (emit_let env v e1 seq) e2 seq
  | Cassign(v, e1) ->
      let rv =
        try
          Tbl.find v env
        with Not_found ->
          fatal_error ("Selection.emit_expr: unbound var " ^ Ident.name v) in
      let r1 = emit_expr env e1 seq in
      insert_moves r1 rv seq;
      [||]
  | Ctuple exp_list ->
      let (simple_list, ext_env) = emit_parts_list env exp_list seq in
      emit_tuple ext_env simple_list seq
  | Cop(Cproj(ofs, len), [Cop(Cload ty, [arg])]) ->
      let byte_offset = size_machtype(Array.sub ty 0 ofs) in
      emit_expr env
        (Cop(Cload(Array.sub ty ofs len),
             [Cop(Cadda, [arg; Cconst_int byte_offset])])) seq
  | Cop(Cproj(ofs, len), [arg]) ->
      let r = emit_expr env arg seq in
      Array.sub r ofs len
  | Cop(Craise, [arg]) ->
      let r1 = emit_expr env arg seq in
      let rd = [|Proc.loc_exn_bucket|] in
      insert (Iop Imove) r1 rd seq;
      insert Iraise rd [||] seq;
      [||]
  | Cop(Ccmpf comp, args) ->
      emit_expr env (Cifthenelse(exp, Cconst_int 1, Cconst_int 0)) seq
  | Cop(op, args) ->
      let (simple_args, env) = emit_parts_list env args seq in
      let ty = oper_result_type op in
      let (new_op, new_args) =
        try
          Proc.select_oper op simple_args
        with Proc.Use_default ->
          sel_operation op simple_args in
      begin match new_op with
        Icall_ind ->
          Proc.contains_calls := true;
          let r1 = emit_tuple env new_args seq in
          let rarg = Array.sub r1 1 (Array.length r1 - 1) in
          let rd = Reg.newv ty in
          let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
          let loc_res = Proc.loc_results rd in
          insert_move_args rarg loc_arg stack_ofs seq;
          insert (Iop Icall_ind) (Array.append [|r1.(0)|] loc_arg) loc_res seq;
          insert_move_results loc_res rd stack_ofs seq;
          rd
      | Icall_imm lbl ->
          Proc.contains_calls := true;
          let r1 = emit_tuple env new_args seq in
          let rd = Reg.newv ty in
          let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
          let loc_res = Proc.loc_results rd in
          insert_move_args r1 loc_arg stack_ofs seq;
          insert (Iop(Icall_imm lbl)) loc_arg loc_res seq;
          insert_move_results loc_res rd stack_ofs seq;
          rd
      | Iextcall(lbl, alloc) ->
          Proc.contains_calls := true;
          let r1 = emit_tuple env new_args seq in
          let rd = Reg.newv ty in
          let (loc_arg, stack_ofs) = Proc.loc_external_arguments r1 in
          let loc_res = Proc.loc_external_results rd in
          insert_move_args r1 loc_arg stack_ofs seq;
          insert (Iop(Iextcall(lbl, alloc))) loc_arg loc_res seq;
          insert_move_results loc_res rd stack_ofs seq;
          rd
      | Iload(Word, addr) ->
          let r1 = emit_tuple env new_args seq in
          let rd = Reg.newv ty in
          let a = ref addr in
          for i = 0 to Array.length ty - 1 do
            insert(Iop(Iload(Word, !a))) r1 [|rd.(i)|] seq;
            a := Arch.offset_addressing !a (size_component ty.(i))
          done;
          rd
      | Istore(Word, addr) ->
          begin match new_args with
            [] -> fatal_error "Selection.Istore"
          | arg_addr :: args_data ->
              let ra = emit_expr env arg_addr seq in
              emit_stores env args_data seq ra addr;
              [||]
          end
      | Istore(chunk, addr) ->
          begin match new_args with
            [arg_addr; arg_data] ->
              let ra = emit_expr env arg_addr seq in
              let rd = emit_expr env arg_data seq in
              insert (Iop(Istore(chunk, addr))) (Array.append rd ra) [||] seq;
              [||]
          | _ -> fatal_error "Selection.Istorechunk"
          end
      | Ialloc _ ->
          Proc.contains_calls := true;
          let rd = Reg.newv typ_addr in
          let size = size_expr env (Ctuple new_args) in
          insert (Iop(Ialloc size)) [||] rd seq;
          emit_stores env new_args seq rd 
            (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int));
          rd
      | op ->
          if op = Imodify then Proc.contains_calls := true;
          let r1 = emit_tuple env new_args seq in
          let rd = Reg.newv ty in
          begin try
            (* Offer the processor description an opportunity to insert moves
               before and after the operation, i.e. for two-address 
               instructions, or instructions using dedicated registers. *)
            let (rsrc, rdst) = Proc.pseudoregs_for_operation op r1 rd in
            insert_moves r1 rsrc seq;
            insert (Iop op) rsrc rdst seq;
            insert_moves rdst rd seq
          with Proc.Use_default ->
            (* Assume no constraints on arg and res registers *)
            insert (Iop op) r1 rd seq
          end;
          rd
      end        
  | Csequence(e1, e2) ->
      emit_expr env e1 seq;
      emit_expr env e2 seq
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = sel_condition econd in
      let rarg = emit_expr env earg seq in
      let (rif, sif) = emit_sequence env eif in
      let (relse, selse) = emit_sequence env eelse in
      let r = join rif sif relse selse in
      insert (Iifthenelse(cond, extract_sequence sif, extract_sequence selse))
             rarg [||] seq;
      r
  | Cswitch(esel, index, ecases) ->
      let rsel = emit_expr env esel seq in
      let rscases = Array.map (emit_sequence env) ecases in
      let r = join_array rscases in
      insert (Iswitch(index,
                      Array.map (fun (r, s) -> extract_sequence s) rscases))
             rsel [||] seq;
      r
  | Cloop(ebody) ->
      let (rarg, sbody) = emit_sequence env ebody in
      insert (Iloop(extract_sequence sbody)) [||] [||] seq;
      [||]
  | Ccatch(e1, e2) ->
      let (r1, s1) = emit_sequence env e1 in
      let (r2, s2) = emit_sequence env e2 in
      let r = join r1 s1 r2 s2 in
      insert (Icatch(extract_sequence s1, extract_sequence s2)) [||] [||] seq;
      r
  | Cexit ->
      insert Iexit [||] [||] seq;
      [||]
  | Ctrywith(e1, v, e2) ->
      Proc.contains_calls := true;
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

and emit_parts env exp seq =
  match exp with
    Cconst_int _ | Cconst_float _ | Cconst_symbol _ | Cconst_pointer _ |
    Cvar _ ->
      (exp, env)
  | Ctuple el ->
      let (explist, env) = emit_parts_list env el seq in
      (Ctuple explist, env)
  | Clet(id, arg, body) ->
      emit_parts (emit_let env id arg seq) body seq
  | Cop(op, args) when cheap_operation op ->
      let (new_args, new_env) = emit_parts_list env args seq in
      (Cop(op, new_args), new_env)
  | _ ->
      let r = emit_expr env exp seq in
      if Array.length r = 0 then
        (Ctuple [], env)
      else begin
        let id = Ident.new "bind" in
        (Cvar id, Tbl.add id r env)
      end

and emit_parts_list env exp_list seq =
  match exp_list with
    [] -> ([], env)
  | exp :: rem ->
      (* This ensures right-to-left evaluation, consistent with the
         bytecode compiler *)
      let (new_rem, new_env) = emit_parts_list env rem seq in
      let (new_exp, fin_env) = emit_parts new_env exp seq in
      (new_exp :: new_rem, fin_env)

and emit_tuple env exp_list seq =
  Array.concat(List.map (fun e -> emit_expr env e seq) exp_list)

and emit_stores env data seq regs_addr addr =
  let a = ref addr in
  List.iter
    (fun e ->
      try
        (* Offer the machine description an opportunity to optimize
           the store, e.g. if constant -> memory or memory -> memory
           moves are available *)
        let (op, arg) = Proc.select_store !a e in
        let r = emit_expr env arg seq in
        insert (Iop op) (Array.append r regs_addr) [||] seq;
        a := Arch.offset_addressing !a (size_expr env e)
      with Proc.Use_default ->
        let r = emit_expr env e seq in
        for i = 0 to Array.length r - 1 do
          insert (Iop(Istore(Word, !a)))
                 (Array.append [|r.(i)|] regs_addr) [||] seq;
          a := Arch.offset_addressing !a (size_component r.(i).typ)
        done)
    data

(* Same, but in tail position *)

let emit_return env exp seq =
  let r = emit_expr env exp seq in
  let loc = Proc.loc_results r in
  insert_moves r loc seq;
  insert Ireturn loc [||] seq

let rec emit_tail env exp seq =
  match exp with
    Clet(v, e1, e2) ->
      emit_tail (emit_let env v e1 seq) e2 seq
  | Cop(Capply ty as op, args) ->
      let (simple_args, env) = emit_parts_list env args seq in
      let (new_op, new_args) = sel_operation op simple_args in
      begin match new_op with
        Icall_ind ->
          let r1 = emit_tuple env new_args seq in
          let rarg = Array.sub r1 1 (Array.length r1 - 1) in
          let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
          if stack_ofs = 0 then begin
            insert_moves rarg loc_arg seq;
            insert (Iop Itailcall_ind)
                   (Array.append [|r1.(0)|] loc_arg) [||] seq
          end else begin
            Proc.contains_calls := true;
            let rd = Reg.newv ty in
            let loc_res = Proc.loc_results rd in
            insert_move_args rarg loc_arg stack_ofs seq;
            insert (Iop Icall_ind)
                   (Array.append [|r1.(0)|] loc_arg) loc_res seq;
            insert(Iop(Istackoffset(-stack_ofs))) [||] [||] seq;
            insert Ireturn loc_res [||] seq
          end
      | Icall_imm lbl ->
          let r1 = emit_tuple env new_args seq in
          let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
          if stack_ofs = 0 then begin
            insert_moves r1 loc_arg seq;
            insert (Iop(Itailcall_imm lbl)) loc_arg [||] seq
          end else begin
            Proc.contains_calls := true;
            let rd = Reg.newv ty in
            let loc_res = Proc.loc_results rd in
            insert_move_args r1 loc_arg stack_ofs seq;
            insert (Iop(Icall_imm lbl)) loc_arg loc_res seq;
            insert(Iop(Istackoffset(-stack_ofs))) [||] [||] seq;
            insert Ireturn loc_res [||] seq
          end
      | _ -> fatal_error "Selection.emit_tail"
      end
  | Cop(Craise, [e1]) ->
      let r1 = emit_expr env e1 seq in
      let rd = [|Proc.loc_exn_bucket|] in
      insert (Iop Imove) r1 rd seq;
      insert Iraise rd [||] seq
  | Csequence(e1, e2) ->
      emit_expr env e1 seq;
      emit_tail env e2 seq
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = sel_condition econd in
      let rarg = emit_expr env earg seq in
      insert (Iifthenelse(cond, emit_tail_sequence env eif,
                                emit_tail_sequence env eelse))
             rarg [||] seq
  | Cswitch(esel, index, ecases) ->
      let rsel = emit_expr env esel seq in
      insert (Iswitch(index, Array.map (emit_tail_sequence env) ecases))
             rsel [||] seq
  | Ccatch(e1, e2) ->
      insert (Icatch(emit_tail_sequence env e1, emit_tail_sequence env e2))
             [||] [||] seq
  | Cexit ->
      insert Iexit [||] [||] seq
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
  emit_tail env f.Cmm.fun_body seq;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = extract_sequence seq;
    fun_fast = f.Cmm.fun_fast }
