(* Instruction selection and choice of evaluation order. *)

open Misc
open Cmm
open Mach

type expression =
    Sconst of Cmm.constant
  | Svar of Ident.t
  | Slet of Ident.t * expression * expression
  | Sassign of Ident.t * expression
  | Stuple of expression array * int list
  | Sop of operation * expression * Cmm.machtype
  | Sproj of expression * int * int
  | Ssequence of expression * expression
  | Sifthenelse of test * expression * expression * expression
  | Sswitch of expression * int array * expression array
  | Sloop of expression * test * expression
  | Scatch of expression * expression
  | Sexit
  | Strywith of expression * Ident.t * expression
  | Sraise of expression

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply ty -> ty
  | Cextcall(s, ty) -> ty
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
  | _ -> fatal_error "Selection.oper_result_type"

(* Estimate the intrinsic cost of an operation.
   The cost reflects both the number of registers destroyed by the operation
   and the time it will take to complete. Since subexpressions with higher
   cost are evaluated first, this increases slightly the probability that
   the result will be ready when needed. *)
   
let oper_cost = function
    Capply ty -> 32
  | Cextcall(s, ty) -> 16
  | Cload ty -> 2 * Array.length ty
  | Cloadchunk c -> 2
  | Cmuli -> 3
  | Cdivi | Cmodi -> 5
  | Caddf | Csubf | Cmulf | Cdivf -> 3
  | _ -> 1

(* Common instruction selection for operations *)

let rec sel_oper op args =
  match (op, args) with
    (Capply ty, Cconst(Const_symbol s) :: rem) -> (Icall_imm s, Ctuple rem)
  | (Capply ty, _) -> (Icall_ind, Ctuple args)
  | (Cextcall(s, ty), _) -> (Iextcall s, Ctuple args)
  | (Cload ty, [arg]) ->
      let (addr, eloc) = Proc.select_addressing arg in
      (Iload(Word, addr), eloc)
  | (Cloadchunk chunk, [arg]) ->
      let (addr, eloc) = Proc.select_addressing arg in
      (Iload(chunk, addr), eloc)
  | (Cstore, arg1 :: rem) ->
      let (addr, eloc) = Proc.select_addressing arg1 in
      (Istore(Word, addr), Ctuple(eloc :: rem))
  | (Cstorechunk chunk, arg1 :: rem) ->
      let (addr, eloc) = Proc.select_addressing arg1 in
      (Istore(chunk, addr), Ctuple(eloc :: rem))
  | (Calloc, _) -> (Ialloc 0, Ctuple args)
  | (Cmodify, [arg]) -> (Imodify, arg)
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
  | (Caddf, _) -> (Iaddf, Ctuple args)
  | (Csubf, _) -> (Isubf, Ctuple args)  
  | (Cmulf, _) -> (Imulf, Ctuple args)  
  | (Cdivf, _) -> (Idivf, Ctuple args)
  | (Cfloatofint, _) -> (Ifloatofint, Ctuple args)
  | (Cintoffloat, _) -> (Iintoffloat, Ctuple args)
  | _ -> fatal_error "Selection.sel_oper"

and sel_arith_comm op = function
    [arg; Cconst(Const_int n)] when Proc.is_immediate n ->
      (Iintop_imm(op, n), arg)
  | [arg; Cconst(Const_pointer n)] when Proc.is_immediate n ->
      (Iintop_imm(op, n), arg)
  | [Cconst(Const_int n); arg] when Proc.is_immediate n ->
      (Iintop_imm(op, n), arg)
  | [Cconst(Const_pointer n); arg] when Proc.is_immediate n ->
      (Iintop_imm(op, n), arg)
  | args ->
      (Iintop op, Ctuple args)

and sel_arith op = function
    [arg; Cconst(Const_int n)] when Proc.is_immediate n ->
      (Iintop_imm(op, n), arg)
  | [arg; Cconst(Const_pointer n)] when Proc.is_immediate n ->
      (Iintop_imm(op, n), arg)
  | args ->
      (Iintop op, Ctuple args)

and sel_arith_comp cmp = function
    [arg; Cconst(Const_int n)] when Proc.is_immediate n ->
      (Iintop_imm(Icomp cmp, n), arg)
  | [arg; Cconst(Const_pointer n)] when Proc.is_immediate n ->
      (Iintop_imm(Icomp cmp, n), arg)
  | [Cconst(Const_int n); arg] when Proc.is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), arg)
  | [Cconst(Const_pointer n); arg] when Proc.is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), arg)
  | args ->
      (Iintop(Icomp cmp), Ctuple args)

and swap_intcomp = function
    Isigned cmp -> Isigned(swap_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_comparison cmp)

(* Instruction selection for conditionals *)

let sel_condition = function
    Cop(Ccmpi cmp, [arg1; Cconst(Const_int n)]) ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst(Const_int n); arg2]) ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst(Const_pointer n)]) ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst(Const_pointer n); arg2]) ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args) ->
      (Ifloattest cmp, Ctuple args)
  | Cconst(Const_int n) ->
      ((if n <> 0 then Ialwaystrue else Ialwaysfalse), Ctuple [])
  | arg ->
      (Itruetest, arg)

(* Flattening of tuples *)

let rec flatten_tuples = function
    [] -> []
  | Ctuple el :: rem -> flatten_tuples el @ flatten_tuples rem
  | exp :: rem -> exp :: flatten_tuples rem

(* Enumerate integers *)

let rec interval lo hi =
  if lo > hi then [] else lo :: interval (lo+1) hi

(* Instruction selection and annotation for an expression *)

let rec sel_expr = function
    Cconst c ->
      (Sconst c, 0)
  | Cvar v ->
      (Svar v, 0)
  | Clet(v, e1, e2) ->
      let (s1, n1) = sel_expr e1 in
      let (s2, n2) = sel_expr e2 in
      (Slet(v, s1, s2), max n1 (n2 + 1))
  | Cassign(v, e1) ->
      let (s1, n1) = sel_expr e1 in
      (Sassign(v, s1), n1)
  | Ctuple(el) ->
      begin match flatten_tuples el with
        [] ->
          (Stuple([||], []), 0)
      | [e1] ->
          sel_expr e1
      | [e1; e2] ->
          let (s1, n1) = sel_expr e1 in
          let (s2, n2) = sel_expr e2 in
          if n1 >= n2 then
            (Stuple([|s1;s2|], [0;1]), max n1 (n2 + 1))
          else
            (Stuple([|s1;s2|], [1;0]), max n2 (n1 + 1))
      | el ->
          let sv = Array.of_list(List.map sel_expr el) in
          let perm =
            Sort.list
              (fun i j ->
                let (_, ni) = sv.(i) and (_, nj) = sv.(j) in i >= j)
              (interval 0 (Array.length sv - 1)) in
          let need = ref 0 and accu = ref 0 in
          List.iter
            (fun i ->
              let (_, ni) = sv.(i) in
              need := max !need (ni + !accu);
              incr accu)
            perm;
          let cases = Array.map (fun (s, n) -> s) sv in
          (Stuple(cases, perm), !need)
      end
  | Csequence(e1, e2) ->
      let (s1, n1) = sel_expr e1 in
      let (s2, n2) = sel_expr e2 in
      (Ssequence(s1, s2), max n1 n2)
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = sel_condition econd in
      let (sarg, narg) = sel_expr earg in
      let (sif, nif) = sel_expr eif in
      let (selse, nelse) = sel_expr eelse in
      (Sifthenelse(cond, sarg, sif, selse), max narg (max nif nelse))
  | Cswitch(esel, index, ecases) ->
      let (ssel, nsel) = sel_expr esel in
      let scases = Array.map sel_expr ecases in
      let need = ref nsel in
      for i = 0 to Array.length scases - 1 do
        let (_, n) = scases.(i) in need := max !need n
      done;
      (Sswitch(ssel, index, Array.map (fun (s, n) -> s) scases), !need)
  | Cwhile(econd, ebody) ->
      let (cond, earg) = sel_condition econd in
      let (sarg, narg) = sel_expr earg in
      let (sbody, nbody) = sel_expr ebody in
      (Sifthenelse(cond, sarg, Sloop(sbody, cond, sarg), Stuple([||], [])),
       max narg nbody)
  | Ccatch(e1, e2) ->
      let (s1, n1) = sel_expr e1 in
      let (s2, n2) = sel_expr e2 in
      (Ssequence(s1, s2), max n1 n2)
  | Cexit ->
      (Sexit, 0)
  | Ctrywith(e1, v, e2) ->
      let (s1, n1) = sel_expr e1 in
      let (s2, n2) = sel_expr e2 in
      (Strywith(s1, v, s2), max n1 (n2 + 1))
  | Cop(Cproj(ofs, len), [Cop(Cload ty, [arg])]) ->
      sel_expr
        (Cop(Cload (Array.sub ty ofs len),
             [Cop(Cadda,
                [arg; Cconst(Const_int(size_machtype(Array.sub ty 0 ofs)))])]))
  | Cop(Cproj(ofs, len), [arg]) ->
      let (s, n) = sel_expr arg in (Sproj(s, ofs, len), n)
  | Cop(Craise, [arg]) ->
      let (s, n) = sel_expr arg in (Sraise s, n)
  | Cop(op, args) ->
      let ty = oper_result_type op in
      let cost = oper_cost op in
      (* Offer the processor description a chance to do its own selection,
         e.g. to recognize processor-specific instructions *)
      try
        let (newop, newarg) = Proc.select_oper op args in
        let (sarg, narg) = sel_expr newarg in
        (Sop(newop, sarg, ty), narg + cost)
      with Proc.Use_default ->
      (* Do our own selection *)
        match op with
          Ccmpf comp ->
            let (sarg, narg) = sel_expr (Ctuple args) in
            (Sifthenelse(Ifloattest comp, sarg,
                         Sconst(Const_int 1), Sconst(Const_int 0)), narg)
        | _ ->
            let (newop, newarg) = sel_oper op args in
            let (sarg, narg) = sel_expr newarg in
            (Sop(newop, sarg, ty), narg + cost)

let expression e =
  let (s, n) = sel_expr e in s
