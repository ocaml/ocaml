(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Cmm
open Reg
open Mach

module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

type environment =
  { vars : (Reg.t array
            * Backend_var.Provenance.t option
            * Asttypes.mutable_flag) V.Map.t;
    static_exceptions : Reg.t array list Int.Map.t;
    (** Which registers must be populated when jumping to the given
        handler. *)
  }

let env_add ?(mut=Asttypes.Immutable) var regs env =
  let provenance = VP.provenance var in
  let var = VP.var var in
  { env with vars = V.Map.add var (regs, provenance, mut) env.vars }

let env_add_static_exception id v env =
  { env with static_exceptions = Int.Map.add id v env.static_exceptions }

let env_find id env =
  let regs, _provenance, _mut = V.Map.find id env.vars in
  regs

let env_find_mut id env =
  let regs, _provenance, mut = V.Map.find id env.vars in
  begin match mut with
  | Asttypes.Mutable -> ()
  | Asttypes.Immutable ->
    Misc.fatal_error "Selectgen.env_find_mut: not mutable"
  end;
  regs

let env_find_static_exception id env =
  Int.Map.find id env.static_exceptions

let env_empty = {
  vars = V.Map.empty;
  static_exceptions = Int.Map.empty;
}

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply ty -> ty
  | Cextcall(_s, ty_res, _ty_args, _alloc) -> ty_res
  | Cload {memory_chunk} ->
      begin match memory_chunk with
      | Word_val -> typ_val
      | Single | Double -> typ_float
      | _ -> typ_int
      end
  | Calloc -> typ_val
  | Cstore (_c, _) -> typ_void
  | Cdls_get -> typ_val
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise _ -> typ_void
  | Ccheckbound -> typ_void
  | Copaque -> typ_val

(* Infer the size in bytes of the result of an expression whose evaluation
   may be deferred (cf. [emit_parts]). *)

let size_component = function
  | Val | Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

let size_expr (env:environment) exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _ -> Arch.size_int
    | Cconst_symbol _ ->
        Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cvar id ->
        begin try
          V.Map.find id localenv
        with Not_found ->
        try
          let regs = env_find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          Misc.fatal_error("Selection.size_expr: unbound var " ^
                           V.unique_name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, _, _) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (V.Map.add (VP.var id) (size localenv arg) localenv) body
    | Csequence(_e1, e2) ->
        size localenv e2
    | _ ->
        Misc.fatal_error "Selection.size_expr"
  in size V.Map.empty exp

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
    Isigned cmp -> Isigned(swap_integer_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_integer_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if not (Reg.anonymous rv.(i)) then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  let id = VP.var id in
  if Array.length rv = 1 then
    rv.(0).raw_name <- Raw_name.create_from_var id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).raw_name <- Raw_name.create_from_var id;
      rv.(i).part <- Some i
    done

(* Name of function being compiled *)
let current_function_name = ref ""

module Effect = struct
  type t =
    | None
    | Raise
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Raise, Raise -> Raise
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let pure = function
    | None -> true
    | Raise | Arbitrary -> false
end

module Coeffect = struct
  type t =
    | None
    | Read_mutable
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Read_mutable, Read_mutable -> Read_mutable
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let copure = function
    | None -> true
    | Read_mutable | Arbitrary -> false
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end = struct
  type t = Effect.t * Coeffect.t

  let none = Effect.None, Coeffect.None
  let arbitrary = Effect.Arbitrary, Coeffect.Arbitrary

  let effect (e, _ce) = e
  let coeffect (_e, ce) = ce

  let pure_and_copure (e, ce) = Effect.pure e && Coeffect.copure ce

  let effect_only e = e, Coeffect.None
  let coeffect_only ce = Effect.None, ce

  let join (e1, ce1) (e2, ce2) =
    Effect.join e1 e2, Coeffect.join ce1 ce2

  let join_list_map xs f =
    match xs with
    | [] -> none
    | x::xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs
end

type selector_state = Mach.instruction

type selector =
  { is_immediate : selector -> Mach.integer_operation -> int -> bool;
    is_immediate_test : selector -> Mach.integer_comparison -> int -> bool;
    select_addressing :
      selector ->
      Cmm.memory_chunk ->
      Cmm.expression ->
      Arch.addressing_mode * Cmm.expression;
    is_simple_expr : selector -> Cmm.expression -> bool;
    effects_of : selector -> Cmm.expression -> Effect_and_coeffect.t;
    select_operation :
      selector ->
      Cmm.operation ->
      Cmm.expression list ->
      Debuginfo.t ->
      Mach.operation * Cmm.expression list;
    select_condition : selector -> Cmm.expression -> Mach.test * Cmm.expression;
    select_store :
      selector ->
      bool ->
      Arch.addressing_mode ->
      Cmm.expression ->
      Mach.operation * Cmm.expression;
    regs_for : selector -> Cmm.machtype -> Reg.t array;
    insert_op :
      selector ->
      environment ->
      Mach.operation ->
      Reg.t array ->
      Reg.t array ->
      Reg.t array;
    insert_op_debug :
      selector ->
      environment ->
      Mach.operation ->
      Debuginfo.t ->
      Reg.t array ->
      Reg.t array ->
      Reg.t array;
    insert_move_extcall_arg :
      selector ->
      environment ->
      Cmm.exttype ->
      Reg.t array ->
      Reg.t array ->
      unit;
    emit_extcall_args :
      selector ->
      environment ->
      Cmm.exttype list ->
      Cmm.expression list ->
      Reg.t array * int;
    emit_stores :
      selector ->
      environment ->
      Cmm.expression list ->
      Reg.t array -> unit;
    mark_call : selector -> unit;
    mark_tailcall : selector -> unit;
    mark_c_tailcall : selector -> unit;
    mark_instr : selector -> Mach.instruction_desc -> unit;
    contains_calls : bool ref;
    mutable instr_seq : selector_state;
  }

(* Wrappers *)

let is_immediate self op n =
  self.is_immediate self op n

let is_immediate_test self cmp n =
  self.is_immediate_test self cmp n

let select_addressing self chunk exp =
  self.select_addressing self chunk exp

let is_simple_expr self expr =
  self.is_simple_expr self expr

let effects_of self expr =
  self.effects_of self expr

let select_operation self op args dbg =
  self.select_operation self op args dbg

let select_condition self test =
  self.select_condition self test

let select_store self is_assign addr arg =
  self.select_store self is_assign addr arg

let regs_for self ty =
  self.regs_for self ty

let insert_op self env op rs rd =
  self.insert_op self env op rs rd

let insert_op_debug self env op dbg rs rd =
  self.insert_op_debug self env op dbg rs rd

let insert_move_extcall_arg self env ty_arg src dst =
  self.insert_move_extcall_arg self env ty_arg src dst

let emit_extcall_args self env ty_args args =
  self.emit_extcall_args self env ty_args args

let emit_stores self env data regs_addr =
  self.emit_stores self env data regs_addr

let mark_call self =
  self.mark_call self

let mark_tailcall self =
  self.mark_tailcall self

let mark_c_tailcall self =
  self.mark_c_tailcall self

let mark_instr self instr =
  self.mark_instr self instr


(* The default instruction selection class *)

(* A syntactic criterion used in addition to judgements about (co)effects as
   to whether the evaluation of a given expression may be deferred by
   [emit_parts].  This criterion is a property of the instruction selection
   algorithm in this file rather than a property of the Cmm language.
*)
let default_is_simple_expr self = function
    Cconst_int _ -> true
  | Cconst_natint _ -> true
  | Cconst_float _ -> true
  | Cconst_symbol _ -> true
  | Cvar _ -> true
  | Ctuple el -> List.for_all (is_simple_expr self) el
  | Clet(_id, arg, body) | Clet_mut(_id, _, arg, body) ->
    is_simple_expr self arg && is_simple_expr self body
  | Cphantom_let(_var, _defining_expr, body) -> is_simple_expr self body
  | Csequence(e1, e2) ->
      is_simple_expr self e1 && is_simple_expr self e2
  | Cop(op, args, _) ->
      begin match op with
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc | Cstore _ | Craise _ | Copaque -> false
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf
      | Cabsf | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Ccmpf _ | Ccheckbound | Cdls_get ->
          List.for_all (is_simple_expr self) args
      end
  | Cassign _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _
  | Ctrywith _ -> false

(* Analyses the effects and coeffects of an expression.  This is used across
   a whole list of expressions with a view to determining which expressions
   may have their evaluation deferred.  The result of this function, modulo
   target-specific judgements if the [effects_of] method is overridden, is a
   property of the Cmm language rather than anything particular about the
   instruction selection algorithm in this file.

   In the case of e.g. an OCaml function call, the arguments whose evaluation
   cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
   order first with their results going into temporaries, then the block is
   allocated, then the remaining arguments are evaluated before being
   combined with the temporaries. *)
let default_effects_of self exp =
  let module EC = Effect_and_coeffect in
  match exp with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cvar _ -> EC.none
  | Ctuple el -> EC.join_list_map el (effects_of self)
  | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
    EC.join (effects_of self arg) (effects_of self body)
  | Cphantom_let (_var, _defining_expr, body) -> effects_of self body
  | Csequence (e1, e2) ->
    EC.join (effects_of self e1) (effects_of self e2)
  | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
    EC.join (effects_of self cond)
      (EC.join (effects_of self ifso) (effects_of self ifnot))
  | Cop (op, args, _) ->
    let from_op =
      match op with
      | Capply _ | Cextcall _ | Copaque -> EC.arbitrary
      | Calloc -> EC.none
      | Cstore _ -> EC.effect_only Effect.Arbitrary
      | Craise _ | Ccheckbound -> EC.effect_only Effect.Raise
      | Cload {mutability = Asttypes.Immutable} -> EC.none
      | Cload {mutability = Asttypes.Mutable} | Cdls_get ->
          EC.coeffect_only Coeffect.Read_mutable
      | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor
      | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf | Cabsf
      | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat | Ccmpf _ ->
        EC.none
    in
    EC.join from_op (EC.join_list_map args (effects_of self))
  | Cassign _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    EC.arbitrary

(* Says whether an integer constant is a suitable immediate argument for
   the given integer operation *)

let default_is_immediate _self op n =
  match op with
  | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
  | _ -> false

(* Default instruction selection for stores (of words) *)

let default_select_store _self is_assign addr arg =
  (Istore(Word_val, addr, is_assign), arg)

let default_mark_call self =
  self.contains_calls := true

let default_mark_tailcall _self = ()

let default_mark_c_tailcall _self = ()

let default_mark_instr self = function
  | Iop (Icall_ind | Icall_imm _ | Iextcall _) ->
      mark_call self
  | Iop (Itailcall_ind | Itailcall_imm _) ->
      mark_tailcall self
  | Iop (Ialloc _) | Iop (Ipoll _) ->
      (* caml_alloc*, caml_garbage_collection (incl. polls) *)
      mark_call self
  | Iop (Iintop (Icheckbound) | Iintop_imm(Icheckbound, _)) ->
      mark_c_tailcall self (* caml_ml_array_bound_error *)
  | Iraise raise_kind ->
    begin match raise_kind with
      | Lambda.Raise_notrace -> ()
      | Lambda.Raise_regular
      | Lambda.Raise_reraise ->
          (* PR#6239 *)
        (* caml_stash_backtrace; we #mark_call rather than
           #mark_c_tailcall to get a good stack backtrace *)
          mark_call self
    end
  | Itrywith _ ->
    mark_call self
  | _ -> ()

(* Default instruction selection for operators *)

let select_arith_comm self op = function
  | [arg; Cconst_int (n, _)] when is_immediate self op n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int (n, _); arg] when is_immediate self op n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

let select_arith self op = function
  | [arg; Cconst_int (n, _)] when is_immediate self op n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

let select_arith_comp self cmp = function
  | [arg; Cconst_int (n, _)] when is_immediate self (Icomp cmp) n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int (n, _); arg]
    when is_immediate self (Icomp(swap_intcomp cmp)) n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

let default_select_operation self op args _dbg =
  match (op, args) with
  | (Capply _, Cconst_symbol (func, _dbg) :: rem) ->
    (Icall_imm { func; }, rem)
  | (Capply _, _) ->
    (Icall_ind, args)
  | (Cextcall(func, ty_res, ty_args, alloc), _) ->
    Iextcall { func; alloc; ty_res; ty_args; stack_ofs = -1}, args
  | (Cload {memory_chunk; mutability; is_atomic}, [arg]) ->
      let (addressing_mode, eloc) =
        select_addressing self memory_chunk arg
      in
      (Iload {memory_chunk; addressing_mode; mutability; is_atomic}, [eloc])
  | (Cstore (chunk, init), [arg1; arg2]) ->
      let (addr, eloc) = select_addressing self chunk arg1 in
      let is_assign =
        match init with
        | Lambda.Root_initialization -> false
        | Lambda.Heap_initialization -> false
        | Lambda.Assignment -> true
      in
      if chunk = Word_int || chunk = Word_val then begin
        let (op, newarg2) = select_store self is_assign addr arg2 in
        (op, [newarg2; eloc])
      end else begin
        (Istore(chunk, addr, is_assign), [arg2; eloc])
        (* Inversion addr/datum in Istore *)
      end
  | (Cdls_get, _) -> Idls_get, args
  | (Calloc, _) -> (Ialloc {bytes = 0; dbginfo = []}), args
  | (Caddi, _) -> select_arith_comm self Iadd args
  | (Csubi, _) -> select_arith self Isub args
  | (Cmuli, _) -> select_arith_comm self Imul args
  | (Cmulhi, _) -> select_arith_comm self Imulh args
  | (Cdivi, _) -> (Iintop Idiv, args)
  | (Cmodi, _) -> (Iintop Imod, args)
  | (Cand, _) -> select_arith_comm self Iand args
  | (Cor, _) -> select_arith_comm self Ior args
  | (Cxor, _) -> select_arith_comm self Ixor args
  | (Clsl, _) -> select_arith self Ilsl args
  | (Clsr, _) -> select_arith self Ilsr args
  | (Casr, _) -> select_arith self Iasr args
  | (Ccmpi comp, _) -> select_arith_comp self (Isigned comp) args
  | (Caddv, _) -> select_arith_comm self Iadd args
  | (Cadda, _) -> select_arith_comm self Iadd args
  | (Ccmpa comp, _) -> select_arith_comp self (Iunsigned comp) args
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)
  | (Cmulf, _) -> (Imulf, args)
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound, _) ->
    select_arith self Icheckbound args
  | _ -> Misc.fatal_error "Selection.select_oper"

(* Instruction selection for conditionals *)

let default_select_condition self = function
  | Cop(Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
    when is_immediate_test self (Isigned cmp) n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int (n, _); arg2], _)
    when is_immediate_test self
        (Isigned (swap_integer_comparison cmp)) n ->
      (Iinttest_imm(Isigned(swap_integer_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args, _) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
    when is_immediate_test self (Iunsigned cmp) n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_int (n, _); arg2], _)
    when is_immediate_test self
        (Iunsigned (swap_integer_comparison cmp)) n ->
      (Iinttest_imm(Iunsigned(swap_integer_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args, _) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args, _) ->
      (Ifloattest cmp, Ctuple args)
  | Cop(Cand, [arg; Cconst_int (1, _)], _) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Return an array of fresh registers of the given type.
   Normally implemented as Reg.createv, but some
   ports (e.g. Arm) can override this definition to store float values
   in pairs of integer registers. *)

let default_regs_for _self tys = Reg.createv tys

let insert_debug self _env desc dbg arg res =
  self.instr_seq <- instr_cons_debug desc arg res dbg self.instr_seq

let insert self _env desc arg res =
  self.instr_seq <- instr_cons desc arg res self.instr_seq

let extract_onto self o =
  let rec extract res i =
    if i == dummy_instr
      then res
      else extract {i with next = res} i.next in
    extract o self.instr_seq

let extract self =
  extract_onto self (end_instr ())

(* Insert a sequence of moves from one pseudoreg set to another. *)

let insert_move self env src dst =
  if src.stamp <> dst.stamp then
    insert self env (Iop Imove) [|src|] [|dst|]

let insert_moves self env src dst =
  for i = 0 to Stdlib.Int.min (Array.length src) (Array.length dst) - 1 do
    insert_move self env src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

let insert_move_args self env arg loc stacksize =
  if stacksize <> 0 then begin
    insert self env (Iop(Istackoffset stacksize)) [||] [||]
  end;
  insert_moves self env arg loc

let insert_move_results self env loc res stacksize =
  if stacksize <> 0 then begin
    insert self env (Iop(Istackoffset(-stacksize))) [||] [||]
  end;
  insert_moves self env loc res

(* Add an Iop opcode. Can be overridden by processor description
   to insert moves before and after the operation, i.e. for two-address
   instructions, or instructions using dedicated registers. *)

let default_insert_op_debug self env op dbg rs rd =
  insert_debug self env (Iop op) dbg rs rd;
  rd

let default_insert_op self env op rs rd =
  insert_op_debug self env op Debuginfo.none rs rd

let bind_let self (env:environment) v r1 =
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    env_add v r1 env
  end else begin
    let rv = Reg.createv_like r1 in
    name_regs v rv;
    insert_moves self env r1 rv;
    env_add v rv env
  end

let bind_let_mut self (env:environment) v k r1 =
  let rv = regs_for self k in
  name_regs v rv;
  insert_moves self env r1 rv;
  env_add ~mut:Mutable v rv env

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join env opt_r1 seq1 opt_r2 seq2 =
  match (opt_r1, opt_r2) with
    (None, _) -> opt_r2
  | (_, None) -> opt_r1
  | (Some r1, Some r2) ->
      let l1 = Array.length r1 in
      assert (l1 = Array.length r2);
      let r = Array.make l1 Reg.dummy in
      for i = 0 to l1-1 do
        if Reg.anonymous r1.(i)
          && Cmm.ge_component r1.(i).typ r2.(i).typ
        then begin
          r.(i) <- r1.(i);
          insert_move seq2 env r2.(i) r1.(i)
        end else if Reg.anonymous r2.(i)
          && Cmm.ge_component r2.(i).typ r1.(i).typ
        then begin
          r.(i) <- r2.(i);
          insert_move seq1 env r1.(i) r2.(i)
        end else begin
          let typ = Cmm.lub_component r1.(i).typ r2.(i).typ in
          r.(i) <- Reg.create typ;
          insert_move seq1 env r1.(i) r.(i);
          insert_move seq2 env r2.(i) r.(i)
        end
      done;
      Some r

(* Same, for N branches *)

let join_array env rs =
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, _) = rs.(i) in
    match r with
    | None -> ()
    | Some r ->
      match !some_res with
      | None -> some_res := Some (r, Array.map (fun r -> r.typ) r)
      | Some (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.typ typ) r types
        in
        some_res := Some (r', types)
  done;
  match !some_res with
    None -> None
  | Some (template, types) ->
      let size_res = Array.length template in
      let res = Array.make size_res Reg.dummy in
      for i = 0 to size_res - 1 do
        res.(i) <- Reg.create types.(i)
      done;
      for i = 0 to Array.length rs - 1 do
        let (r, s) = rs.(i) in
        match r with
          None -> ()
        | Some r -> insert_moves s env r res
      done;
      Some res

(* Add the instructions for the given expression
   at the end of the self sequence *)

let rec emit_expr self (env:environment) exp =
  match exp with
    Cconst_int (n, _dbg) ->
      let r = regs_for self typ_int in
      Some(insert_op self env (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint (n, _dbg) ->
      let r = regs_for self typ_int in
      Some(insert_op self env (Iconst_int n) [||] r)
  | Cconst_float (n, _dbg) ->
      let r = regs_for self typ_float in
      Some(insert_op self env
             (Iconst_float (Int64.bits_of_float n)) [||] r)
  | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of
         which may point to heap values. However, any such blocks will be
         registered in the compilation unit's global roots structure, so
         adding this register to the frame table would be redundant *)
      let r = regs_for self typ_int in
      Some(insert_op self env (Iconst_symbol n) [||] r)
  | Cvar v ->
      begin try
        Some(env_find v env)
      with Not_found ->
        Misc.fatal_error("Selection.emit_expr: unbound var " ^ V.unique_name v)
      end
  | Clet(v, e1, e2) ->
      begin match emit_expr self env e1 with
        None -> None
      | Some r1 -> emit_expr self (bind_let self env v r1) e2
      end
  | Clet_mut(v, k, e1, e2) ->
      begin match emit_expr self env e1 with
        None -> None
      | Some r1 -> emit_expr self (bind_let_mut self env v k r1) e2
      end
  | Cphantom_let (_var, _defining_expr, body) ->
      emit_expr self env body
  | Cassign(v, e1) ->
      let rv =
        try
          env_find_mut v env
        with Not_found ->
          Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.name v) in
      begin match emit_expr self env e1 with
        None -> None
      | Some r1 ->
          insert_moves self env r1 rv; Some [||]
      end
  | Ctuple [] ->
      Some [||]
  | Ctuple exp_list ->
      begin match emit_parts_list self env exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          Some(emit_tuple self ext_env simple_list)
      end
  | Cop(Craise k, [arg], dbg) ->
      begin match emit_expr self env arg with
        None -> None
      | Some r1 ->
          let rd = [|Proc.loc_exn_bucket|] in
          insert self env (Iop Imove) r1 rd;
          insert_debug self env  (Iraise k) dbg rd [||];
          None
      end
  | Cop(Ccmpf _, _, dbg) ->
      emit_expr self env
        (Cifthenelse (exp,
          dbg, Cconst_int (1, dbg),
          dbg, Cconst_int (0, dbg),
          dbg))
  | Cop(Copaque, args, dbg) ->
      begin match emit_parts_list self env args with
        None -> None
      | Some (simple_args, env) ->
         let rs = emit_tuple self env simple_args in
         Some (insert_op_debug self env Iopaque dbg rs rs)
      end
  | Cop(op, args, dbg) ->
      begin match emit_parts_list self env args with
        None -> None
      | Some(simple_args, env) ->
          let ty = oper_result_type op in
          let (new_op, new_args) =
            select_operation self op simple_args dbg
          in
          match new_op with
            Icall_ind ->
              let r1 = emit_tuple self env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = regs_for self ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv rarg) in
              let loc_res = Proc.loc_results (Reg.typv rd) in
              insert_move_args self env rarg loc_arg stack_ofs;
              insert_debug self env (Iop new_op) dbg
                          (Array.append [|r1.(0)|] loc_arg) loc_res;
              insert_move_results self env loc_res rd stack_ofs;
              Some rd
          | Icall_imm _ ->
              let r1 = emit_tuple self env new_args in
              let rd = regs_for self ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv r1) in
              let loc_res = Proc.loc_results (Reg.typv rd) in
              insert_move_args self env r1 loc_arg stack_ofs;
              insert_debug self env (Iop new_op) dbg loc_arg loc_res;
              insert_move_results self env loc_res rd stack_ofs;
              Some rd
          | Iextcall r ->
              let (loc_arg, stack_ofs) =
                emit_extcall_args self env r.ty_args new_args in
              let rd = regs_for self ty in
              let loc_res =
                insert_op_debug self env
                  (Iextcall {r with stack_ofs = stack_ofs}) dbg
                  loc_arg (Proc.loc_external_results (Reg.typv rd)) in
              insert_move_results self env loc_res rd stack_ofs;
              Some rd
          | Ialloc { bytes = _; } ->
              let rd = regs_for self typ_val in
              let bytes = size_expr env (Ctuple new_args) in
              assert (bytes mod Arch.size_addr = 0);
              let alloc_words = bytes / Arch.size_addr in
              let op =
                Ialloc { bytes; dbginfo = [{alloc_words; alloc_dbg = dbg}] }
              in
              insert_debug self env (Iop op) dbg [||] rd;
              emit_stores self env new_args rd;
              Some rd
          | op ->
              let r1 = emit_tuple self env new_args in
              let rd = regs_for self ty in
              Some (insert_op_debug self env op dbg r1 rd)
      end
  | Csequence(e1, e2) ->
      begin match emit_expr self env e1 with
        None -> None
      | Some _ -> emit_expr self env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
      let (cond, earg) = select_condition self econd in
      begin match emit_expr self env earg with
        None -> None
      | Some rarg ->
          let (rif, sif) = emit_sequence self env eif in
          let (relse, selse) = emit_sequence self env eelse in
          let r = join env rif sif relse selse in
          insert self env (Iifthenelse(cond, extract sif, extract selse))
                      rarg [||];
          r
      end
  | Cswitch(esel, index, ecases, _dbg) ->
      begin match emit_expr self env esel with
        None -> None
      | Some rsel ->
          let rscases =
            Array.map (fun (case, _dbg) -> emit_sequence self env case) ecases
          in
          let r = join_array env rscases in
          insert self env (Iswitch(index,
                                   Array.map (fun (_, s) -> extract s) rscases))
                      rsel [||];
          r
      end
  | Ccatch(_, [], e1) ->
      emit_expr self env e1
  | Ccatch(rec_flag, handlers, body) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = regs_for self typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg))
          handlers
      in
      let env =
        (* Since the handlers may be recursive, and called from the body,
           the same environment is used for translating both the handlers and
           the body. *)
        List.fold_left (fun env (nfail, _ids, rs, _e2, _dbg) ->
            env_add_static_exception nfail rs env)
          env handlers
      in
      let (r_body, s_body) = emit_sequence self env body in
      let translate_one_handler (nfail, ids, rs, e2, _dbg) =
        assert(List.length ids = List.length rs);
        let new_env =
          List.fold_left (fun env ((id, _typ), r) -> env_add id r env)
            env (List.combine ids rs)
        in
        let (r, s) = emit_sequence self new_env e2 in
        (nfail, (r, s))
      in
      let l = List.map translate_one_handler handlers in
      let a = Array.of_list ((r_body, s_body) :: List.map snd l) in
      let r = join_array env a in
      let aux (nfail, (_r, s)) = (nfail, extract s) in
      insert self env (Icatch (rec_flag, List.map aux l, extract s_body))
        [||] [||];
      r
  | Cexit (nfail,args) ->
      begin match emit_parts_list self env args with
        None -> None
      | Some (simple_list, ext_env) ->
          let src = emit_tuple self ext_env simple_list in
          let dest_args =
            try env_find_static_exception nfail env
            with Not_found ->
              Misc.fatal_error ("Selection.emit_expr: unbound label "^
                                Stdlib.Int.to_string nfail)
          in
          (* Intermediate registers to handle cases where some
             registers from src are present in dest *)
          let tmp_regs = Reg.createv_like src in
          (* Ccatch registers must not contain out of heap pointers *)
          Array.iter (fun reg -> assert(reg.typ <> Addr)) src;
          insert_moves self env src tmp_regs ;
          insert_moves self env tmp_regs (Array.concat dest_args) ;
          insert self env (Iexit nfail) [||] [||];
          None
      end
  | Ctrywith(e1, v, e2, _dbg) ->
      let (r1, s1) = emit_sequence self env e1 in
      let rv = regs_for self typ_val in
      let (r2, s2) = emit_sequence self (env_add v rv env) e2 in
      let r = join env r1 s1 r2 s2 in
      insert self env
        (Itrywith(extract s1,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                             (extract s2)))
        [||] [||];
      r

and emit_sequence self (env:environment) exp =
  let s = { self with instr_seq = dummy_instr } in
  let r = emit_expr s env exp in
  (r, s)

(* The following two functions, [emit_parts] and [emit_parts_list], force
   right-to-left evaluation order as required by the Flambda [Un_anf] pass
   (and to be consistent with the bytecode compiler). *)

and emit_parts self (env:environment) ~effects_after exp =
  let module EC = Effect_and_coeffect in
  let may_defer_evaluation =
    let ec = effects_of self exp in
    match EC.effect ec with
    | Effect.Arbitrary | Effect.Raise ->
      (* Preserve the ordering of effectful expressions by evaluating them
         early (in the correct order) and assigning their results to
         temporaries.  We can avoid this in just one case: if we know that
         every [exp'] in the original expression list (cf. [emit_parts_list])
         to be evaluated after [exp] cannot possibly affect the result of
         [exp] or depend on the result of [exp], then [exp] may be deferred.
         (Checking purity here is not enough: we need to check copurity too
         to avoid e.g. moving mutable reads earlier than the raising of
         an exception.) *)
      EC.pure_and_copure effects_after
    | Effect.None ->
      match EC.coeffect ec with
      | Coeffect.None ->
        (* Pure expressions may be moved. *)
        true
      | Coeffect.Read_mutable -> begin
        (* Read-mutable expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects
           "worse" (in the sense of the ordering in [Effect.t]) than raising
           an exception. *)
        match EC.effect effects_after with
        | Effect.None | Effect.Raise -> true
        | Effect.Arbitrary -> false
      end
      | Coeffect.Arbitrary -> begin
        (* Arbitrary expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects. *)
        match EC.effect effects_after with
        | Effect.None -> true
        | Effect.Arbitrary | Effect.Raise -> false
      end
  in
  (* Even though some expressions may look like they can be deferred from
     the (co)effect analysis, it may be forbidden to move them. *)
  if may_defer_evaluation && is_simple_expr self exp then
    Some (exp, env)
  else begin
    match emit_expr self env exp with
      None -> None
    | Some r ->
        if Array.length r = 0 then
          Some (Ctuple [], env)
        else begin
          (* The normal case *)
          let id = V.create_local "bind" in
          if all_regs_anonymous r then
            (* r is an anonymous, unshared register; use it directly *)
            Some (Cvar id, env_add (VP.create id) r env)
          else begin
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            insert_moves self env r tmp;
            Some (Cvar id, env_add (VP.create id) tmp env)
          end
        end
  end

and emit_parts_list self (env:environment) exp_list =
  let module EC = Effect_and_coeffect in
  let exp_list_right_to_left, _effect =
    (* Annotate each expression with the (co)effects that happen after it
       when the original expression list is evaluated from right to left.
       The resulting expression list has the rightmost expression first. *)
    List.fold_left (fun (exp_list, effects_after) exp ->
        let exp_effect = effects_of self exp in
        (exp, effects_after)::exp_list, EC.join exp_effect effects_after)
      ([], EC.none)
      exp_list
  in
  List.fold_left (fun results_and_env (exp, effects_after) ->
      match results_and_env with
      | None -> None
      | Some (result, env) ->
          match emit_parts self env exp ~effects_after with
          | None -> None
          | Some (exp_result, env) -> Some (exp_result :: result, env))
    (Some ([], env))
    exp_list_right_to_left

and emit_tuple_not_flattened self env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      match emit_expr self env exp with
        None -> assert false  (* should have been caught in emit_parts *)
      | Some loc_exp -> loc_exp :: loc_rem
  in
  emit_list exp_list

and emit_tuple self env exp_list =
  Array.concat (emit_tuple_not_flattened self env exp_list)

let default_emit_extcall_args self env ty_args args =
  let args = emit_tuple_not_flattened self env args in
  let ty_args =
    if ty_args = [] then List.map (fun _ -> XInt) args else ty_args in
  let locs, stack_ofs = Proc.loc_external_arguments ty_args in
  let ty_args = Array.of_list ty_args in
  if stack_ofs <> 0 then
    insert self env (Iop(Istackoffset stack_ofs)) [||] [||];
  List.iteri
    (fun i arg ->
      insert_move_extcall_arg self env ty_args.(i) arg locs.(i))
    args;
  Array.concat (Array.to_list locs), stack_ofs

let default_insert_move_extcall_arg self env _ty_arg src dst =
  (* The default implementation is one or two ordinary moves.
     (Two in the case of an int64 argument on a 32-bit platform.)
     It can be overridden to use special move instructions,
     for example a "32-bit move" instruction for int32 arguments. *)
  insert_moves self env src dst

let default_emit_stores self env data regs_addr =
  let a =
    ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int)) in
  List.iter
    (fun e ->
      let (op, arg) = select_store self false !a e in
      match emit_expr self env arg with
        None -> assert false
      | Some regs ->
          match op with
            Istore(_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind = if r.typ = Float then Double else Word_val in
                insert self env
                            (Iop(Istore(kind, !a, false)))
                            (Array.append [|r|] regs_addr) [||];
                a := Arch.offset_addressing !a (size_component r.typ)
              done
          | _ ->
              insert self env (Iop op) (Array.append regs regs_addr) [||];
              a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)

let emit_return self (env:environment) exp =
  match emit_expr self env exp with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results (Reg.typv r) in
      insert_moves self env r loc;
      insert self env Ireturn loc [||]

let rec emit_tail self (env:environment) exp =
  match exp with
    Clet(v, e1, e2) ->
      begin match emit_expr self env e1 with
        None -> ()
      | Some r1 -> emit_tail self (bind_let self env v r1) e2
      end
  | Clet_mut (v, k, e1, e2) ->
     begin match emit_expr self env e1 with
       None -> ()
     | Some r1 -> emit_tail self (bind_let_mut self env v k r1) e2
     end
  | Cphantom_let (_var, _defining_expr, body) ->
      emit_tail self env body
  | Cop((Capply ty) as op, args, dbg) ->
      begin match emit_parts_list self env args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) =
            select_operation self op simple_args dbg
          in
          match new_op with
            Icall_ind ->
              let r1 = emit_tuple self env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv rarg) in
              if stack_ofs = 0 then begin
                let call = Iop (Itailcall_ind) in
                insert_moves self env rarg loc_arg;
                insert_debug self env call dbg
                            (Array.append [|r1.(0)|] loc_arg) [||];
              end else begin
                let rd = regs_for self ty in
                let loc_res = Proc.loc_results (Reg.typv rd) in
                insert_move_args self env rarg loc_arg stack_ofs;
                insert_debug self env (Iop new_op) dbg
                            (Array.append [|r1.(0)|] loc_arg) loc_res;
                insert self env (Iop(Istackoffset(-stack_ofs))) [||] [||];
                insert self env Ireturn loc_res [||]
              end
          | Icall_imm { func; } ->
              let r1 = emit_tuple self env new_args in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv r1) in
              if stack_ofs = 0 then begin
                let call = Iop (Itailcall_imm { func; }) in
                insert_moves self env r1 loc_arg;
                insert_debug self env call dbg loc_arg [||];
              end else if func = !current_function_name then begin
                let call = Iop (Itailcall_imm { func; }) in
                let loc_arg' = Proc.loc_parameters (Reg.typv r1) in
                insert_moves self env r1 loc_arg';
                insert_debug self env call dbg loc_arg' [||];
              end else begin
                let rd = regs_for self ty in
                let loc_res = Proc.loc_results (Reg.typv rd) in
                insert_move_args self env r1 loc_arg stack_ofs;
                insert_debug self env (Iop new_op) dbg loc_arg loc_res;
                insert self env (Iop(Istackoffset(-stack_ofs))) [||] [||];
                insert self env Ireturn loc_res [||]
              end
          | _ -> Misc.fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match emit_expr self env e1 with
        None -> ()
      | Some _ -> emit_tail self env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
      let (cond, earg) = select_condition self econd in
      begin match emit_expr self env earg with
        None -> ()
      | Some rarg ->
          insert self env
                      (Iifthenelse(cond, emit_tail_sequence self env eif,
                                         emit_tail_sequence self env eelse))
                      rarg [||]
      end
  | Cswitch(esel, index, ecases, _dbg) ->
      begin match emit_expr self env esel with
        None -> ()
      | Some rsel ->
          let cases =
            Array.map (fun (case, _dbg) -> emit_tail_sequence self env case)
              ecases
          in
          insert self env (Iswitch (index, cases)) rsel [||]
      end
  | Ccatch(_, [], e1) ->
      emit_tail self env e1
  | Ccatch(rec_flag, handlers, e1) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = regs_for self typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg))
          handlers in
      let env =
        List.fold_left (fun env (nfail, _ids, rs, _e2, _dbg) ->
            env_add_static_exception nfail rs env)
          env handlers in
      let s_body = emit_tail_sequence self env e1 in
      let aux (nfail, ids, rs, e2, _dbg) =
        assert(List.length ids = List.length rs);
        let new_env =
          List.fold_left
            (fun env ((id, _typ),r) -> env_add id r env)
            env (List.combine ids rs) in
        nfail, emit_tail_sequence self new_env e2
      in
      insert self env (Icatch(rec_flag, List.map aux handlers, s_body))
        [||] [||]
  | Ctrywith(e1, v, e2, _dbg) ->
      let (opt_r1, s1) = emit_sequence self env e1 in
      let rv = regs_for self typ_val in
      let s2 = emit_tail_sequence self (env_add v rv env) e2 in
      insert self env
        (Itrywith(extract s1,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv s2))
        [||] [||];
      begin match opt_r1 with
        None -> ()
      | Some r1 ->
          let loc = Proc.loc_results (Reg.typv r1) in
          insert_moves self env r1 loc;
          insert self env Ireturn loc [||]
      end
  | Cop _
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cexit _ ->
    emit_return self env exp

and emit_tail_sequence self env exp =
  let s = { self with instr_seq = dummy_instr } in
  emit_tail s env exp;
  extract s

(* Sequentialization of a function definition *)

let emit_fundecl self ~future_funcnames f =
  current_function_name := f.Cmm.fun_name;
  let rargs =
    List.map
      (fun (id, ty) -> let r = regs_for self ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters (Reg.typv rarg) in
  let env =
    List.fold_right2
      (fun (id, _ty) r env -> env_add id r env)
      f.Cmm.fun_args rargs env_empty in
  emit_tail self env f.Cmm.fun_body;
  let body = extract self in
  self.instr_seq <- dummy_instr;
  insert_moves self env loc_arg rarg;
  let polled_body =
    if Polling.requires_prologue_poll ~future_funcnames
         ~fun_name:f.Cmm.fun_name body
      then
        instr_cons_debug
          (Iop(Ipoll { return_label = None })) [||] [||] f.Cmm.fun_dbg body
    else
      body
    in
  let body_with_prologue = extract_onto self polled_body in
  instr_iter (fun instr -> mark_instr self instr.Mach.desc)
    body_with_prologue;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = body_with_prologue;
    fun_codegen_options = f.Cmm.fun_codegen_options;
    fun_dbg  = f.Cmm.fun_dbg;
    fun_poll = f.Cmm.fun_poll;
    fun_num_stack_slots = Array.make Proc.num_register_classes 0;
    fun_contains_calls = !(self.contains_calls);
  }

let reset () =
  current_function_name := ""

let default_selector =
  { is_immediate = default_is_immediate;
    is_immediate_test = (fun _self ->
        Misc.fatal_error "Selectgen: is_immediate must be provided");
    select_addressing = (fun _self ->
        Misc.fatal_error "Selectgen: select_addressing must be provided");
    is_simple_expr = default_is_simple_expr;
    effects_of = default_effects_of;
    select_operation = default_select_operation;
    select_condition = default_select_condition;
    select_store = default_select_store;
    regs_for = default_regs_for;
    insert_op = default_insert_op;
    insert_op_debug = default_insert_op_debug;
    insert_move_extcall_arg = default_insert_move_extcall_arg;
    emit_extcall_args = default_emit_extcall_args;
    emit_stores = default_emit_stores;
    mark_call = default_mark_call;
    mark_tailcall = default_mark_tailcall;
    mark_c_tailcall = default_mark_c_tailcall;
    mark_instr = default_mark_instr;
    contains_calls = ref false;
    instr_seq = dummy_instr;
  }
