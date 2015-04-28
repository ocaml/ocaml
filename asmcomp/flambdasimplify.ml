(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Abstract_identifiers

module A = Flambdaapprox
module C = Flambdacost

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external swapnative : nativeint -> nativeint = "%bswap_native"

(* CR mshinwell for pchambart: add comment. *)
let lift_lets tree =
  let rec aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fsequence(Flet(str, v, def, body, d1), seq, dseq) ->
        Flet(str, v, def, Fsequence( aux body, seq, dseq), d1)
    | Flet(str1, v1, Flet(str2, v2, def2, body2, d2), body1, d1) ->
        Flet(str2, v2, def2, aux (Flet(str1, v1, body2, body1, d1)), d2)
    | e -> e
  in
  Flambdaiter.map aux tree

let lift_set_of_closures tree =
  let aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fclosure({ fu_closure = Fset_of_closures(set, dset) } as closure, d) ->
        (* let decl = Flambdautils.find_declaration closure.fu_fun set.cl_fun in *)
        (* if not decl.stub then *)
        (*   expr *)
        (* else *)
          (* If the function is a stub, we create an intermediate let to allow
             eliminating it *)
          let set_of_closures_var =
            Variable.create
              ~current_compilation_unit:(Compilenv.current_unit ())
              "set_of_closures"
          in
          Flet(Not_assigned, set_of_closures_var,
               Fset_of_closures(set, dset),
               Fclosure({ closure with
                          fu_closure = Fvar (set_of_closures_var, Expr_id.create ()) },
                        d),
               Expr_id.create ())
    | e -> e
  in
  Flambdaiter.map aux tree

(** A variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables tree =
  let used_variable_within_closure,
      used_closure_id =
    let used = ref Var_within_closure.Set.empty in
    let used_fun = ref Closure_id.Set.empty in
    let aux (expr : _ Flambda.t) =
      match expr with
      | Fvariable_in_closure({ vc_var; vc_fun }, _) ->
          used := Var_within_closure.Set.add vc_var !used;
          used_fun := Closure_id.Set.add vc_fun !used_fun;
      | Fclosure({ fu_fun; fu_relative_to }, _) ->
          used_fun := Closure_id.Set.add fu_fun !used_fun;
          begin match fu_relative_to with
          | None -> ()
          | Some fu_relative_to ->
              used_fun := Closure_id.Set.add fu_relative_to !used_fun
          end
      | e -> ()
    in
    Flambdaiter.iter aux tree;
    !used, !used_fun
  in
  let aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fset_of_closures ({ cl_fun; cl_free_var } as closure, eid) ->
       let all_free_var =
         Variable.Map.fold
           (fun _ { Flambda. free_variables } acc ->
             Variable.Set.union free_variables acc)
           cl_fun.funs
           Variable.Set.empty in
       let cl_free_var =
         Variable.Map.filter (fun id expr ->
             Variable.Set.mem id all_free_var
             || Var_within_closure.Set.mem (Var_within_closure.wrap id)
               used_variable_within_closure
             || not (Flambdaeffects.no_effects expr))
           cl_free_var in
       let cl_fun =
         { cl_fun with
           funs = Variable.Map.filter (fun fun_id _ ->
               Variable.Set.mem fun_id all_free_var
               || Closure_id.Set.mem (Closure_id.wrap fun_id)
                 used_closure_id)
               cl_fun.funs } in
       Fset_of_closures ({ closure with cl_free_var; cl_fun }, eid)
    | e -> e
  in
  Flambdaiter.map aux tree

(* CR mshinwell: rename [eid] and/or [annot] to be consistent *)
let const_int_expr expr n eid =
  if Flambdaeffects.no_effects expr
  then A.make_const_int n eid
  else expr, A.value_int n
let const_char_expr expr c eid =
  if Flambdaeffects.no_effects expr
  then A.make_const_int (Char.code c) eid
  else expr, A.value_int (Char.code c)
let const_ptr_expr expr n eid =
  if Flambdaeffects.no_effects expr
  then A.make_const_ptr n eid
  else expr, A.value_constptr n
let const_bool_expr expr b eid =
  const_ptr_expr expr (if b then 1 else 0) eid
let const_float_expr expr f eid =
  if Flambdaeffects.no_effects expr
  then A.make_const_float f eid
  else expr, A.value_float f
let const_boxed_int_expr expr t i eid =
  if Flambdaeffects.no_effects expr
  then A.make_const_boxed_int t i eid
  else expr, A.value_boxed_int t i

let const_comparison_expr expr cmp x y eid =
  let open Lambda in
  const_bool_expr expr
    (match cmp with
     | Ceq -> x = y
     | Cneq -> x <> y
     | Clt -> x < y
     | Cgt -> x > y
     | Cle -> x <= y
     | Cge -> x >= y)
    eid

module Simplify_sequential_logical_operator (G : sig
  val canonical_absorbing_element : int
  val is_absorbing_element : int -> bool
  val primitive : Lambda.primitive
end) = struct
  (* Simplify a sequential ("short-circuiting") operator using knowledge from
     (a) value approximations; and (b) side effect analysis. *)
  let sequential_op ~arg1 ~(arg1_approx : A.t) ~arg2 ~(arg2_approx : A.t)
        ~dbg ~annot =
    let arg1_no_effects = Flambdaeffects.no_effects arg1 in
    let arg2_no_effects = Flambdaeffects.no_effects arg2 in
    let arg2_annot = Flambdautils.data_at_toplevel_node arg2 in
    let completely_eliminated () : _ Flambda.t * A.t * C.benefit =
      Fconst (Fconst_pointer G.canonical_absorbing_element, annot),
        A.value_constptr G.canonical_absorbing_element,
        C.remove_branch (C.remove_code arg1 (
          C.remove_code arg2 C.no_benefit))
    in
    match arg1_approx.descr with
    | (Value_int n | Value_constptr n) when G.is_absorbing_element n ->
      if arg1_no_effects then
        completely_eliminated ()
      else
        arg1, arg1_approx, C.remove_branch (C.remove_code arg2 C.no_benefit)
    | (Value_int n | Value_constptr n) -> (* when not the absorbing element *)
      if arg1_no_effects then
        arg2, arg2_approx, C.remove_branch (C.remove_code arg1 C.no_benefit)
      else
        begin match arg2_approx.descr with
        | (Value_int arg2_val | Value_constptr arg2_val)
            when arg2_no_effects ->
          Fsequence (arg1, Fconst (Fconst_pointer arg2_val, arg2_annot),
              annot), arg2_approx,
            C.remove_branch (C.remove_code arg2 C.no_benefit)
        | _ ->
          Fsequence (arg1, arg2, annot), arg2_approx,
            C.remove_branch C.no_benefit
        end
    | _ ->
      match arg2_approx.descr with
      | (Value_int n | Value_constptr n)
          when G.is_absorbing_element n ->
        begin match arg1_no_effects, arg2_no_effects with
        | true, true -> completely_eliminated ()
        | true, false (* we must run [arg1]: it might short-circuit [arg2] *)
        | false, false ->
          Fprim (G.primitive, [arg1; arg2], dbg, annot),
            A.value_constptr G.canonical_absorbing_element,
              C.no_benefit
        | false, true ->
          Fsequence (arg1,
              Fconst (Fconst_pointer G.canonical_absorbing_element,
                arg2_annot), annot),
            A.value_constptr G.canonical_absorbing_element,
              C.remove_branch (C.remove_code arg2 C.no_benefit)
        end
      | _ ->
        Fprim (G.primitive, [arg1; arg2], dbg, annot),
          A.value_unknown, C.no_benefit
end

module Simplify_and = Simplify_sequential_logical_operator (struct
  let canonical_absorbing_element = 0
  let is_absorbing_element n = (n = 0)
  let primitive = Lambda.Psequand
end)
let sequential_and = Simplify_and.sequential_op

module Simplify_or = Simplify_sequential_logical_operator (struct
  let canonical_absorbing_element = 1
  let is_absorbing_element n = (n <> 0)
  let primitive = Lambda.Psequor
end)
let sequential_or = Simplify_or.sequential_op

(* Simplification of operations on boxed integers (nativeint, Int32, Int64). *)
module Simplify_boxed_integer_operator (I : sig
  type t
  val kind : Lambda.boxed_integer
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val to_int : t -> int
  val to_int32 : t -> Int32.t
  val to_int64 : t -> Int64.t
  val neg : t -> t
  val swap : t -> t
  val compare : t -> t -> int
end) = struct
  let simplify_unop (p : Lambda.primitive) (kind : I.t A.boxed_int)
        expr (n : I.t) eid =
    let eval op = const_boxed_int_expr expr kind (op n) eid in
    let eval_conv kind op = const_boxed_int_expr expr kind (op n) eid in
    let eval_unboxed op = const_int_expr expr (op n) eid in
    match p with
    | Pintofbint kind when kind = I.kind -> eval_unboxed I.to_int
    | Pcvtbint (kind, Pint32) when kind = I.kind -> eval_conv A.Int32 I.to_int32
    | Pcvtbint (kind, Pint64) when kind = I.kind -> eval_conv A.Int64 I.to_int64
    | Pnegbint kind when kind = I.kind -> eval I.neg
    | Pbbswap kind when kind = I.kind -> eval I.swap
    | _ -> expr, A.value_unknown

  let simplify_binop (p : Lambda.primitive) (kind : I.t A.boxed_int)
        expr (n1 : I.t) (n2 : I.t) eid =
    let eval op = const_boxed_int_expr expr kind (op n1 n2) eid in
    let non_zero n = (I.compare I.zero n) <> 0 in
    match p with
    | Paddbint kind when kind = I.kind -> eval I.add
    | Psubbint kind when kind = I.kind -> eval I.sub
    | Pmulbint kind when kind = I.kind -> eval I.mul
    | Pdivbint kind when kind = I.kind && non_zero n2 -> eval I.div
    | Pmodbint kind when kind = I.kind && non_zero n2 -> eval I.rem
    | Pandbint kind when kind = I.kind -> eval I.logand
    | Porbint kind when kind = I.kind -> eval I.logor
    | Pxorbint kind when kind = I.kind -> eval I.logxor
    | Pbintcomp (kind, c) when kind = I.kind ->
      const_comparison_expr expr c n1 n2 eid
    | _ -> expr, A.value_unknown

  let simplify_binop_int (p : Lambda.primitive) (kind : I.t A.boxed_int)
        expr (n1 : I.t) (n2 : int) eid =
    let eval op = const_boxed_int_expr expr kind (op n1 n2) eid in
    let precond = 0 <= n2 && n2 < 8 * Arch.size_int in
    match p with
    | Plslbint kind when kind = I.kind && precond -> eval I.shift_left
    | Plsrbint kind when kind = I.kind && precond -> eval I.shift_right_logical
    | Pasrbint kind when kind = I.kind && precond -> eval I.shift_right
    | _ -> expr, A.value_unknown
end

module Simplify_boxed_nativeint = Simplify_boxed_integer_operator (struct
  include Nativeint
  let to_int64 = Int64.of_nativeint
  let swap = swapnative
  let kind = Lambda.Pnativeint
end)

module Simplify_boxed_int32 = Simplify_boxed_integer_operator (struct
  include Int32
  let to_int32 i = i
  let to_int64 = Int64.of_int32
  let swap = swap32
  let kind = Lambda.Pint32
end)

module Simplify_boxed_int64 = Simplify_boxed_integer_operator (struct
  include Int64
  let to_int64 i = i
  let swap = swap64
  let kind = Lambda.Pint64
end)

let primitive (p : Lambda.primitive) (args, approxs) expr : _ Flambda.t * A.t =
  let fpc = !Clflags.float_const_prop in
  match p with
  | Pmakeblock(tag, Asttypes.Immutable) ->
    expr, A.value_block(tag, Array.of_list approxs)
  | _ ->
    let eid = Flambdautils.data_at_toplevel_node expr in
    match A.descrs approxs with
    | [Value_int x] ->
      begin match p with
      | Pidentity -> const_int_expr expr x eid
      | Pnot -> const_bool_expr expr (x = 0) eid
      | Pnegint -> const_int_expr expr (-x) eid
      | Pbswap16 -> const_int_expr expr (swap16 x) eid
      | Poffsetint y -> const_int_expr expr (x + y) eid
      | Pfloatofint when fpc -> const_float_expr expr (float_of_int x) eid
      | Pbintofint Pnativeint ->
        const_boxed_int_expr expr Nativeint (Nativeint.of_int x) eid
      | Pbintofint Pint32 ->
        const_boxed_int_expr expr Int32 (Int32.of_int x) eid
      | Pbintofint Pint64 ->
        const_boxed_int_expr expr Int64 (Int64.of_int x) eid
      | _ -> expr, A.value_unknown
      end
    | [(Value_int x | Value_constptr x); (Value_int y | Value_constptr y)] ->
      let shift_precond = 0 <= y && y < 8 * Arch.size_int in
      begin match p with
      | Paddint -> const_int_expr expr (x + y) eid
      | Psubint -> const_int_expr expr (x - y) eid
      | Pmulint -> const_int_expr expr (x * y) eid
      | Pdivint when y <> 0 -> const_int_expr expr (x / y) eid
      | Pmodint when y <> 0 -> const_int_expr expr (x mod y) eid
      | Pandint -> const_int_expr expr (x land y) eid
      | Porint -> const_int_expr expr (x lor y) eid
      | Pxorint -> const_int_expr expr (x lxor y) eid
      | Plslint when shift_precond -> const_int_expr expr (x lsl y) eid
      | Plsrint when shift_precond -> const_int_expr expr (x lsr y) eid
      | Pasrint when shift_precond -> const_int_expr expr (x asr y) eid
      | Pintcomp cmp -> const_comparison_expr expr cmp x y eid
      | Pisout -> const_bool_expr expr (y > x || y < 0) eid
      (* [Psequand] and [Psequor] have special simplification rules, above. *)
      | _ -> expr, A.value_unknown
      end
    | [Value_constptr x] ->
      begin match p with
      | Pidentity -> const_ptr_expr expr x eid
      | Pnot -> const_bool_expr expr (x = 0) eid
      | Pisint -> const_bool_expr expr true eid
      | Poffsetint y -> const_ptr_expr expr (x + y) eid
      | Pctconst c ->
        begin match c with
        | Big_endian -> const_bool_expr expr Arch.big_endian eid
        | Word_size -> const_int_expr expr (8*Arch.size_int) eid
        | Int_size -> const_int_expr expr (8*Arch.size_int - 1) eid
        | Max_wosize ->
          (* CR mshinwell: this function should maybe not live here. *)
          const_int_expr expr ((1 lsl ((8*Arch.size_int) - 10)) - 1) eid
        | Ostype_unix -> const_bool_expr expr (Sys.os_type = "Unix") eid
        | Ostype_win32 -> const_bool_expr expr (Sys.os_type = "Win32") eid
        | Ostype_cygwin -> const_bool_expr expr (Sys.os_type = "Cygwin") eid
        end
      | _ -> expr, A.value_unknown
      end
    | [Value_float x] when fpc ->
      begin match p with
      | Pintoffloat -> const_int_expr expr (int_of_float x) eid
      | Pnegfloat -> const_float_expr expr (-. x) eid
      | Pabsfloat -> const_float_expr expr (abs_float x) eid
      | _ -> expr, A.value_unknown
      end
    | [Value_float n1; Value_float n2] when fpc ->
      begin match p with
      | Paddfloat -> const_float_expr expr (n1 +. n2) eid
      | Psubfloat -> const_float_expr expr (n1 -. n2) eid
      | Pmulfloat -> const_float_expr expr (n1 *. n2) eid
      | Pdivfloat -> const_float_expr expr (n1 /. n2) eid
      | Pfloatcomp c  -> const_comparison_expr expr c n1 n2 eid
      | _ -> expr, A.value_unknown
      end
    | [A.Value_boxed_int(A.Nativeint, n)] ->
      Simplify_boxed_nativeint.simplify_unop p Nativeint expr n eid
    | [A.Value_boxed_int(A.Int32, n)] ->
      Simplify_boxed_int32.simplify_unop p Int32 expr n eid
    | [A.Value_boxed_int(A.Int64, n)] ->
      Simplify_boxed_int64.simplify_unop p Int64 expr n eid
    | [A.Value_boxed_int(A.Nativeint, n1);
       A.Value_boxed_int(A.Nativeint, n2)] ->
      Simplify_boxed_nativeint.simplify_binop p Nativeint expr n1 n2 eid
    | [A.Value_boxed_int(A.Int32, n1); A.Value_boxed_int(A.Int32, n2)] ->
      Simplify_boxed_int32.simplify_binop p Int32 expr n1 n2 eid
    | [A.Value_boxed_int(A.Int64, n1); A.Value_boxed_int(A.Int64, n2)] ->
      Simplify_boxed_int64.simplify_binop p Int64 expr n1 n2 eid
    | [A.Value_boxed_int(A.Nativeint, n1); Value_int n2] ->
      Simplify_boxed_nativeint.simplify_binop_int p Nativeint expr n1 n2 eid
    | [A.Value_boxed_int(A.Int32, n1); Value_int n2] ->
      Simplify_boxed_int32.simplify_binop_int p Int32 expr n1 n2 eid
    | [A.Value_boxed_int(A.Int64, n1); Value_int n2] ->
      Simplify_boxed_int64.simplify_binop_int p Int64 expr n1 n2 eid
    | [Value_block _] when p = Pisint -> const_bool_expr expr false eid
    | [Value_string { size }] when p = Pstringlength ->
        Printf.printf "\nstring length\n\n%!";
        const_int_expr expr size eid
    | [Value_string { size; contents = Some s };
       (Value_int x | Value_constptr x)] when x >= 0 && x < size ->
        begin match p with
        | Pstringrefu
        | Pstringrefs ->
            Printf.printf "\nstring get\n\n%!";
            const_char_expr expr s.[x] eid
        | _ -> expr, A.value_unknown
        end
    | _ -> expr, A.value_unknown

let rename_var var =
  Variable.rename ~current_compilation_unit:(Compilenv.current_unit ()) var
let nid () = Expr_id.create ()

let remove_params unused (fun_decl: _ Flambda.function_declaration) =
  let unused_params, used_params = List.partition (fun v -> Variable.Set.mem v unused) fun_decl.params in
  let unused_params = List.filter (fun v ->
      Variable.Set.mem v fun_decl.free_variables) unused_params in
  let body = List.fold_left (fun body var ->
      Flambda.Flet(Not_assigned,
           var,
           Fconst(Fconst_pointer 0, nid ()),
           body,
           nid ()))
      fun_decl.body
      unused_params
  in
  let free_variables =
    Variable.Set.filter (fun var -> not (Variable.Set.mem var unused))
      fun_decl.free_variables
  in
  { fun_decl with
    params = used_params;
    free_variables;
    body }

let make_stub unused var (fun_decl : _ Flambda.function_declaration) =
  let renamed = rename_var var in
  let args = List.map (fun var -> var, rename_var var) fun_decl.params in
  let ap_arg =
    List.map (fun (_, var) -> Flambda.Fvar(var, nid ()))
      (List.filter (fun (var, _) -> not (Variable.Set.mem var unused)) args)
  in
  let ap_kind = Flambda.Direct (Closure_id.wrap renamed) in
  let ap_dbg = fun_decl.dbg in
  let body : _ Flambda.flambda =
    Fapply(
      { ap_function = Fvar(renamed, nid ());
        ap_arg;
        ap_kind;
        ap_dbg },
      nid ())
  in
  let free_variables =
    List.fold_left
      (fun set (_, renamed_arg) -> Variable.Set.add renamed_arg set)
      (Variable.Set.singleton renamed)
      args
  in
  let decl : _ Flambda.function_declaration = {
    params = List.map snd args;
    body;
    free_variables;
    stub = true;
    dbg = fun_decl.dbg;
  }
  in
  decl, renamed

let separate_unused_arguments (set_of_closures : _ Flambda.fset_of_closures) =
  let decl = set_of_closures.cl_fun in
  let unused = Flambdautils.unused_arguments decl in
  let non_stub_arguments =
    Variable.Map.fold (fun _ (decl : _ Flambda.function_declaration) acc ->
        if decl.stub then
          acc
        else
          Variable.Set.union acc (Variable.Set.of_list decl.Flambda.params))
      decl.funs Variable.Set.empty
  in
  let unused = Variable.Set.inter non_stub_arguments unused in
  if Variable.Set.is_empty unused
  then None
  else begin
    let funs =
      Variable.Map.fold (fun fun_id (fun_decl : _ Flambda.function_declaration) acc ->
          if List.exists (fun v -> Variable.Set.mem v unused) fun_decl.params
          then begin
            let stub, renamed_fun_id = make_stub unused fun_id fun_decl in
            let cleaned = remove_params unused fun_decl in
            Variable.Map.add fun_id stub
              (Variable.Map.add renamed_fun_id cleaned acc)
          end
          else
            Variable.Map.add fun_id fun_decl acc
        )
        decl.funs Variable.Map.empty
    in
    let cl_specialised_arg =
      Variable.Map.filter (fun param _ -> not (Variable.Set.mem param unused))
        set_of_closures.cl_specialised_arg
    in
    Some
      { set_of_closures with
        cl_fun = { decl with funs };
        cl_specialised_arg; }
  end

let separate_unused_arguments_in_closures tree =
  let aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fset_of_closures (set_of_closures, eid) -> begin
        match separate_unused_arguments set_of_closures with
        | None -> expr
        | Some set_of_closures ->
            Fset_of_closures (set_of_closures, eid)
      end
    | e -> e
  in
  Flambdaiter.map aux tree

let used_globals id tree =
  let used = ref Ext_types.Int.Set.empty in
  Flambdaiter.iter (function
      | Fprim(Pgetglobalfield(modul, pos), _, _, _) when Ident.same id modul ->
          used := Ext_types.Int.Set.add pos !used
      | _ -> ()) tree;
  !used

let remove_unused_globals tree =
  let id = Compilenv.current_unit_id () in
  let used = used_globals id tree in
  Flambdaiter.map (function
      | Fprim(Psetglobalfield(Not_exported, pos), arg, dbg, attr)
        when not (Ext_types.Int.Set.mem pos used) ->
          Fprim(Pignore, arg, dbg, attr)
      | e -> e)
    tree
