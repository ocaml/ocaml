(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module Env = struct
  type scope = Current | Outer

  type t = {
    backend : (module Backend_intf.S);
    round : int;
    approx : (scope * Simple_value_approx.t) Variable.Map.t;
    approx_mutable : Simple_value_approx.t Mutable_variable.Map.t;
    approx_sym : Simple_value_approx.t Symbol.Map.t;
    current_functions : Set_of_closures_id.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    inside_branch : int;
    inside_simplify : bool;
    (* Number of times "inline" has been called recursively *)
    freshening : Freshening.t;
    never_inline : bool ;
    possible_unrolls : int;
    closure_depth : int;
    inlining_stats_closure_stack : Inlining_stats.Closure_stack.t;
  }

  let create ~never_inline ~backend ~round =
    let possible_unrolls =
      (* CR-someday mshinwell: Share this code. *)
      match !Clflags.unroll with
      | Always possible_unrolls -> possible_unrolls
      | Variable by_round ->
        match Ext_types.Int.Map.find round by_round with
        | possible_unrolls -> possible_unrolls
        | exception Not_found -> Clflags.default_unroll
    in
    { backend;
      round;
      approx = Variable.Map.empty;
      approx_mutable = Mutable_variable.Map.empty;
      approx_sym = Symbol.Map.empty;
      current_functions = Set_of_closures_id.Set.empty;
      inlining_level = 0;
      inside_branch = 0;
      inside_simplify = false;
      freshening = Freshening.empty;
      never_inline;
      possible_unrolls;
      closure_depth = 0;
      inlining_stats_closure_stack =
        Inlining_stats.Closure_stack.create ();
    }

  let backend t = t.backend
  let round t = t.round

  let local env =
    { env with
      approx = Variable.Map.empty;
      freshening = Freshening.empty_preserving_activation_state env.freshening;
    }

  let inlining_level_up env =
    { env with inlining_level = env.inlining_level + 1 }

  let print ppf t =
    Format.fprintf ppf "Environment maps: %a@.Freshening: %a@."
        Variable.Set.print (Variable.Map.keys t.approx)
        Freshening.print t.freshening

  let mem t var = Variable.Map.mem var t.approx

  let add_internal t var (approx : Simple_value_approx.t) ~scope =
    let approx =
      (* The semantics of this [match] are what preserve the property
         described at the top of simple_value_approx.mli, namely that when a
         [var] is mem on an approximation (amongst many possible [var]s),
         it is the one with the outermost scope. *)
      match approx.var with
      | Some var when mem t var -> approx
      | _ -> Simple_value_approx.augment_with_variable approx var
    in
    { t with approx = Variable.Map.add var (scope, approx) t.approx }

  let add t var approx = add_internal t var approx ~scope:Current
  let add_outer_scope t var approx = add_internal t var approx ~scope:Outer

  let add_mutable t mut_var approx =
    { t with approx_mutable =
        Mutable_variable.Map.add mut_var approx t.approx_mutable;
    }

  let really_import_approx t approx =
    let module Backend = (val (t.backend) : Backend_intf.S) in
    Backend.really_import_approx approx

  let really_import_approx_with_scope t (scope, approx) =
    scope, really_import_approx t approx

  let find_symbol_exn t symbol =
    really_import_approx t
      (Symbol.Map.find symbol t.approx_sym)

  let find_symbol_opt t symbol =
    try Some (really_import_approx t
                (Symbol.Map.find symbol t.approx_sym))
    with Not_found -> None

  let find_symbol_fatal t symbol =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      Misc.fatal_errorf "Symbol %a is unbound.  Maybe there is a missing \
          [Let_symbol], [Import_symbol] or similar?"
        Symbol.print symbol
    | approx -> approx

  let find_or_load_symbol t symbol =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      if Compilation_unit.equal
          (Compilation_unit.get_current_exn ())
          (Symbol.compilation_unit symbol)
      then
        Misc.fatal_errorf "Symbol %a from the current compilation unit is unbound. \
                           Maybe there is a missing [Let_symbol] or similar?"
          Symbol.print symbol;
      let module Backend = (val (t.backend) : Backend_intf.S) in
      Backend.import_symbol symbol
    | approx -> approx

  let add_symbol t symbol approx =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      { t with
        approx_sym = Symbol.Map.add symbol approx t.approx_sym;
      }
    | _ ->
      Misc.fatal_errorf "Attempt to redefine symbol %a (to %a) in environment \
          for [Inline_and_simplify]"
        Symbol.print symbol
        Simple_value_approx.print approx

  let redefine_symbol t symbol approx =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      assert false
    | _ ->
      { t with
        approx_sym = Symbol.Map.add symbol approx t.approx_sym;
      }

  let find_with_scope_exn t id =
    try
      really_import_approx_with_scope t
        (Variable.Map.find id t.approx)
    with Not_found ->
      Misc.fatal_errorf "Inlining_env.find_with_scope_exn: Unbound variable \
          %a@.%s@. Environment: %a@."
        Variable.print id
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
        print t

  let find_exn t id =
    snd (find_with_scope_exn t id)

  let find_mutable_exn t mut_var =
    try Mutable_variable.Map.find mut_var t.approx_mutable
    with Not_found ->
      Misc.fatal_errorf "Env.find_mutable_exn: Unbound variable \
          %a@.%s@. Environment: %a@."
        Mutable_variable.print mut_var
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
        print t

  let find_list_exn t vars =
    List.map (fun var -> find_exn t var) vars

  let find_opt t id =
    try Some (really_import_approx t
                (snd (Variable.Map.find id t.approx)))
    with Not_found -> None

  let activate_freshening t =
    { t with freshening = Freshening.activate t.freshening }

  let enter_set_of_closures_declaration ident t =
    { t with
      current_functions =
        Set_of_closures_id.Set.add ident t.current_functions; }

  let inside_set_of_closures_declaration closure_id t =
    Set_of_closures_id.Set.mem closure_id t.current_functions

  let at_toplevel t =
    t.closure_depth = 0

  let is_inside_branch env = env.inside_branch > 0

  let branch_depth env = env.inside_branch

  let inside_branch t =
    { t with inside_branch = t.inside_branch + 1 }

  let inside_simplify t =
    { t with inside_simplify = true }

  let set_freshening t freshening  =
    { t with freshening; }

  let increase_closure_depth t =
    let approx =
      Variable.Map.map (fun (_scope, approx) -> Outer, approx) t.approx
    in
    { t with
      approx;
      closure_depth = t.closure_depth + 1;
    }

  let set_never_inline t =
    { t with never_inline = true }

  let unrolling_allowed t =
    t.possible_unrolls > 0

  let inside_unrolled_function t =
    { t with possible_unrolls = t.possible_unrolls - 1 }

  let inlining_level t = t.inlining_level
  let freshening t = t.freshening
  let never_inline t = t.never_inline

  (* CR-soon mshinwell: this is a bit contorted (see use in
     inlining_decision.ml) *)
  let note_entering_closure t ~closure_id ~where =
    { t with
      inlining_stats_closure_stack =
        Inlining_stats.Closure_stack.note_entering_closure
          t.inlining_stats_closure_stack ~closure_id ~where;
    }

  let enter_closure t ~closure_id ~inline_inside ~where ~f =
    let t =
      if inline_inside then t
      else set_never_inline t
    in
    f (note_entering_closure t ~closure_id ~where)

  let inlining_stats_closure_stack t = t.inlining_stats_closure_stack
end

module Result = struct
  module Int = Ext_types.Int

  type t =
    { approx : Simple_value_approx.t;
      used_staticfail : Static_exception.Set.t;
      inlining_threshold : Inlining_cost.inlining_threshold;
      benefit : Inlining_cost.Benefit.t;
    }

  let create ~round =
    let inlining_threshold : Inlining_cost.inlining_threshold =
      let unscaled =
        match !Clflags.inline_threshold with
        | Always threshold -> threshold
        | Variable by_round ->
          match Int.Map.find round by_round with
          | threshold -> threshold
          | exception Not_found -> Clflags.default_inline_threshold
      in
      (* CR-soon pchambart: Add a warning if this is too big
         mshinwell: later *)
      Can_inline_if_no_larger_than
        (unscaled * Inlining_cost.scale_inline_threshold_by)
    in
    { approx = Simple_value_approx.value_unknown Other;
      used_staticfail = Static_exception.Set.empty;
      inlining_threshold;
      benefit = Inlining_cost.Benefit.zero;
    }

  let approx t = t.approx
  let set_approx t approx = { t with approx }

  let use_staticfail t i =
    { t with used_staticfail = Static_exception.Set.add i t.used_staticfail }

  let used_staticfail t = t.used_staticfail

  let exit_scope_catch t i =
    { t with
      used_staticfail = Static_exception.Set.remove i t.used_staticfail;
    }

  let map_benefit t f =
    { t with benefit = f t.benefit }

  let benefit t = t.benefit

  let reset_benefit t =
    { t with benefit = Inlining_cost.Benefit.zero; }

  let set_inlining_threshold t inlining_threshold =
    { t with inlining_threshold }

  let inlining_threshold t = t.inlining_threshold
end
