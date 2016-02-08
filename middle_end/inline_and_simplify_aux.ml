(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Env = struct
  type scope = Current | Outer

  type t = {
    backend : (module Backend_intf.S);
    round : int;
    approx : (scope * Simple_value_approx.t) Variable.Map.t;
    approx_mutable : Simple_value_approx.t Mutable_variable.Map.t;
    approx_sym : Simple_value_approx.t Symbol.Map.t;
    projections : Variable.t Projection.Map.t;
    current_functions : Set_of_closures_origin.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    (* Number of times "inline" has been called recursively *)
    inside_branch : int;
    freshening : Freshening.t;
    never_inline : bool ;
    never_inline_inside_closures : bool;
    never_inline_outside_closures : bool;
    unroll_counts : int Set_of_closures_origin.Map.t;
    closure_depth : int;
    inlining_stats_closure_stack : Inlining_stats.Closure_stack.t;
  }

  let create ~never_inline ~backend ~round =
    { backend;
      round;
      approx = Variable.Map.empty;
      approx_mutable = Mutable_variable.Map.empty;
      approx_sym = Symbol.Map.empty;
      projections = Projection.Map.empty;
      current_functions = Set_of_closures_origin.Set.empty;
      inlining_level = 0;
      inside_branch = 0;
      freshening = Freshening.empty;
      never_inline;
      never_inline_inside_closures = false;
      never_inline_outside_closures = false;
      unroll_counts = Set_of_closures_origin.Map.empty;
      closure_depth = 0;
      inlining_stats_closure_stack =
        Inlining_stats.Closure_stack.create ();
    }

  let backend t = t.backend
  let round t = t.round

  let local env =
    { env with
      approx = Variable.Map.empty;
      projections = Projection.Map.empty;
      freshening = Freshening.empty_preserving_activation_state env.freshening;
    }

  let inlining_level_up env =
    let max_level =
      Clflags.Int_arg_helper.get ~key:(env.round) !Clflags.max_inlining_depth
    in
    if (env.inlining_level + 1) > max_level then
      Misc.fatal_error "Inlining level increased above maximum";
    { env with inlining_level = env.inlining_level + 1 }

  let print ppf t =
    Format.fprintf ppf
      "Environment maps: %a@.Projections: %a@.Freshening: %a@."
      Variable.Set.print (Variable.Map.keys t.approx)
      (Projection.Map.print Variable.print) t.projections
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

  let add_projection t ~projection ~bound_to =
    { t with
      projections =
        Projection.Map.add projection bound_to t.projections;
    }

  let find_projection t ~projection =
    match Projection.Map.find projection t.projections with
    | exception Not_found -> None
    | var -> Some var

  let does_not_bind t vars =
    not (List.exists (mem t) vars)

  let does_not_freshen t vars =
    Freshening.does_not_freshen t.freshening vars

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
      Misc.fatal_errorf "Env.find_with_scope_exn: Unbound variable \
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

  let enter_set_of_closures_declaration origin t =
    { t with
      current_functions =
        Set_of_closures_origin.Set.add origin t.current_functions; }

  let inside_set_of_closures_declaration origin t =
    Set_of_closures_origin.Set.mem origin t.current_functions

  let at_toplevel t =
    t.closure_depth = 0

  let is_inside_branch env = env.inside_branch > 0

  let branch_depth env = env.inside_branch

  let inside_branch t =
    { t with inside_branch = t.inside_branch + 1 }

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
    if t.never_inline then t
    else { t with never_inline = true }

  let set_never_inline_inside_closures t =
    if t.never_inline_inside_closures then t
    else { t with never_inline_inside_closures = true }

  let unset_never_inline_inside_closures t =
    if t.never_inline_inside_closures then
      { t with never_inline_inside_closures = false }
    else t

  let set_never_inline_outside_closures t =
    if t.never_inline_outside_closures then t
    else { t with never_inline_outside_closures = true }

  let unset_never_inline_outside_closures t =
    if t.never_inline_outside_closures then
      { t with never_inline_outside_closures = false }
    else t

  let unrolling_allowed t origin =
    let unroll_count =
      try
        Set_of_closures_origin.Map.find origin t.unroll_counts
      with Not_found ->
        Clflags.Int_arg_helper.get ~key:t.round !Clflags.unroll
    in
    unroll_count > 0

  let inside_unrolled_function t origin =
    let unroll_count =
      try
        Set_of_closures_origin.Map.find origin t.unroll_counts
      with Not_found ->
        Clflags.Int_arg_helper.get ~key:t.round !Clflags.unroll
    in
    let unroll_counts =
      Set_of_closures_origin.Map.add origin (unroll_count - 1) t.unroll_counts
    in
    { t with unroll_counts }

  let inlining_level t = t.inlining_level
  let freshening t = t.freshening
  let never_inline t = t.never_inline || t.never_inline_outside_closures

  let note_entering_closure t ~closure_id ~debuginfo =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_closure
            t.inlining_stats_closure_stack ~closure_id ~debuginfo;
      }

  let note_entering_call t ~closure_id ~debuginfo =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_call
            t.inlining_stats_closure_stack ~closure_id ~debuginfo;
      }

  let note_entering_inlined t =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_inlined
            t.inlining_stats_closure_stack;
      }

  let note_entering_specialised t ~closure_ids =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_specialised
            t.inlining_stats_closure_stack ~closure_ids;
      }

  let enter_closure t ~closure_id ~inline_inside ~debuginfo ~f =
    let t =
      if inline_inside && not t.never_inline_inside_closures then t
      else set_never_inline t
    in
    let t = unset_never_inline_outside_closures t in
    f (note_entering_closure t ~closure_id ~debuginfo)

  let record_decision t decision =
    Inlining_stats.record_decision decision
      ~closure_stack:t.inlining_stats_closure_stack
end

let initial_inlining_threshold ~round : Inlining_cost.Threshold.t =
  let unscaled =
    Clflags.Float_arg_helper.get ~key:round !Clflags.inline_threshold
  in
  (* CR-soon pchambart: Add a warning if this is too big
     mshinwell: later *)
  Can_inline_if_no_larger_than
    (int_of_float
      (unscaled *. float_of_int Inlining_cost.scale_inline_threshold_by))

let initial_inlining_toplevel_threshold ~round : Inlining_cost.Threshold.t =
  let ordinary_threshold =
    Clflags.Float_arg_helper.get ~key:round !Clflags.inline_threshold
  in
  let toplevel_threshold =
    Clflags.Int_arg_helper.get ~key:round !Clflags.inline_toplevel_threshold
  in
  let unscaled =
    (int_of_float ordinary_threshold) + toplevel_threshold
  in
  (* CR-soon pchambart: Add a warning if this is too big
     mshinwell: later *)
  Can_inline_if_no_larger_than
    (unscaled * Inlining_cost.scale_inline_threshold_by)

module Result = struct
  module Int = Numbers.Int

  type t =
    { approx : Simple_value_approx.t;
      used_static_exceptions : Static_exception.Set.t;
      inlining_threshold : Inlining_cost.Threshold.t option;
      benefit : Inlining_cost.Benefit.t;
      num_direct_applications : int;
    }

  let create () =
    { approx = Simple_value_approx.value_unknown Other;
      used_static_exceptions = Static_exception.Set.empty;
      inlining_threshold = None;
      benefit = Inlining_cost.Benefit.zero;
      num_direct_applications = 0;
    }

  let approx t = t.approx
  let set_approx t approx = { t with approx }

  let use_static_exception t i =
    { t with
      used_static_exceptions =
        Static_exception.Set.add i t.used_static_exceptions;
    }

  let used_static_exceptions t = t.used_static_exceptions

  let exit_scope_catch t i =
    { t with
      used_static_exceptions =
        Static_exception.Set.remove i t.used_static_exceptions;
    }

  let map_benefit t f =
    { t with benefit = f t.benefit }

  let add_benefit t b =
    { t with benefit = Inlining_cost.Benefit.(+) t.benefit b }

  let benefit t = t.benefit

  let reset_benefit t =
    { t with benefit = Inlining_cost.Benefit.zero; }

  let set_inlining_threshold t inlining_threshold =
    { t with inlining_threshold }

  let add_inlining_threshold t j =
    match t.inlining_threshold with
    | None -> t
    | Some i ->
      let inlining_threshold = Some (Inlining_cost.Threshold.add i j) in
      { t with inlining_threshold }

  let sub_inlining_threshold t j =
    match t.inlining_threshold with
    | None -> t
    | Some i ->
      let inlining_threshold = Some (Inlining_cost.Threshold.sub i j) in
      { t with inlining_threshold }

  let inlining_threshold t = t.inlining_threshold

  let seen_direct_application t =
    { t with num_direct_applications = t.num_direct_applications + 1; }

  let num_direct_applications t =
    t.num_direct_applications
end
