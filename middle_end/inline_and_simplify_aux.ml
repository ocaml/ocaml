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
  type t = {
    backend : (module Backend_intf.S);
    approx : Simple_value_approx.t Variable.Map.t;
    current_functions : Set_of_closures_id.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    inside_branch : bool;
    inside_simplify : bool;
    (* Number of times "inline" has been called recursively *)
    freshening : Freshening.t;
    never_inline : bool ;
    possible_unrolls : int;
    closure_depth : int;
    inlining_stats_closure_stack : Inlining_stats.Closure_stack.t;
  }

  let create ~never_inline ~backend =
    { backend;
      approx = Variable.Map.empty;
      current_functions = Set_of_closures_id.Set.empty;
      inlining_level = 0;
      inside_branch = false;
      inside_simplify = false;
      freshening = Freshening.empty;
      never_inline;
      possible_unrolls = !Clflags.unroll;
      closure_depth = 0;
      inlining_stats_closure_stack =
        Inlining_stats.Closure_stack.create ();
    }

  let backend t = t.backend

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

  let add t var (approx : Simple_value_approx.t) =
    let approx =
      (* The semantics of this [match] are what preserve the property
         described at the top of simple_value_approx.mli, namely that when a
         [var] is mem on an approximation (amongst many possible [var]s),
         it is the one with the outermost scope. *)
      match approx.var with
      | Some var when mem t var -> approx
      | _ -> Simple_value_approx.augment_with_variable approx var
    in
    { t with approx = Variable.Map.add var approx t.approx }

  let find_exn t id =
    try Variable.Map.find id t.approx
    with Not_found ->
      Misc.fatal_errorf "Inlining_env.find: Unbound variable %a@.%s@.\
          Environment: %a@."
        Variable.print id
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
        print t

  let find_list_exn t vars =
    List.map (fun var -> find_exn t var) vars

  let find_opt t id =
    try Some (Variable.Map.find id t.approx)
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

  let is_inside_branch env = env.inside_branch

  let inside_branch t =
    { t with inside_branch = true }

  let inside_simplify t =
    { t with inside_simplify = true }

  let set_freshening t freshening  =
    { t with freshening; }

  let increase_closure_depth t =
    { t with closure_depth = t.closure_depth + 1; }

  let set_never_inline t =
    { t with never_inline = true }

  let unrolling_allowed t =
    t.possible_unrolls > 0

  let inside_unrolled_function t =
    { t with possible_unrolls = t.possible_unrolls - 1 }

  let inlining_level t = t.inlining_level
  let freshening t = t.freshening
  let never_inline t = t.never_inline

  (* CR mshinwell: this is a bit contorted (see use in inlining_decision.ml) *)
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
      globals : Simple_value_approx.t Int.Map.t;
      used_staticfail : Static_exception.Set.t;
      inlining_threshold : Inlining_cost.inlining_threshold;
      benefit : Inlining_cost.Benefit.t;
    }

  let create () =
    { approx = Simple_value_approx.value_unknown;
      globals = Int.Map.empty;
      used_staticfail = Static_exception.Set.empty;
      inlining_threshold =
        (* CR pchambart: Add a warning if this is too big *)
        Inlining_cost.Can_inline_if_no_larger_than !Clflags.inline_threshold;
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

  let clear_benefit t =
    { t with benefit = Inlining_cost.Benefit.zero; }

  let set_inlining_threshold t inlining_threshold =
    { t with inlining_threshold }

  let inlining_threshold t = t.inlining_threshold

  let add_global t ~field_index ~approx =
    { t with globals = Int.Map.add field_index approx t.globals }

  let find_global t ~field_index =
    try Int.Map.find field_index t.globals with
    | Not_found ->
      Misc.fatal_error (Format.asprintf
          "Inlining_result.find_global: couldn't find global %i@."
            field_index)
end
