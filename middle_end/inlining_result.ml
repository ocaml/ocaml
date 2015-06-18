open Abstract_identifiers

module IntMap = Ext_types.Int.Map

type t =
  { approx : Simple_value_approx.t;
    globals : Simple_value_approx.t IntMap.t;
    used_variables : Variable.Set.t;
    used_staticfail : Static_exception.Set.t;
    inlining_threshold : Inlining_cost.inlining_threshold;
    benefit : Inlining_cost.Benefit.t;
  }

let create () =
  { approx = Simple_value_approx.value_unknown;
    globals = IntMap.empty;
    used_variables = Variable.Set.empty;
    used_staticfail = Static_exception.Set.empty;
    inlining_threshold =
      (* CR pchambart: Add a warning if this is too big *)
      Inlining_cost.Can_inline_if_no_larger_than !Clflags.inline_threshold;
    benefit = Inlining_cost.Benefit.zero;
  }

let approx t = t.approx
let set_approx t approx = { t with approx }

let use_var t var =
  { t with used_variables = Variable.Set.add var t.used_variables }

let set_used_variables t used_variables =
  { t with used_variables; }

let used_variables t = t.used_variables

let exit_scope t var =
  { t with used_variables = Variable.Set.remove var t.used_variables }

let use_staticfail t i =
  { t with used_staticfail = Static_exception.Set.add i t.used_staticfail }

let used_staticfail t = t.used_staticfail

let exit_scope_catch t i =
  { t with used_staticfail = Static_exception.Set.remove i t.used_staticfail }

let map_benefit t f =
  { t with benefit = f t.benefit }

let benefit t = t.benefit

let clear_benefit t =
  { t with benefit = Inlining_cost.Benefit.zero; }

let set_inlining_threshold t inlining_threshold =
  { t with inlining_threshold }

let inlining_threshold t = t.inlining_threshold

let add_global t ~field_index ~approx =
  { t with globals = IntMap.add field_index approx t.globals }

let find_global t ~field_index =
  try IntMap.find field_index t.globals with
  | Not_found ->
    Misc.fatal_error (Format.asprintf
        "Inlining_result.find_global: couldn't find global %i@."
          field_index)
