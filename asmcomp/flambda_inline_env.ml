open Abstract_identifiers
open Flambdaapprox

type t = {
  env_approx : Flambdaapprox.t Variable.Map.t;
  current_functions : Set_of_closures_id.Set.t;
  (* The functions currently being declared: used to avoid inlining
     recursively *)
  inlining_level : int;
  (* Number of times "inline" has been called recursively *)
  sb : Flambdasubst.t;
  never_inline : bool ;
  possible_unrolls : int;
  closure_depth : int;
}

let empty ~never_inline =
  { env_approx = Variable.Map.empty;
    current_functions = Set_of_closures_id.Set.empty;
    inlining_level = 0;
    sb = Flambdasubst.empty;
    never_inline;
    possible_unrolls = !Clflags.unroll;
    closure_depth = 0}

let local env =
  { env with
    env_approx = Variable.Map.empty;
    sb = Flambdasubst.new_substitution env.sb }

let inlining_level_up env = { env with inlining_level = env.inlining_level + 1 }

let find id env =
  try Variable.Map.find id env.env_approx
  with Not_found ->
    Misc.fatal_error
      (Format.asprintf "unbound variable %a@." Variable.print id)

let present env var = Variable.Map.mem var env.env_approx

let activate_substitution env =
  { env with sb = Flambdasubst.activate env.sb }
let disactivate_substitution env =
  { env with sb = Flambdasubst.empty }

let add_approx id approx env =
  let approx =
    match approx.var with
    | Some var when present env var ->
      approx
    | _ ->
      { approx with var = Some id }
  in
  { env with env_approx = Variable.Map.add id approx env.env_approx }

let clear_approx id env =
  { env with env_approx = Variable.Map.add id value_unknown env.env_approx }

let enter_set_of_closures_declaration ident env =
  { env with
    current_functions =
      Set_of_closures_id.Set.add ident env.current_functions; }

let inside_set_of_closures_declaration closure_id env =
  Set_of_closures_id.Set.mem closure_id env.current_functions

let at_toplevel env =
  env.closure_depth = 0

let set_sb sb env =
  { env with sb; }

let increase_closure_depth env =
  { env with closure_depth = env.closure_depth + 1; }

let set_never_inline env =
  { env with never_inline = true }

let unrolling_allowed env =
  env.possible_unrolls > 0

let inside_unrolled_function env =
  { env with possible_unrolls = env.possible_unrolls - 1 }

let inlining_level t = t.inlining_level
let sb t = t.sb
let never_inline t = t.never_inline
