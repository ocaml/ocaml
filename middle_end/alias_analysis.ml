
module Eq_id = Ext_types.Id(struct end)
let fresh () = Eq_id.create ()

type equation_left =
  | Eq_id of Eq_id.t (* introduced expression *)
  | Var of Variable.t
  | Global of int
  | Var_within_closure of Var_within_closure.t
  | Static_exception_arg of Static_exception.t * int

module Eq_left_m = struct
  type t = equation_left
  let compare x y = match x, y with
    | Eq_id x, Eq_id y ->
      Eq_id.compare x y
    | Var x, Var y ->
      Variable.compare x y
    | Global x, Global y ->
      Ext_types.Int.compare x y
    | Var_within_closure x, Var_within_closure y ->
      Var_within_closure.compare x y
    | Static_exception_arg (x, n), Static_exception_arg (y, m) ->
      let c = Static_exception.compare x y in
      if c = 0 then
        Ext_types.Int.compare n m
      else c
    | Eq_id _, _ -> -1
    | _, Eq_id _ -> 1
    | Var _, _ -> -1
    | _, Var _ -> 1
    | Global _, _ -> -1
    | _, Global _ -> 1
    | Var_within_closure _, Static_exception_arg _ -> -1
    | Static_exception_arg _, Var_within_closure _ -> 1

  let equal x y = compare x y = 0

  let hash x = Hashtbl.hash x

  let print _ = failwith "not_implemented"

  let output _ = failwith "not_implemented"

end

module Eq_left = struct
  include Ext_types.Identifiable.Make(Eq_left_m)
end

type lset = Eq_left.Set.t

type resolved_equation =
  | Ground_const of Variable.t
  (* Any constant expression that is not a new block in that file.
     The variable is the one of the let introducing the value.
     It is used as the cannonical identifier of the value *)
  | Block of Variable.t * lset array
  | Bottom
  | Not_const

type alias_equation =
  | Field of int * lset
  (* An unresolved field expression.
     If the lval is Block, then the result will be the field. *)
  | Set of lset

type equation_right =
  | Resolved of resolved_equation
  | Alias of alias_equation

type equations = equation_right Eq_left.Tbl.t

let add equations l_eq r_eq = Eq_left.Tbl.add equations l_eq r_eq
let replace equations l_eq r_eq = Eq_left.Tbl.replace equations l_eq r_eq

let to_lset t = function
  | Alias (Set s) -> s
  | r_eq ->
    let id = fresh () in
    add t (Eq_id id) r_eq;
    Eq_left.Set.singleton (Eq_id id)

let add_branch equations lval rval =
  match Eq_left.Tbl.find equations lval with
  | r ->
    let set = Eq_left.Set.union (to_lset equations rval) (to_lset equations r) in
    Eq_left.Tbl.replace equations lval (Alias (Set set))
  | exception Not_found ->
    Misc.fatal_error "Alias_analysis : Missing equation to add"

let alias eq_left =
  Alias (Set (Eq_left.Set.singleton eq_left))

let alias_set sets =
  let set = List.fold_left Eq_left.Set.union Eq_left.Set.empty sets in
  Alias (Set set)

let rec collect_equations (t:equations) : Flambda.t -> equation_right = function
  | Var v -> alias (Var v)
  | Let (_, v, def, body) ->
    add t (Var v) (collect_equations_named t v def);
    collect_equations t body
  | Assign (v, expr) ->
    add t (Var v) (collect_equations t expr);
    Resolved Not_const
  | Let_rec (defs, body) ->
    List.iter (fun (v, def) ->
        add t (Var v) (collect_equations_named t v def))
      defs;
    collect_equations t body
  | Apply _ ->
    (* If we want to propagate arguments to function we also need to
       find escaping functions. This may not be that much clutter *)
    Resolved Not_const
  | Send (_, e1, e2, args, _) ->
    List.iter (fun e ->
        ignore (collect_equations t e : equation_right))
      (e1 :: e2 :: args);
    Resolved Not_const
  | If_then_else (cond, ifso, ifnot) ->
    ignore (collect_equations t cond : equation_right);
    let ifso_eq = to_lset t (collect_equations t ifso) in
    let ifnot_eq = to_lset t (collect_equations t ifnot) in
    alias_set [ifso_eq; ifnot_eq]
  | Switch (cond, { consts; blocks; failaction }) ->
    ignore (collect_equations t cond : equation_right);
    let conv (_, e) : lset = to_lset t (collect_equations t e) in
    let eqs = List.map conv (consts @ blocks) in
    let eqs = match failaction with
      | None -> eqs
      | Some e -> conv ((),e) :: eqs
    in
    alias_set eqs
  | String_switch (cond, branches, failaction) ->
    ignore (collect_equations t cond : equation_right);
    let conv (_,e) = to_lset t (collect_equations t e) in
    let eqs = List.map conv branches in
    let eqs = match failaction with
    | None -> eqs
    | Some e -> conv ((),e) :: eqs
    in
    alias_set eqs
  | Static_raise (e, args) ->
    (* It is ok to use add_branch: every potential 'to_lset' is
       outside the corresponding static_catch where all the branch
       will have been added. *)
    List.iteri (fun i arg ->
        add_branch t (Static_exception_arg (e, i)) (collect_equations t arg))
      args;
    Resolved Bottom
  | Static_catch (e, params, body, handler) ->
    List.iteri (fun i param ->
        add t (Static_exception_arg (e, i)) (alias (Var param)))
      params;
    let body_eq = to_lset t (collect_equations t body) in
    let handler_eq = to_lset t (collect_equations t handler) in
    Alias (Set (Eq_left.Set.union body_eq handler_eq))
  | Try_with (body, _var, handler) ->
    let body_eq = to_lset t (collect_equations t body) in
    let handler_eq = to_lset t (collect_equations t handler) in
    Alias (Set (Eq_left.Set.union body_eq handler_eq))
  | While (cond, body) ->
    ignore (collect_equations t cond : equation_right);
    ignore (collect_equations t body : equation_right);
    Resolved Not_const
  | For (_, low, high, _, body) ->
    ignore (collect_equations t low : equation_right);
    ignore (collect_equations t high : equation_right);
    ignore (collect_equations t body : equation_right);
    Resolved Not_const
  | Proved_unreachable ->
    Resolved Not_const

and collect_equations_named (t:equations) (var:Variable.t) : Flambda.named -> equation_right = function
  | Symbol _
  | Const _ ->
    Resolved (Ground_const var)

  | Expr e ->
    collect_equations t e

  | Set_of_closures { free_vars; specialised_args } ->
    Variable.Map.iter (fun free_var def_var ->
        add t
          (Var_within_closure (Var_within_closure.wrap free_var))
          (alias (Var def_var)))
      free_vars;
    Variable.Map.iter (fun arg var ->
        add t
          (Var arg)
          (alias (Var var)))
      specialised_args;
    Resolved Not_const

  | Project_closure _
  | Move_within_set_of_closures _ ->
    (* Probably don't need to bother with closures *)
    Resolved Not_const

  | Project_var {var} ->
    alias (Var_within_closure var)

  | Prim (Pfield n,[arg],_) ->
    Alias (Field (n, Eq_left.Set.singleton (Var arg)))

  | Prim (Pmakeblock (_tag, Immutable), args, _) ->
    let fields =
      List.map (fun v ->
          Eq_left.Set.singleton (Var v)) args
    in
    Resolved (Block (var, Array.of_list fields))

  | Prim (Pgetglobalfield (id, n), [], _) when
      id = Compilation_unit.get_current_id_exn () ->
    alias (Global n)

  | Prim (Psetglobalfield (_, n), [arg], _) ->
    add t (Global n) (alias (Var arg));
    Resolved Not_const

  | Prim (Praise _, _, _) ->
    Resolved Bottom

  | Prim _ ->
    Resolved Not_const

let find t l =
  try
    Eq_left.Tbl.find t l
  with Not_found ->
    Resolved Not_const

type get_field =
  | Not_const
  | Set of lset
  | Field

exception Not_const_field
exception Field_through_field

let block_field t l field =
  match find t l with
  | Resolved (Block (_, fields)) ->
    if Array.length fields <= field then
      raise Not_const_field
    else
      fields.(field)
  | Resolved (Not_const | Ground_const _) ->
    raise Not_const_field
  | Alias (Field _) ->
    raise Field_through_field
  | Resolved Bottom
  | Alias (Set _) ->
    assert false

let blocks_field t lset field =
  try
    let set =
      Eq_left.Set.fold (fun l acc ->
          let set = block_field t l field in
          Eq_left.Set.union set acc)
        lset Eq_left.Set.empty
    in
    Set set
  with
  | Not_const_field ->
    Not_const
  | Field_through_field ->
    Field

(* Assumes that there are no equality loop *)
let rec follow_aliases t l =
  match find t l with
  | Resolved (Not_const | Ground_const _) ->
    Eq_left.Set.singleton l
  | Resolved (Block (_v, _fields)) ->
    (* Not followed to avoid loop: traversed in a second pass: update_block *)
    Eq_left.Set.singleton l
  | Resolved Bottom ->
    Eq_left.Set.empty
  | Alias (Set s) ->
    let set = follow_set t s in
    replace t l (Alias (Set set));
    set
  | Alias (Field (n, s)) ->
    let set = follow_set t s in
    begin match blocks_field t set n with
    | Not_const ->
      replace t l (Resolved Not_const);
      Eq_left.Set.singleton l
    | Field ->
      replace t l (Alias (Field (n, set)));
      Eq_left.Set.singleton l
    | Set s ->
      replace t l (Alias (Set s));
      s
    end

and follow_set t s =
  Eq_left.Set.fold (fun l acc ->
      Eq_left.Set.union
        (follow_aliases t l)
        acc)
    s Eq_left.Set.empty

let update_block t l =
  match find t l with
  | Resolved (Block (v, fields)) ->
    let fields = Array.map (follow_set t) fields in
    replace t l (Resolved (Block (v, fields)))
  | _ ->
    ()

let contains_fields t =
  Eq_left.Tbl.fold (fun _ elt acc ->
      match elt with
      | Alias (Field _) ->
        true
      | _ -> acc)
    t false

let rec fixpoint t =
  Eq_left.Tbl.iter (fun l _ ->
      ignore(follow_aliases t l:lset))
    t;
  Eq_left.Tbl.iter (fun l _ -> update_block t l) t;
  if contains_fields t then
    fixpoint t

let run tree =
  let t = Eq_left.Tbl.create 10 in
  let _ : equation_right = collect_equations t tree in
  fixpoint t;
  Eq_left.Tbl.fold (fun k r map ->
      match k with
      | Var left -> begin
          match r with
          | Resolved (Ground_const right)
          | Resolved (Block (right, _)) ->
            Variable.Map.add left right map
          | _ -> map
        end
      | _ -> map)
    t Variable.Map.empty
