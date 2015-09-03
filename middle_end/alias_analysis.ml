
(* This analysis finds for variables that could be constant, of which constants
   they can be aliases.

   This works by accumulating a set of equations, then propagating them.

   The representable equations are of the form:
     the set A is a super-ser of the set B

   The accepted left sets (A) are all the identifiers that can carry a
   value in flambda. This means that equations more precisely represents:

     the set of all the value that the identifier I can contains is
     a super-set of the set B

   The accepter right sets (B) are descriptions of singleton values,
   sets of blocks allocated at a single program point or potential alias.

   The described aliases can represents direct alias, or field access:
     the set A is a super-set of what the set B contains in its f field

   Once all equations are collected, all direct subset aliases are
   completely propagated, and sequences of
     A super-set of B's field f
     B is a super-set of the sets of blocks containting C in its field f
   are resolved to A super-set of C

   [v] When all equations are propagated, we ends up with for every variable
   either:
   - [v] is aliased to blocks allocated at a given point (identified by a variable [x]).
   - [v] is aliased to simple constants allocated at a given point (identified by a variable [x]).
   - [v] can be an alias of multiple values

   In the first two cases, we will keep the information [v] is an
   alias of a value allocated at the definition of [x]. If [x] is
   not a constant, this does not provide much information, but if
   [x] is a constant, that means that there is a single value
   allocated at [x], hence [v] is an alias of [x].
*)


module Eq_id = Ext_types.Id(struct end)
let fresh () = Eq_id.create ()

(* All kinds of identifiers to which a value can be bound.
   Some expressions are not directly let-bound, in some cases
   an identifier is introduced to represent them: it's the Eq_id.t
   type. *)
type equation_left =
  | Eq_id of Eq_id.t (* introduced expression *)
  | Var of Variable.t
  | Mut of Mutable_variable.t
  | Global of int
  | Symbol of Symbol.t
  | Var_within_closure of Var_within_closure.t
  | Static_exception_arg of Static_exception.t * int

module Eq_left_m = struct
  type t = equation_left
  let compare x y = match x, y with
    | Eq_id x, Eq_id y ->
      Eq_id.compare x y
    | Var x, Var y ->
      Variable.compare x y
    | Mut x, Mut y ->
      Mutable_variable.compare x y
    | Global x, Global y ->
      Ext_types.Int.compare x y
    | Symbol x, Symbol y ->
      Symbol.compare x y
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
    | Mut _, _ -> -1
    | _, Mut _ -> 1
    | Global _, _ -> -1
    | _, Global _ -> 1
    | Symbol _, _ -> -1
    | _, Symbol _ -> 1
    | Var_within_closure _, Static_exception_arg _ -> -1
    | Static_exception_arg _, Var_within_closure _ -> 1

  let equal x y = compare x y = 0

  let hash x = Hashtbl.hash x

  let print ppf = function
    | Eq_id id -> Format.fprintf ppf "Eq_id %a" Eq_id.print id
    | Var var ->  Format.fprintf ppf "Var %a" Variable.print var
    | Mut var ->  Format.fprintf ppf "Mut %a" Mutable_variable.print var
    | Global i -> Format.fprintf ppf "Global %i" i
    | Var_within_closure v ->
                  Format.fprintf ppf "Var_withing_closure %a" Var_within_closure.print v
    | Static_exception_arg (s,n) ->
                  Format.fprintf ppf "St_exn_arg (%a, %i)" Static_exception.print s n
    | Symbol s ->
      Format.fprintf ppf "Symbol %a" Symbol.print s

  let output _ = failwith "not_implemented"

end

module Eq_left = struct
  include Ext_types.Identifiable.Make(Eq_left_m)
end

type lset = Eq_left.Set.t

type allocation_point =
  | Symbol of Symbol.t
  | Variable of Variable.t

type block_field =
  | Id of lset
  | Const

type resolved_equation =
  | Ground_const of allocation_point
  (* Any constant expression that is not a new block in that file.
     The variable is the one of the let introducing the value.
     It is used as the cannonical identifier of the value *)
  | Block of allocation_point * block_field array
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

let print_allocation_point ppf = function
  | Symbol s -> Symbol.print ppf s
  | Variable v -> Variable.print ppf v

let equation_right_print ppf = function
  | Resolved (Ground_const v) -> Format.fprintf ppf "Ground_const %a" print_allocation_point v
  | Resolved (Block (v,_)) -> Format.fprintf ppf "Block %a" print_allocation_point v
  | Resolved _ -> Format.fprintf ppf "Resolved"
  | Alias (Set s) -> Format.fprintf ppf "Set %a" Eq_left.Set.print s
  | Alias (Field (n,s)) -> Format.fprintf ppf "Field %i %a" n Eq_left.Set.print s

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
    Eq_left.Tbl.add equations lval rval

let alias eq_left =
  Alias (Set (Eq_left.Set.singleton eq_left))

let alias_set sets =
  let set = List.fold_left Eq_left.Set.union Eq_left.Set.empty sets in
  Alias (Set set)

let constant_defining_value_block_field_id : Flambda.constant_defining_value_block_field -> block_field = function
  | Symbol s -> Id (Eq_left.Set.singleton (Symbol s))
  | Const _ -> Const

let rec collect_equations (t:equations) : Flambda.t -> equation_right = function
  | Var v -> alias (Var v)
  | Let (v, def, body) ->
    add t (Var v) (collect_equations_named t v def);
    collect_equations t body
  | Let_mutable (mut, def, body) ->
    add t (Mut mut) (alias (Var def));
    collect_equations t body
  | Assign { being_assigned; new_value } ->
    add_branch t (Mut being_assigned) (alias (Var new_value));
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
  | Send _ ->
    Resolved Not_const
  | If_then_else (_cond, ifso, ifnot) ->
    let ifso_eq = to_lset t (collect_equations t ifso) in
    let ifnot_eq = to_lset t (collect_equations t ifnot) in
    alias_set [ifso_eq; ifnot_eq]
  | Switch (_cond, { consts; blocks; failaction }) ->
    let conv (_, e) : lset = to_lset t (collect_equations t e) in
    let eqs = List.map conv (consts @ blocks) in
    let eqs = match failaction with
      | None -> eqs
      | Some e -> conv ((),e) :: eqs
    in
    alias_set eqs
  | String_switch (_cond, branches, failaction) ->
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
  | For { body } ->
    ignore (collect_equations t body : equation_right);
    Resolved Not_const
  | Proved_unreachable ->
    Resolved Not_const

and collect_equations_named (t:equations) (var:Variable.t) : Flambda.named -> equation_right = function
  | Symbol _
  | Const _ ->
    Resolved (Ground_const (Variable var))
  | Allocated_const _ ->
    (* CR mshinwell for pchambart: Is this right?
       Should we do someting with the [Allocated_const (Block (...))] case,
       which contains free variables? *)
    Resolved (Ground_const (Variable var))
  | Read_mutable mut ->
    alias (Mut mut)
  | Read_symbol_field (_symbol, _field) ->
    assert false
  | Expr e ->
    collect_equations t e

  | Set_of_closures { function_decls; free_vars; specialised_args } ->
    Variable.Map.iter (fun _ ({ body }:Flambda.function_declaration) ->
        ignore (collect_equations t body:equation_right);
      )
      function_decls.funs;
    Variable.Map.iter (fun free_var def_var ->
        let var_within_closure =
          Var_within_closure (Var_within_closure.wrap free_var)
        in
        add t
          (Var free_var)
          (alias var_within_closure);
        add t
          var_within_closure
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
    Resolved (Ground_const (Variable var))

  | Project_var {var} ->
    alias (Var_within_closure var)

  | Prim (Pfield n,[arg],_) ->
    Alias (Field (n, Eq_left.Set.singleton (Var arg)))

  (* We could also track aliases on mutable blocks (and setfields),
     but that would need an associated escape analysis *)
  | Prim (Pmakeblock (_tag, Immutable), args, _) ->
    let fields =
      List.map (fun v ->
          Id (Eq_left.Set.singleton (Var v))) args
    in
    Resolved (Block (Variable var, Array.of_list fields))

  | Prim (Pgetglobalfield (id, n), _, _) when
      id = Compilation_unit.get_current_id_exn () ->
    (* Format.printf "add alias global %i@." n; *)
    alias (Global n)

  (* | Prim (Pgetglobalfield (id, n), _, _) -> *)
  (*   Format.printf "get global diff %a %a %i@." *)
  (*     Ident.print id *)
  (*     Ident.print (Compilation_unit.get_current_id_exn ()) *)
  (*     n; *)
  (*   Resolved Not_const *)

  | Prim (Psetglobalfield n, [arg], _) ->
    add t (Global n) (alias (Var arg));
    Resolved Not_const

  | Prim (Praise _, _, _) ->
    Resolved Bottom

  | Prim _ ->
    Resolved Not_const

and constant_defining_value_to_equation t sym (def:Flambda.constant_defining_value) : equation_right =
  match def with
  | Flambda.Allocated_const _ ->
    Resolved (Ground_const (Symbol sym))
  | Flambda.Block (_,fields) ->
    let fields = Array.map constant_defining_value_block_field_id (Array.of_list fields) in
    Resolved (Block (Symbol sym, fields))
  | Flambda.Set_of_closures { function_decls; free_vars; specialised_args } ->
    Variable.Map.iter (fun _ ({ body }:Flambda.function_declaration) ->
        ignore (collect_equations t body:equation_right);
      )
      function_decls.funs;
    assert(Variable.Map.is_empty free_vars);
    assert(Variable.Map.is_empty specialised_args);
    Resolved Not_const

  | Flambda.Project_closure (_,_) -> failwith "TODO"


and collect_equation_program (t:equations) : Flambda.program -> unit = function
  | End _ -> ()
  | Let_symbol (symbol, def, program) ->
    let eq_right = constant_defining_value_to_equation t symbol def in
    add t (Symbol symbol) eq_right;
    collect_equation_program t program
  | _ -> assert false

  (* | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program *)
  (* | Import_symbol of Symbol.t * program *)
  (* | Initialize_symbol of Symbol.t * t * program *)
  (* | End *)

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
          match block_field t l field with
          | Id set ->
            Eq_left.Set.union set acc
          | Const ->
            raise Not_const_field)
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
    if Eq_left.Set.cardinal set = 1 then
      let elt = Eq_left.Set.choose set in
      replace t l (find t elt)
    else
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
    let fields = Array.map (function
        | Id field -> Id (follow_set t field)
        | Const -> Const)
        fields
    in
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

let run program =
  let t = Eq_left.Tbl.create 10 in
  let () = collect_equation_program t program in
  Format.printf "alias equations:@.%a@."
    (Eq_left.Map.print equation_right_print) (Eq_left.Tbl.to_map t);
  fixpoint t;
  Eq_left.Tbl.fold (fun k r map ->
      match k with
      | Var left ->
        (* Format.printf "look at: %a -> %a@." *)
        (*   Variable.print left *)
        (*   equation_right_print r; *)
        begin
          match r with
          | Resolved (Ground_const right)
          | Resolved (Block (right, _)) ->
            Variable.Map.add left right map
          | _ -> map
        end
      | _ -> map)
    t Variable.Map.empty

let () = let _ = equation_right_print in ()


(* Version without collection *)


type constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * Variable.t list
  | Set_of_closures of Flambda.set_of_closures
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of
      Flambda.move_within_set_of_closures
  | Project_var of Flambda.project_var
  | Field of Variable.t * int
  | Const of Flambda.const
  | Symbol of Symbol.t
  | Variable of Variable.t

type definitions =
  {
    variable : constant_defining_value Variable.Map.t;
    symbol : Flambda.constant_defining_value Symbol.Map.t;
    symbol_alias : Variable.t Symbol.Map.t;
  }

let rec resolve_definition
    (definitions: definitions)
    (var: Variable.t)
    (def: constant_defining_value) : allocation_point =
  match def with
  | Allocated_const _
  | Block _
  | Set_of_closures _
  | Project_closure _
  | Const _
  | Move_within_set_of_closures _ ->
    Variable var
  | Project_var {var} ->
    fetch_variable definitions (Var_within_closure.unwrap var)
  | Variable v ->
    fetch_variable definitions v
  | Symbol sym -> begin
      match Symbol.Map.find sym definitions.symbol_alias with
      | exception Not_found ->
        Symbol sym
      | v ->
        fetch_variable definitions v
    end
  | Field (v, n) ->
    match fetch_variable definitions v with
    | Symbol s ->
      fetch_symbol_field definitions s n
    | Variable v ->
      fetch_variable_field definitions v n

and fetch_variable
    (definitions: definitions)
    (var: Variable.t) : allocation_point =
  match Variable.Map.find var definitions.variable with
  | exception Not_found -> Variable var
  | def ->
    resolve_definition definitions var def

and fetch_variable_field
    (definitions: definitions)
    (var: Variable.t)
    (field: int) : allocation_point =
  match Variable.Map.find var definitions.variable with
  | Block (_, fields) ->
    begin match List.nth fields field with
    | exception Not_found ->
      Misc.fatal_errorf "No field %i in block %a" field Variable.print var
    | v ->
      fetch_variable definitions v
    end
  | exception Not_found ->
    Misc.fatal_errorf "No definition for field access to %a" Variable.print var
  | Symbol _ | Variable _ | Project_var _ | Field _ ->
    (* Must have been resolved *)
    assert false
  | Const _ | Allocated_const _
  | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _ ->
    Misc.fatal_errorf "Field access to %a which is not a block" Variable.print var

and fetch_symbol_field
    (definitions: definitions)
    (sym: Symbol.t)
    (field: int) : allocation_point =
  match Symbol.Map.find sym definitions.symbol with
  | Block (_, fields) ->
    begin match List.nth fields field with
    | exception Not_found ->
      Misc.fatal_errorf "No field %i in block %a" field Symbol.print sym
    | Symbol s ->
      Symbol s
    | Const _ ->
      Symbol sym
    end
  | exception Not_found ->
    Misc.fatal_errorf "No definition for field access to %a" Symbol.print sym
  | Allocated_const _ | Set_of_closures _ | Project_closure _ ->
    Misc.fatal_errorf "Field access to %a which is not a block" Symbol.print sym

let second_take variable symbol symbol_alias =
  let definitions = { variable; symbol; symbol_alias; } in
  Variable.Map.mapi (resolve_definition definitions) definitions.variable
