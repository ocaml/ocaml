(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let equal (x : t) y = x = y
    let compare (x : t) y = compare x y
    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      Item { comp_unit = current_unit; id = !id }

  let of_compilation_unit_id id =
    if not (Ident.persistent id) then
      Misc.fatal_errorf "Types.Uid.of_compilation_unit_id %S" (Ident.name id);
    Compilation_unit (Ident.name id)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Module
    | Module_type
    | Class
    | Class_type ->
        true
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t
    let compare = compare

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type
  end

  include T

  module Map = Map.Make(T)
end

type var = Ident.t
type t = { uid: Uid.t option; desc: desc }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string

let print fmt =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  let rec aux fmt { uid; desc } =
    match desc with
    | Var id ->
        Format.fprintf fmt "%a%a" Ident.print id print_uid_opt uid
    | Abs (id, t) ->
        Format.fprintf fmt "Abs@[%a@,(@[%a,@ @[%a@]@])@]"
          print_uid_opt uid Ident.print id aux t
    | App (t1, t2) ->
        Format.fprintf fmt "@[%a(@,%a)%a@]" aux t1 aux t2
          print_uid_opt uid
    | Leaf ->
        Format.fprintf fmt "<%a>" (Format.pp_print_option Uid.print) uid
    | Proj (t, (name, ns)) ->
        begin match uid with
        | None ->
            Format.fprintf fmt "@[%a@ .@ %S[%s]@]"
              aux t
              name
              (Sig_component_kind.to_string ns)
        | Some uid ->
            Format.fprintf fmt "@[(%a@ .@ %S[%s])<%a>@]"
              aux t
              name
              (Sig_component_kind.to_string ns)
              Uid.print uid
        end
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct map ->
        let print_map fmt =
          Item.Map.iter (fun (name, ns) t ->
              Format.fprintf fmt "@[<hv 4>(%S, %s) ->@ %a;@]@,"
                name
                (Sig_component_kind.to_string ns)
                aux t
            )
        in
        Format.fprintf fmt "{@[<v>%a@,%a@]}" print_uid_opt uid print_map map
  in
  Format.fprintf fmt"@[%a@]@;" aux

let improve_uid uid t =
  match t.uid with
  | Some _ -> t
  | None -> { t with uid }

let fresh_var ?(name="shape-var") uid =
  let var = Ident.create_local name in
  var, { uid = Some uid; desc = Var var }

let for_unnamed_functor_param = Ident.create_local "()"

let var uid id =
  { uid = Some uid; desc = Var id }

let abs ?uid var body =
  { uid; desc = Abs (var, body) }

let str ?uid map =
  { uid; desc = Struct map }

let leaf uid =
  { uid = Some uid; desc = Leaf }

let proj ?uid t item =
  match t.desc with
  | Leaf ->
      (* When stuck projecting in a leaf we propagate the leaf
        as a best effort *)
      t
  | Struct map ->
      begin try Item.Map.find item map
      with Not_found -> t (* ill-typed program *)
      end
  | _ ->
      { uid; desc = Proj (t, item) }

let app ?uid f ~arg =
      { uid; desc = App (f, arg) }

module Make_reduce(Params : sig
  type env
  val fuel : int
  val read_unit_shape : unit_name:string -> t option
  val find_shape : env -> Ident.t -> t
end) = struct
  type strategy = Weak | Strong
  type local_env = t option Ident.tbl
  (* Local environments carry an optional strong normal form: when
     reducing in the body of an abstraction [Abs(x, body)], we bind
     [x] to [None] in the environment. [Some v] is used for actual
     substitutions, for example in [App(Abs(x, body), t)], when [v] is
     the strong normal form of [v]. *)

  type env = {
    fuel: int ref;
    global_env: Params.env;
    local_env: local_env;
  }

  let bind env var shape =
    { env with local_env = Ident.add var shape env.local_env }

  (* # Note on the implementation of strong reduction.

     ## Simple version

     The general idea is to reduce under abstractions
     (and within structures). This could be implemented with code
     looking as follows:

     let rec reduce_strong env = function
     | ...
     | Abs (x, body) ->
       let env' = bind env x None in
       Abs (x, reduce_strong env' body)
     | App(t, u) ->
       let f = reduce_strong env t in
       let v = reduce_strong env u in
       begin match f with
       | Abs(x, body) ->
         let env' = bind env x (Some v) in
         reduce_strong env' body
       | other -> App(other, v)
       end


     ## Mixing Weak|Strong for efficiency

     A problem with this approach is that the body of `t` is traversed twice:
     - once when computing `let f = reduce_strong env t`, which goes under abstractions
     - a second time when computing `reduce_strong (bind env x v) body`

     Traversing the input twice can lead to an exponential blowup, consider for example
       (\x2. (\x1. (\x0. large_normal_form) y0) y1) y2
     which comes naturally as a desugaring of
       let x2 = y2 in let x1 = y1 in let x0 = y0 in large_normal_form
     and whose reduction following the rule would traverse `large_normal_form` 8 times.

     To avoid this problem, we mix strong reductions as above with "weak" reductions,
     that stop on abstractions and returns a partially-evaluated value.

     let rec reduce strategy env = function
     | ...
     | Abs(x, t) ->
       begin match strategy with
       | Weak -> Abs(x, t)
       | Strong ->
         let env' = bind env x None in
         Abs(x, reduce_strong env' t)
       end
     | App(t, u) ->
       let f = reduce Weak env t in
       let v = reduce Strong env u in
       begin match f with
       | Abs(x, body) ->
         let env' = bind env x (Some v) in
         reduce strategy env' body
       | other -> App(other, v)
       end

     With this version, on `App(t, u)` we first perform a Weak
     reduction of `t`, which stops at the first Abs(x, body) without
     traversing the body, which is only traversed during the reduction
     `reduce strategy env' body` that follows -- whether that
     reduction is Weak or Strong depends on the caller.

     Notice that even when the strategy is `Weak`, the function
     argument `u` is reduced strongly: only the "main" part of the
     code, the one whose evaluation will give the final result,
     is reduced weakly. This corresponds to the "code" argument
     in an abstract machine -- as opposed to the continuation and
     environments, where we only store strong values.

     This choice of reducing strongly outside the "main" subexpression
     gives us the following guarantee: at function types, a normal
     form for the Weak strategy must be either an Abs(x,t) or
     (if evaluation got stuck on free variables) a Strong normal form:
     in the
       | other -> App(other, v)
     case of application, we can assume that `other` is in strong
     normal form, and `App(other, v)` is also a strong normal form
     because `v` is.


     ### Returning environments

     A final problem remains with this second version: it has a bug.
     The problem is that the weak value corresponding to Abs(x, t)
     should not be
       Abs(x, t)
     but a closure
       Clos(local_env, x, t)
     Indeed `t` was not traversed yet and it may contain variables bound
     in the environment, so returning just `Abs(x, t)` returns unbound
     identifier.

     On the other hand, when we have a Strong normal form Abs(x, t),
     we do not need the environment anymore: `t` was fully traversed
     and all variables from the local environment have been substituted.

     Morally the type of "return value" should be different for
     the Weak and Strong strategies; we could represent this using
     a GADT, but it would make everything more complex, in particular
     the memoization code.

     Instead of introducing a notion of closures, we make our
     strategy-generic function [reduce_strat] return a pair of the
     local environment and the value (weak or strong). Then we have
     two helpers, [reduce_strong] which throws this local environment
     away and [reduce_weak] which returns it.

     (We could also introduce Let-bindings to be able to reify the
     local environment back into the term, but that would change the
     syntax observable from outside this module.)

     Scoping invariants about [reduce_strat strategy env]:
     - we assume that [env] only contains closed strong normal forms
     - then it returns a pair [(env', t)] where:
       + [env'] is an extension of [env]
       + if [strategy = Weak], [t] may contain variables bound in [env']
       + if [strategy = Strong], [t] is a closed strong normal form
  *)
  let rec reduce_weak env t =
    reduce_strat Weak env t

  and reduce_strong env t =
    let (_result_env, normal_form) = reduce_strat Strong env t in
    normal_form

  and reduce_strat strategy ({fuel; global_env; local_env; _} as env) t =
    let reduce env t = reduce_strat strategy env t in
    let return t = local_env, t in
    if !fuel < 0 then
      return t
    else
      match t.desc with
      | Comp_unit unit_name ->
          begin match Params.read_unit_shape ~unit_name with
          | Some t -> reduce env t
          | None -> return t
          end
      | App(f, arg) ->
          let lenv_f, f = reduce_weak env f in
          let arg = reduce_strong env arg in
          let env = { env with local_env = lenv_f } in
          begin match f.desc with
          | Abs(var, body) ->
              (* we only add Strong normal forms to the environment. *)
              let env = bind env var (Some arg) in
              let lenv_v, v = reduce env body in
              lenv_v, improve_uid t.uid v
          | _ ->
              (* If f is well-typed at a function type, its Weak
                 normal forms are either Abs or a Strong normal form. *)
              return { t with desc = App(f, arg) }
          end
      | Proj(str, item) ->
          let lenv_str, str = reduce_weak env str in
          let env = { env with local_env = lenv_str } in
          let nored () = return { t with desc = Proj(str, item) } in
          begin match str.desc with
          | Struct items ->
              begin match Item.Map.find item items with
              | exception Not_found -> nored ()
              | item ->
                  reduce env item
              end
          | _ ->
              (* If [str] is well-typed at a function type, its Weak
                 normal forms are either Abs or a Strong normal form. *)
              nored ()
          end
      | Abs(var, body) ->
          begin match strategy with
          | Weak -> return t
          | Strong ->
              let env = bind env var None in
              let _, v = reduce env body in
              return { t with desc = Abs(var, v) }
          end
      | Var id ->
          begin match Ident.find_same id local_env with
          (* local_env bindings are already in Strong normal form *)
          | None -> return t
          | Some def -> return def
          | exception Not_found ->
          match Params.find_shape global_env id with
          | res ->
              if res = t then
                (* reducing here would loop forever *)
                return t
              else begin
                decr fuel;
                reduce env res
              end
          | exception Not_found ->
              return t
          end
      | Leaf -> return t
      | Struct m ->
          begin match strategy with
          | Weak -> return t
          | Strong ->
              return { t with desc = Struct (Item.Map.map (reduce_strong env) m) }
          end

  let reduce global_env t =
    let fuel = ref Params.fuel in
    let local_env = Ident.empty in
    let env = { fuel; global_env; local_env} in
    reduce_strong env t
end

module Local_reduce =
  Make_reduce(struct
    type env = unit
    let fuel = 10
    let read_unit_shape ~unit_name:_ = None
    let find_shape _env _id = raise Not_found
  end)

let local_reduce shape =
  Local_reduce.reduce () shape

let dummy_mod = { uid = None; desc = Struct Item.Map.empty }

let of_path ~find_shape ~namespace =
  let rec aux : Sig_component_kind.t -> Path.t -> t = fun ns -> function
    | Pident id -> find_shape ns id
    | Pdot (path, name) -> proj (aux Module path) (name, ns)
    | Papply (p1, p2) -> app (aux Module p1) ~arg:(aux Module p2)
  in
  aux namespace

let for_persistent_unit s =
  { uid = Some (Uid.of_compilation_unit_id (Ident.create_persistent s));
    desc = Comp_unit s }

let leaf_for_unpack = { uid = None; desc = Leaf }

let set_uid_if_none t uid =
  match t.uid with
  | None -> { t with uid = Some uid }
  | _ -> t

module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add t item shape = Item.Map.add item shape t

  let add_value t id uid = Item.Map.add (Item.value id) (leaf uid) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id uid = Item.Map.add (Item.type_ id) (leaf uid) t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (leaf uid) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id uid =
    Item.Map.add (Item.extension_constructor id) (leaf uid) t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (leaf uid) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (leaf uid) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
