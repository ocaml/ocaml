open Asttypes
open Types
open Typedtree

let omega = {
  pat_desc = Tpat_any;
  pat_loc = Location.none;
  pat_extra = [];
  pat_type = Ctype.none;
  pat_env = Env.empty;
  pat_attributes = [];
}

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = List.map (fun _ -> omega) l

module Head : sig
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
    | Array of int
    | Lazy

  type t = desc pattern_data

  val arity : t -> int

  (** [deconstruct p] returns the head of [p] and the list of sub patterns.

      @raises [Invalid_arg _] if [p] is an or-pattern.  *)
  val deconstruct : pattern -> t * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val omega : t
end = struct
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of int
    | Lazy

  type t = desc pattern_data

  let deconstruct q =
    let rec deconstruct_desc = function
      | Tpat_any
      | Tpat_var _ -> Any, []
      | Tpat_constant c -> Constant c, []
      | Tpat_alias (p,_,_) -> deconstruct_desc p.pat_desc
      | Tpat_tuple args ->
          Tuple (List.length args), args
      | Tpat_construct (_, c, args) ->
          Construct c, args
      | Tpat_variant (tag, arg, cstr_row) ->
          let has_arg, pats =
            match arg with
            | None -> false, []
            | Some a -> true, [a]
          in
          let type_row () =
            match Ctype.expand_head q.pat_env q.pat_type with
              | {desc = Tvariant type_row} -> Btype.row_repr type_row
              | _ -> assert false
          in
          Variant {tag; has_arg; cstr_row; type_row}, pats
      | Tpat_array args ->
          Array (List.length args), args
      | Tpat_record (largs, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record lbls, pats
      | Tpat_lazy p ->
          Lazy, [p]
      | Tpat_or _ -> invalid_arg "Parmatch.Pattern_head.deconstruct: (P | Q)"
    in
    let desc, pats = deconstruct_desc q.pat_desc in
    { q with pat_desc = desc }, pats

  let arity t =
    match t.pat_desc with
      | Any -> 0
      | Constant _ -> 0
      | Construct c -> c.cstr_arity
      | Tuple n | Array n -> n
      | Record l -> List.length l
      | Variant { has_arg; _ } -> if has_arg then 1 else 0
      | Lazy -> 1

  let to_omega_pattern t =
    let pat_desc =
      let mkloc x = Location.mkloc x t.pat_loc in
      match t.pat_desc with
      | Any -> Tpat_any
      | Lazy -> Tpat_lazy omega
      | Constant c -> Tpat_constant c
      | Tuple n -> Tpat_tuple (omegas n)
      | Array n -> Tpat_array (omegas n)
      | Construct c ->
          let lid_loc = mkloc (Longident.Lident c.cstr_name) in
          Tpat_construct (lid_loc, c, omegas c.cstr_arity)
      | Variant { tag; has_arg; cstr_row } ->
          let arg_opt = if has_arg then Some omega else None in
          Tpat_variant (tag, arg_opt, cstr_row)
      | Record lbls ->
          let lst =
            List.map (fun lbl ->
              let lid_loc = mkloc (Longident.Lident lbl.lbl_name) in
              (lid_loc, lbl, omega)
            ) lbls
          in
          Tpat_record (lst, Closed)
    in
    { t with
      pat_desc;
      pat_extra = [];
    }

  let omega = { omega with pat_desc = Any }
end
