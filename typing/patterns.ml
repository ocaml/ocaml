(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Gabriel Scherer, projet Partout, INRIA Paris-Saclay           *)
(*          Thomas Refis, Jane Street Europe                              *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types
open Typedtree

(* useful pattern auxiliary functions *)

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

module Non_empty_row = struct
  type 'a t = 'a * Typedtree.pattern list

  let of_initial = function
    | [] -> assert false
    | pat :: patl -> (pat, patl)

  let map_first f (p, patl) = (f p, patl)
end

(* "views" on patterns are polymorphic variants
   that allow to restrict the set of pattern constructors
   statically allowed at a particular place *)

module Simple = struct
  type view = [
    | `Any
    | `Constant of constant
    | `Tuple of pattern list
    | `Construct of
        Longident.t loc * constructor_description * pattern list
    | `Variant of label * pattern option * row_desc ref
    | `Record of
        (Longident.t loc * label_description * pattern) list * closed_flag
    | `Array of pattern list
    | `Lazy of pattern
  ]

  type pattern = view pattern_data

  let omega = { omega with pat_desc = `Any }
end

module Half_simple = struct
  type view = [
    | Simple.view
    | `Or of pattern * pattern * row_desc option
  ]

  type pattern = view pattern_data
end

module General = struct
  type view = [
    | Half_simple.view
    | `Var of Ident.t * string loc
    | `Alias of pattern * Ident.t * string loc
  ]
  type pattern = view pattern_data

  let view_desc = function
    | Tpat_any ->
       `Any
    | Tpat_var (id, str) ->
       `Var (id, str)
    | Tpat_alias (p, id, str) ->
       `Alias (p, id, str)
    | Tpat_constant cst ->
       `Constant cst
    | Tpat_tuple ps ->
       `Tuple ps
    | Tpat_construct (cstr, cstr_descr, args, _) ->
       `Construct (cstr, cstr_descr, args)
    | Tpat_variant (cstr, arg, row_desc) ->
       `Variant (cstr, arg, row_desc)
    | Tpat_record (fields, closed) ->
       `Record (fields, closed)
    | Tpat_array ps -> `Array ps
    | Tpat_or (p, q, row_desc) -> `Or (p, q, row_desc)
    | Tpat_lazy p -> `Lazy p

  let view p : pattern =
    { p with pat_desc = view_desc p.pat_desc }

  let erase_desc = function
    | `Any -> Tpat_any
    | `Var (id, str) -> Tpat_var (id, str)
    | `Alias (p, id, str) -> Tpat_alias (p, id, str)
    | `Constant cst -> Tpat_constant cst
    | `Tuple ps -> Tpat_tuple ps
    | `Construct (cstr, cst_descr, args) ->
       Tpat_construct (cstr, cst_descr, args, None)
    | `Variant (cstr, arg, row_desc) ->
       Tpat_variant (cstr, arg, row_desc)
    | `Record (fields, closed) ->
       Tpat_record (fields, closed)
    | `Array ps -> Tpat_array ps
    | `Or (p, q, row_desc) -> Tpat_or (p, q, row_desc)
    | `Lazy p -> Tpat_lazy p

  let erase p : Typedtree.pattern =
    { p with pat_desc = erase_desc p.pat_desc }

  let rec strip_vars (p : pattern) : Half_simple.pattern =
    match p.pat_desc with
    | `Alias (p, _, _) -> strip_vars (view p)
    | `Var _ -> { p with pat_desc = `Any }
    | #Half_simple.view as view -> { p with pat_desc = view }
end

(* the head constructor of a simple pattern *)

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

  (** [deconstruct p] returns the head of [p] and the list of sub patterns. *)
  val deconstruct : Simple.pattern -> t * pattern list

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

  let deconstruct (q : Simple.pattern) =
    let deconstruct_desc = function
      | `Any -> Any, []
      | `Constant c -> Constant c, []
      | `Tuple args ->
          Tuple (List.length args), args
      | `Construct (_, c, args) ->
          Construct c, args
      | `Variant (tag, arg, cstr_row) ->
          let has_arg, pats =
            match arg with
            | None -> false, []
            | Some a -> true, [a]
          in
          let type_row () =
            match get_desc (Ctype.expand_head q.pat_env q.pat_type) with
            | Tvariant type_row -> type_row
            | _ -> assert false
          in
          Variant {tag; has_arg; cstr_row; type_row}, pats
      | `Array args ->
          Array (List.length args), args
      | `Record (largs, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record lbls, pats
      | `Lazy p ->
          Lazy, [p]
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
          Tpat_construct (lid_loc, c, omegas c.cstr_arity, None)
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
