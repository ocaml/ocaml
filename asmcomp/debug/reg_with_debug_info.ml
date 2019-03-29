(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module V = Backend_var

(* We unfortunately cannot depend on [Proc] here, so we can't use
   [Proc.register_name]. *)
let register_name r = Printf.sprintf "r%d" r

let reg_printer ?print_reg ppf t =
  match print_reg with
  | None -> Reg.print ~register_name ppf t
  | Some print_reg -> print_reg ppf t

module Holds_value_of = struct
  type t =
    | Var of Backend_var.t
    | Const_int of Targetint.t
    | Const_naked_float of Int64.t
    | Const_symbol of string

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Var var1, Var var2 -> Backend_var.compare var1 var2
      | Const_int i1, Const_int i2 -> Targetint.compare i1 i2
      | Const_naked_float f1, Const_naked_float f2 -> Int64.compare f1 f2
      | Const_symbol sym1, Const_symbol sym2 -> String.compare sym1 sym2
      | Var _, _ -> -1
      | Const_int _, Var _ -> 1
      | Const_int _, _ -> -1
      | Const_naked_float _, (Var _ | Const_int _) -> 1
      | Const_naked_float _, _ -> -1
      | Const_symbol _, _ -> 1

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      match t with
      | Var var -> Hashtbl.hash (0, Backend_var.hash var)
      | Const_int i -> Hashtbl.hash (1, i)
      | Const_naked_float f -> Hashtbl.hash (2, f)
      | Const_symbol sym -> Hashtbl.hash (3, sym)

    let print ppf t =
      match t with
      | Var var ->
        Format.fprintf ppf "@[(Var@ %a)@]"
          Backend_var.print var
      | Const_int i ->
        Format.fprintf ppf "@[(Const_int@ %a)@]" Targetint.print i
      | Const_naked_float f ->
        Format.fprintf ppf "@[(Const_naked_float@ 0x%Lx)@]" f
      | Const_symbol sym ->
        Format.fprintf ppf "@[(Const_symbol@ %s)@]" sym

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

module Debug_info = struct
  type t = {
    holds_value_of : Holds_value_of.t;
    part_of_value : int;
    num_parts_of_value : int;
    is_parameter : Is_parameter.t;
    provenance : Backend_var.Provenance.t option;
  }

  let compare
        { holds_value_of = holds_value_of1; part_of_value = part_of_value1;
          num_parts_of_value = num_parts_of_value1;
          is_parameter = is_parameter1; provenance = provenance1;
        }
        { holds_value_of = holds_value_of2; part_of_value = part_of_value2;
          num_parts_of_value = num_parts_of_value2;
          is_parameter = is_parameter2; provenance = provenance2;
        } =
    let c = Holds_value_of.compare holds_value_of1 holds_value_of2 in
    if c <> 0 then c
    else
      let c = Stdlib.compare part_of_value1 part_of_value2 in
      if c <> 0 then c
      else
        let c = Stdlib.compare num_parts_of_value1 num_parts_of_value2 in
        if c <> 0 then c
        else
          let c = Is_parameter.compare is_parameter1 is_parameter2 in
          if c <> 0 then c
          else
            Option.compare Backend_var.Provenance.compare
              provenance1 provenance2

  let holds_value_of t = t.holds_value_of
  let part_of_value t = t.part_of_value
  let num_parts_of_value t = t.num_parts_of_value
  let is_parameter t = t.is_parameter
  let provenance t = t.provenance

  let print ppf t =
    Format.fprintf ppf "%a" Holds_value_of.print t.holds_value_of;
    (* To be enabled once new [Debuginfo] is merged.
    begin match t.provenance with
    | None -> ()
    | Some provenance ->
      let dbg = Backend_var.Provenance.location provenance in
      let block =
        Debuginfo.Current_block.to_block (Debuginfo.innermost_block dbg)
      in
      match block with
      | Toplevel -> Format.fprintf ppf "[toplevel]"
      | Block block -> Format.fprintf ppf "[%a]" Debuginfo.Block.print_id block
    end;
    *)
    if not (t.part_of_value = 0 && t.num_parts_of_value = 1) then begin
      Format.fprintf ppf "(%d/%d)" t.part_of_value t.num_parts_of_value
    end;
    begin match t.is_parameter with
    | Local -> ()
    | Parameter { index; } -> Format.fprintf ppf "[P%d]" index
    end
end

type t = {
  reg : Reg.t;
  debug_info : Debug_info.t option;
}

type reg_with_debug_info = t

let print ?print_reg ppf t =
  match t.debug_info with
  | None -> reg_printer ?print_reg ppf t.reg
  | Some debug_info ->
    Format.fprintf ppf "%a(%a)"
      (reg_printer ?print_reg) t.reg
      Debug_info.print debug_info

let compare { reg = reg1; debug_info = debug_info1; }
            { reg = reg2; debug_info = debug_info2; } =
  let c = reg1.stamp - reg2.stamp in
  if c <> 0 then c
  else Option.compare Debug_info.compare debug_info1 debug_info2

let create ~reg ~holds_value_of ~part_of_value ~num_parts_of_value
      is_parameter ~provenance =
  assert (num_parts_of_value >= 1);
  assert (part_of_value >= 0 && part_of_value < num_parts_of_value);
  let debug_info : Debug_info.t =
    { holds_value_of;
      part_of_value;
      num_parts_of_value;
      is_parameter;
      provenance;
    }
  in
  { reg;
    debug_info = Some debug_info;
  }

let create_with_debug_info ~reg ~debug_info =
  { reg;
    debug_info;
  }

let create_without_debug_info ~reg =
  { reg;
    debug_info = None;
  }

let create_copying_debug_info ~reg ~debug_info_from =
  { reg;
    debug_info = debug_info_from.debug_info;
  }

let reg t = t.reg
let location t = t.reg.loc

let maybe_holds_pointer t =
  match t.reg.typ with
  | Addr | Val -> true
  | Int | Float -> false

let always_holds_non_pointer t = not (maybe_holds_pointer t)

let assigned_to_stack t =
  match t.reg.loc with
  | Stack _ -> true
  | Reg _ | Unknown -> false

let regs_at_same_location (reg1 : Reg.t) (reg2 : Reg.t) ~register_class =
  (* We need to check the register classes too: two locations both saying
     "stack offset N" might actually be different physical locations, for
     example if one is of class "Int" and another "Float" on amd64.
     [register_class] will be [Proc.register_class], but cannot be here,
     due to a circular dependency. *)
  reg1.loc = reg2.loc
    && register_class reg1 = register_class reg2

let at_same_location t (reg : Reg.t) ~register_class =
  regs_at_same_location t.reg reg ~register_class

let debug_info t = t.debug_info

let clear_debug_info t =
  { t with debug_info = None; }

module type Set_intf = sig
  type t
  type reg_with_debug_info
  val print
     : ?print_reg:(Format.formatter -> Reg.t -> unit)
    -> Format.formatter
    -> t
    -> unit
  val equal : t -> t -> bool
  val empty : t
  val is_empty : t -> bool
  val of_list : reg_with_debug_info list -> t
  val of_array : reg_with_debug_info array -> t
  val without_debug_info : Reg.Set.t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val filter : t -> f:(reg_with_debug_info -> bool) -> t
  val fold : t -> init:'a -> f:(reg_with_debug_info -> 'a -> 'a) -> 'a
  val mem_reg : t -> Reg.t -> bool
  val find_reg : t -> Reg.t -> reg_with_debug_info option
  val filter_reg : t -> Reg.t -> t
  val forget_debug_info : t -> Reg.Set.t
  val made_unavailable_by_clobber
     : t
    -> regs_clobbered:Reg.t array
    -> register_class:(Reg.t -> int)
    -> t
end

module Set0 = Set.Make (struct
  type nonrec t = t

  (* The comparison function matches on the underlying [Reg.t] and the
     [Debug_info.t]. This ensures that, for example, when we intersect two sets
     together (for example to handle a join point in a function's code) then we
     correctly lose debugging information when it is not valid on all paths.

     For example, suppose the value of a mutable variable x is copied into
     another variable y; then there is a conditional where on one branch x is
     assigned and on the other branch it is not. This means that on the former
     branch we have forgotten about y holding the value of x; but we have not on
     the latter. At the join point we must have forgotten the information. *)
  let compare = compare
end)

module Make_set (C : sig
  val canonicalise : Set0.t -> Set0.t
end) = struct
  type t = Set0.t

  let canonicalise = C.canonicalise

  let print ?print_reg ppf t =
    let elts ppf t =
      Set0.iter (fun rd -> Format.fprintf ppf "@ %a" (print ?print_reg) rd) t
    in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts t

  let invariant t =
    (* if !Clflags.ddebug_invariants then begin *)
    if not (Set0.equal t (canonicalise t)) then begin
      Misc.fatal_errorf "Invariant broken:@ %a" (print ?print_reg:None) t
    end
    (* end *)

  let equal = Set0.equal

  let empty = Set0.empty
  let is_empty = Set0.is_empty

  let of_list xs = canonicalise (Set0.of_list xs)
  let of_array arr = of_list (Array.to_list arr)

  let without_debug_info regs =
    Reg.Set.fold (fun reg acc -> Set0.add (create_without_debug_info ~reg) acc)
      regs
      empty

  (* [inter] and [diff], by construction, preserve the canonical form. *)
  let inter t1 t2 =
    let t = Set0.inter t1 t2 in
    invariant t;
    t

  let diff t1 t2 =
    let t = Set0.diff t1 t2 in
    invariant t;
    t

  let map t ~f = Set0.map f t

  let fold t ~init ~f = Set0.fold f t init

  let filter t ~f = Set0.filter f t

  let subset = Set0.subset

  let mem_reg t (reg : Reg.t) =
    Set0.exists (fun t -> t.reg.stamp = reg.stamp) t

  (* CR-someday mshinwell: Well, it looks like we should have used a map.
     mshinwell: Also see @chambart's suggestion on GPR#856. *)
  let find_reg t (reg : Reg.t) =
    match Set0.elements (Set0.filter (fun t -> t.reg.stamp = reg.stamp) t) with
    | [] -> None
    | [reg] -> Some reg
    (* XXX We don't currently rule out the same Reg.t but different
       Debug_info.t ! *)
    | _ -> assert false

  let filter_reg t (reg : Reg.t) =
    Set0.filter (fun t -> t.reg.stamp <> reg.stamp) t

  let forget_debug_info t =
    Set0.fold (fun t acc -> Reg.Set.add (reg t) acc) t Reg.Set.empty

  let made_unavailable_by_clobber t ~regs_clobbered ~register_class =
    let t =
      Reg.Set.fold (fun reg acc ->
          let made_unavailable =
            Set0.filter (fun reg' ->
                regs_at_same_location reg'.reg reg ~register_class)
              t
          in
          Set0.union made_unavailable acc)
        (Reg.set_of_array regs_clobbered)
        (* ~init:*)empty
    in
    invariant t;
    t
end

module Set = struct
  include Make_set (struct
    let canonicalise t = t
  end)

  let singleton rd = Set0.singleton rd
  let add t rd = Set0.add rd t
  let union = Set0.union
end

module Canonical_set = struct
  let canonicalise set =
    let regs_by_var = V.Tbl.create 42 in
    Set0.iter (fun reg ->
        match debug_info reg with
        | None -> ()
        | Some debug_info ->
          match Debug_info.holds_value_of debug_info with
          | Var name ->
            begin match V.Tbl.find regs_by_var name with
            | exception Not_found -> V.Tbl.add regs_by_var name reg
            | (reg' : t) ->
              let loc = location reg in
              let loc' = location reg' in
              match loc, loc' with
              | Stack _, Reg _ ->
                (* We prefer registers that are assigned to the stack since
                   they probably give longer available ranges (less likely to
                   be clobbered).  Additionally, for the DWARF call site
                   argument descriptions, we can only describe registers
                   assigned to the stack, as all others may have been
                   clobbered. *)
                V.Tbl.remove regs_by_var name;
                V.Tbl.add regs_by_var name reg
              | Reg _, Stack _
              | Reg _, Reg _
              | Stack _, Stack _
              | _, Unknown
              | Unknown, _ ->
                (* In all other cases, we use a stable method to determine
                   which register to choose as the canonical one, to avoid
                   unnecessary opening and closing of ranges
                   (c.f. [Compute_ranges]). *)
                let c = Stdlib.compare loc loc' in
                if c = 0 then begin
                  ()
                end else if c < 0 then begin
                  V.Tbl.remove regs_by_var name;
                  V.Tbl.add regs_by_var name reg
                end else begin
                  V.Tbl.remove regs_by_var name;
                  V.Tbl.add regs_by_var name reg'
                end
            end
          | Const_int _ | Const_naked_float _ | Const_symbol _ -> ())
      set;
    let result =
      V.Tbl.fold (fun _var reg result ->
          Set0.add reg result)
        regs_by_var
        Set0.empty
    in
    (* To be enabled once Clflags pull request is presented
    if !Clflags.ddebug_invariants then begin
    *)
      assert (Set0.subset result set)
    (*
    end *);
    result

  include Make_set (struct
    let canonicalise = canonicalise
  end)

  let of_set set = canonicalise set

  let find_holding_value_of_variable t var =
    let t =
      Set0.filter (fun rd ->
          match debug_info rd with
          | None -> false
          | Some debug_info ->
            Holds_value_of.equal (Debug_info.holds_value_of debug_info)
              (Var var))
        t
    in
    match Set0.elements t with
    | [] -> None
    | [rd] -> Some rd
    | _ ->
      invariant t;
      assert false  (* The invariant must have been broken. *)
end
