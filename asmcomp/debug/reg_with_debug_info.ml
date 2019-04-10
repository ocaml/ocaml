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
    | Const_symbol of Backend_sym.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Var var1, Var var2 -> Backend_var.compare var1 var2
      | Const_int i1, Const_int i2 -> Targetint.compare i1 i2
      | Const_naked_float f1, Const_naked_float f2 -> Int64.compare f1 f2
      | Const_symbol sym1, Const_symbol sym2 -> Backend_sym.compare sym1 sym2
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
      | Const_symbol sym -> Hashtbl.hash (3, Backend_sym.hash sym)

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
        Format.fprintf ppf "@[(Const_symbol@ %a)@]" Backend_sym.print sym

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

  let equal t1 t2 = (compare t1 t2 = 0)

  let holds_value_of t = t.holds_value_of
  let part_of_value t = t.part_of_value
  let num_parts_of_value t = t.num_parts_of_value
  let is_parameter t = t.is_parameter
  let provenance t = t.provenance

  let print ppf t =
    Format.fprintf ppf "%a" Holds_value_of.print t.holds_value_of;
    begin match t.provenance with
    | None -> ()
    | Some provenance ->
      let dbg = Backend_var.Provenance.debuginfo provenance in
      let block =
        Debuginfo.Current_block.to_block (Debuginfo.innermost_block dbg)
      in
      match block with
      | Toplevel -> Format.fprintf ppf "[toplevel]"
      | Block block -> Format.fprintf ppf "[%a]" Debuginfo.Block.print_id block
    end;
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
  val print : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val empty : t
  val is_empty : t -> bool
  val of_list : reg_with_debug_info list -> t
  val of_array : reg_with_debug_info array -> t
  val without_debug_info : Reg.Set.t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val filter : (reg_with_debug_info -> bool) -> t -> t
  val fold : (reg_with_debug_info -> 'a -> 'a) -> t -> 'a -> 'a
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

(* Register availability sets are actually represented as maps, to allow
   lookup by [Reg.t] to be fast, and to statically forbid multiple
   [Debug_info.t option] values being associated with any given [Reg.t]
   value. *)
type avail_map = Debug_info.t option Reg.Map.t

module Make_set (C : sig
  val canonicalise : avail_map -> avail_map
end) = struct
  type t = avail_map

  let canonicalise = C.canonicalise

  let print ppf t =
    (* CR-someday mshinwell: Try to pass a proper [print_reg] here, or break
       the circular dependency so [print_reg] isn't needed. *)
    let print_reg = None in
    let elts ppf t =
      Reg.Map.iter (fun reg debug_info ->
          let rd = create_with_debug_info ~reg ~debug_info in
          Format.fprintf ppf "@ %a" (print ?print_reg) rd)
        t
    in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts t

  let invariant t =
    if !Clflags.ddebug_invariants then begin
      if not (Reg.Map.equal (Option.equal Debug_info.equal) t (canonicalise t))
      then begin
        Misc.fatal_errorf "Invariant broken:@ %a" print t
      end
    end

  let equal = Reg.Map.equal

  let empty = Reg.Map.empty
  let is_empty = Reg.Map.is_empty

  let of_list rds =
    let map =
      List.fold_left (fun map rd -> Reg.Map.add rd.reg rd.debug_info map)
        empty
        rds
    in
    if Reg.Map.cardinal map <> List.length rds then begin
      Misc.fatal_error "Cannot have multiple bindings for any given [Reg.t]"
    end;
    canonicalise map

  let of_array arr = of_list (Array.to_list arr)

  let without_debug_info regs =
    Reg.Set.fold (fun reg acc -> Reg.Map.add reg None acc)
      regs
      empty

  (* [inter] and [diff], by construction, preserve the canonical form used
     in [Canonical_set] below. *)
  let inter t1 t2 =
    let t =
      Reg.Map.merge (fun _reg debug_info_opt1 debug_info_opt2 ->
          match debug_info_opt1, debug_info_opt2 with
          | Some debug_info, None
          | None, Some debug_info -> Some debug_info
          | Some debug_info1, Some debug_info2 ->
            (* This check ensures that, for example, when we intersect two sets
               together (for example to handle a join point in a function's
               code) then we correctly lose debugging information when it is not
               valid on all paths.
          
               For example, suppose the value of a mutable variable x is copied
               into another variable y; then there is a conditional where on one
               branch x is assigned and on the other branch it is not. This
               means that on the former branch we have forgotten about y holding
               the value of x; but we have not on the latter. At the join point
               we must have forgotten the information. *)
            if Option.equal Debug_info.equal debug_info1 debug_info2 then
              Some debug_info1
            else
              None
          | None, None -> Misc.fatal_error "Bug in [Map.merge]")
        t1 t2
    in
    invariant t;
    t

  let diff t1 t2 =
    let t = Reg.Map.filter (fun reg _ -> not (Reg.Map.mem reg t2)) t1 in
    invariant t;
    t

  let map = Reg.Map.map
  let fold = Reg.Map.fold
  let filter = Reg.Map.filter

  let subset t1 t2 =
    let regs1 = Reg.Set.of_list (List.map fst (Reg.Map.bindings t1)) in
    let regs2 = Reg.Set.of_list (List.map fst (Reg.Map.bindings t2)) in
    Reg.Set.subset regs1 regs2

  let mem_reg t (reg : Reg.t) =
    Reg.Map.exists (fun reg' _debug_info -> reg.stamp = reg'.stamp) t

  let find_reg t (reg : Reg.t) =
    match Reg.Map.find reg t with
    | exception Not_found -> None
    | debug_info -> Some (create_with_debug_info ~reg ~debug_info)

  let filter_reg t (reg : Reg.t) =
    match find_reg t reg with
    | None -> empty
    | Some debug_info -> Reg.Map.singleton reg debug_info

  let forget_debug_info t =
    Reg.Map.fold (fun reg _debug_info acc -> Reg.Set.add reg acc)
      t
      Reg.Set.empty

  let made_unavailable_by_clobber t ~regs_clobbered ~register_class =
    let regs_clobbered = Reg.set_of_array regs_clobbered in
    let t =
      Reg.Map.filter (fun reg _debug_info ->
          Reg.Set.for_all (fun reg' ->
              not (regs_at_same_location reg' reg ~register_class))
            regs_clobbered)
        t
    in
    invariant t;
    t
end

module Availability_set = struct
  include Make_set (struct
    let canonicalise t = t
  end)

  let singleton rd = Reg.Map.singleton rd
  let add t rd = Reg.Map.add rd t

  let disjoint_union t1 t2 =
    Reg.Map.union (fun _reg _debug_info1 _debug_info2 ->
        Misc.fatal_errorf "[Reg.t] values in supplied availability sets are \
          not disjoint")
      t1 t2
end

module Canonical_set = struct
  let canonicalise set =
    let regs_by_var = V.Tbl.create 42 in
    Reg.Map.iter (fun reg ->
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
          Reg.Map.add reg result)
        regs_by_var
        Reg.Map.empty
    in
    if !Clflags.ddebug_invariants then begin
      assert (Reg.Map.subset result set)
    end;
    result

  include Make_set (struct
    let canonicalise = canonicalise
  end)

  let of_set set = canonicalise set

  let find_holding_value_of_variable t var =
    let t =
      Reg.Map.filter (fun rd ->
          match debug_info rd with
          | None -> false
          | Some debug_info ->
            Holds_value_of.equal (Debug_info.holds_value_of debug_info)
              (Var var))
        t
    in
    match Reg.Map.elements t with
    | [] -> None
    | [rd] -> Some rd
    | _ ->
      invariant t;
      assert false  (* The invariant must have been broken. *)
end

module With_canonical_set = struct
  type nonrec t = t

  let print ppf t : unit = print ?print_reg:None ppf t

  module Set = Canonical_set

  module Map = Map.Make (struct
    type nonrec t = t
    let compare = compare
  end)
end
