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

  let create ~holds_value_of ~part_of_value ~num_parts_of_value
        is_parameter ~provenance =
    assert (num_parts_of_value >= 1);
    assert (part_of_value >= 0 && part_of_value < num_parts_of_value);
    { holds_value_of;
      part_of_value;
      num_parts_of_value;
      is_parameter;
      provenance;
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
  let c = Stdlib.compare reg1.stamp reg2.stamp in
  if c <> 0 then c
  else Option.compare Debug_info.compare debug_info1 debug_info2

let create_with_debug_info reg debug_info =
  { reg;
    debug_info;
  }

let reg t = t.reg
let location t = t.reg.loc
let debug_info t = t.debug_info

(* We use maps to allow lookup by [Reg.t] to be fast, and to statically forbid
   multiple [Debug_info.t option] values being associated with any given [Reg.t]
   value.  Using maps rather than sets also simplifies the code in
   [Available_regs]. *)
module Availability_map = struct
  type t = Debug_info.t option Reg.Map.t

  let print ppf t =
    (* CR-someday mshinwell: Try to pass a proper [print_reg] here, or break
       the circular dependency so [print_reg] isn't needed. *)
    let print_reg = None in
    let elts ppf t =
      Reg.Map.iter (fun reg debug_info ->
          let rd = create_with_debug_info reg debug_info in
          Format.fprintf ppf "@ %a" (print ?print_reg) rd)
        t
    in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts t

  let equal t1 t2 = Reg.Map.equal (Option.equal Debug_info.equal) t1 t2

  let empty = Reg.Map.empty

  let singleton reg debug_info = Reg.Map.singleton reg debug_info

  let add_or_replace t reg debug_info = Reg.Map.add reg debug_info t

  let of_assoc_array arr =
    Array.fold_left (fun t (reg, debug_info) ->
        add_or_replace t reg debug_info)
      empty
      arr

  let mem t reg = Reg.Map.mem reg t

  let find t reg =
    match Reg.Map.find reg t with
    | exception Not_found -> None
    | debug_info -> Some debug_info

  let keys t =
    Reg.Set.of_list (List.map fst (Reg.Map.bindings t))

  let map t ~f = Reg.Map.map f t

  let filter t ~f = Reg.Map.filter (fun reg _debug_info -> f reg) t

  let diff_domain t1 t2 =
    Reg.Map.filter (fun reg _ -> not (Reg.Map.mem reg t2)) t1

  let inter t1 t2 =
    Reg.Map.merge (fun _reg debug_info_opt1 debug_info_opt2 ->
        match debug_info_opt1, debug_info_opt2 with
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
        | Some debug_info, None
        | None, Some debug_info ->
          (* The register only occurred in one of [t1] and [t2]. *)
          Some debug_info
        | None, None -> Misc.fatal_error "Bug in [Map.merge]")
      t1 t2

  let disjoint_union t1 t2 =
    Reg.Map.union (fun _reg _debug_info1 _debug_info2 ->
        Misc.fatal_errorf "[Reg.t] keys in supplied availability maps are \
          not disjoint")
      t1 t2

  let made_unavailable_by_clobber t ~regs_clobbered ~register_class =
    let regs_clobbered = Reg.set_of_array regs_clobbered in
    Reg.Map.filter (fun reg _debug_info ->
        Reg.Set.exists (fun reg' ->
            Reg.at_same_location reg' reg ~register_class)
          regs_clobbered)
      t

  let subset t1 t2 =
    let regs1 = Reg.Set.of_list (List.map fst (Reg.Map.bindings t1)) in
    let regs2 = Reg.Set.of_list (List.map fst (Reg.Map.bindings t2)) in
    Reg.Set.subset regs1 regs2
end

module Canonical_availability_map = struct
  type t = Availability_map.t

  let print = Availability_map.print

  let empty = Reg.Map.empty

  let create (map : Availability_map.t) =
    let regs_by_var : reg_with_debug_info V.Tbl.t = V.Tbl.create 42 in
    Reg.Map.iter (fun reg debug_info ->
        let reg = create_with_debug_info reg debug_info in
        match debug_info with
        | None -> ()
        | Some debug_info ->
          match Debug_info.holds_value_of debug_info with
          | Var name ->
            begin match V.Tbl.find regs_by_var name with
            | exception Not_found -> V.Tbl.add regs_by_var name reg
            | (reg' : reg_with_debug_info) ->
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
      map;
    let result =
      V.Tbl.fold (fun _var reg result ->
          Reg.Map.add reg.reg reg.debug_info result)
        regs_by_var
        Reg.Map.empty
    in
    if !Clflags.ddebug_invariants then begin
      assert (Availability_map.subset result map)
    end;
    result

  let of_list rds =
    let regs = Reg.Set.of_list (List.map reg rds) in
    if Reg.Set.cardinal regs <> List.length rds then begin
      Misc.fatal_error "More than one binding with the same [Reg.t]"
    end;
    let avail_map =
      List.fold_left (fun t rd ->
          Availability_map.add_or_replace t (reg rd) (debug_info rd))
        Availability_map.empty
        rds
    in
    create avail_map

  let is_empty = Reg.Map.is_empty

  let invariant t =
    if !Clflags.ddebug_invariants then begin
      if not (Reg.Map.equal (Option.equal Debug_info.equal) t (create t))
      then begin
        Misc.fatal_errorf "Invariant broken:@ %a" print t
      end
    end

  (* [diff] and [inter], by construction, preserve the canonical form. *)

  let diff t1 t2 =
    let t =
      Reg.Map.filter (fun reg debug_info_t1 ->
          let occurs_in_t2 =
            match Reg.Map.find reg t2 with
            | exception Not_found -> false
            | debug_info_t2 ->
              Option.equal Debug_info.equal debug_info_t1 debug_info_t2
          in
          not occurs_in_t2)
        t1
    in
    invariant t;
    t

  let inter t1 t2 =
    let t = Availability_map.inter t1 t2 in
    invariant t;
    t

  let fold f t init =
    Reg.Map.fold (fun reg debug_info acc ->
        let rd = create_with_debug_info reg debug_info in
        f rd acc)
      t
      init

  let find_holding_value_of_variable t var =
    let t =
      Reg.Map.filter (fun _reg debug_info ->
          match debug_info with
          | None -> false
          | Some debug_info ->
            Holds_value_of.equal (Debug_info.holds_value_of debug_info)
              (Var var))
        t
    in
    match Reg.Map.bindings t with
    | [] -> None
    | [reg, debug_info] -> Some (create_with_debug_info reg debug_info)
    | _ ->
      invariant t;
      assert false  (* The invariant must have been broken. *)
end

module For_compute_ranges = struct
  type nonrec t = t

  let print ppf t : unit = print ?print_reg:None ppf t

  module Set = Canonical_availability_map

  module Map = Map.Make (struct
    type nonrec t = t
    let compare = compare
  end)
end
