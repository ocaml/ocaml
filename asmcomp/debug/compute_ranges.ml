(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

module Make (S : sig
  module Key : sig
    type t

    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t

    val assert_valid : t -> unit
  end

  module Index : Identifiable.S

  module Subrange_state : Subrange_state_intf

  val available_before : L.instruction -> Key.Set.t

  val range_info
     : fundecl:L.fundecl
    -> key:Key.t
    -> start_insn:L.instruction
    -> (Backend_var.t * type_info * is_parameter) option

  val create_subrange
     : fundecl:L.fundecl
    -> key:Key.t
    -> start_pos:L.label
    -> start_insn:L.instruction
    -> end_pos:L.label
    -> end_pos_offset:int option
    -> Subrange_state.t
    -> Available_subrange.t

  val end_pos_offset
     : prev_insn:L.instruction option
    -> key:Key.t
    -> int option

  val maybe_restart_ranges : bool
end) = struct
  module Subrange_info = S.Subrange_info
  module Subrange_state = S.Subrange_state

  module Available_subrange : sig
    type t

    type 'a location =
      | Reg of Reg.t * 'a
      | Phantom

    val create
       : reg:Reg.t
      -> start_insn:L.instruction
      -> start_pos:L.label
      -> end_pos:L.label
      -> end_pos_offset:int option
      -> subrange_state:Subrange_state.t
      -> t

    val create_phantom
       : start_pos:Linearize.label
      -> end_pos:Linearize.label
      -> t

    val start_pos : t -> L.label
    val end_pos : t -> L.label
    val end_pos_offset : t -> int option
    val location : t -> unit location
  end = struct
    type t = {
      start_pos : L.label;
      (* CR mshinwell: we need to check exactly what happens with function
         epilogues, including returns in the middle of functions. *)
      end_pos : L.label;
      end_pos_offset : int option;
      subrange_info : Subrange_info.t;
    }

    let create ~(reg : Reg.t) ~(start_insn : Linearize.instruction)
          ~start_pos ~end_pos ~end_pos_offset ~subrange_state =
      let subrange_info = (* XXX Reg.t seems wrong here, might not have it *)
        Subrange_info.create ~reg ~start_insn ...
      in
      match start_insn.desc with
      | Llabel _ ->
        { start_pos;
          end_pos;
          end_pos_offset;
          subrange_info;
        }
      | _ -> failwith "Available_subrange.create"

    let start_pos t = t.start_pos
    let end_pos t = t.end_pos
    let end_pos_offset t = t.end_pos_offset
  end

  module Available_range : sig
    type t

    val create : S.range_info -> t

    val info : t -> S.range_info

    val add_subrange : t -> subrange:Available_subrange.t -> unit
    val extremities : t -> L.label * L.label

    val iter
       : t
      -> f:(available_subrange:Available_subrange.t -> unit)
      -> unit

    val fold
       : t
      -> init:'a
      -> f:('a -> available_subrange:Available_subrange.t -> 'a)
      -> 'a
  end = struct
    type t = {
      mutable subranges : Available_subrange.t list;
      mutable min_pos : L.label option;
      mutable max_pos : L.label option;
      type_info : type_info;
      is_parameter : is_parameter;
      var_location : Debuginfo.t;
    }

    let create range_info =
      { subranges = [];
        min_pos = None;
        max_pos = None;
        range_info;
      }

    let range_info t = t.range_info

    let add_subrange t ~subrange =
      let start_pos = Available_subrange.start_pos subrange in
      let end_pos = Available_subrange.end_pos subrange in
      (* This is dubious, but should be correct by virtue of the way label
         counters are allocated (see linearize.ml) and the fact that, below,
         we go through the code from lowest (code) address to highest.  As
         such the label with the highest integer value should be the one with
         the highest address, and vice-versa.  (Note that we also exploit the
         ordering when constructing location lists, to ensure that they are
         sorted in increasing program counter order by start address.
         However by that stage [Coalesce_labels] has run.) *)
      assert (compare start_pos end_pos <= 0);
      begin
        match t.min_pos with
        | None -> t.min_pos <- Some start_pos
        | Some min_pos ->
          if compare start_pos min_pos < 0 then t.min_pos <- Some start_pos
      end;
      begin
        match t.max_pos with
        | None -> t.max_pos <- Some end_pos
        | Some max_pos ->
          if compare (`At_label end_pos) (`At_label max_pos) > 0 then
            t.max_pos <- Some end_pos
      end;
      t.subranges <- subrange::t.subranges

    let extremities t =
      (* We ignore any [end_pos_offsets] here; should be ok. *)
      match t.min_pos, t.max_pos with
      | Some min, Some max -> min, max
      | Some _, None | None, Some _ -> assert false
      | None, None -> failwith "Available_ranges.extremities on empty range"

    let iter t ~f =
      List.iter (fun available_subrange -> f ~available_subrange)
        t.subranges

    let fold t ~init ~f =
      List.fold_left (fun acc available_subrange -> f acc ~available_subrange)
        init
        t.subranges
  end

  type t = {
    ranges : Available_range.t Backend_var.Tbl.t;
  }

  let find t ~var =
    match Backend_var.Tbl.find t.ranges var with
    | exception Not_found -> None
    | range -> Some range

  let iter t ~f =
    Backend_var.Tbl.iter (fun var range -> f var range)
      t.ranges

  let fold t ~init ~f =
    Backend_var.Tbl.fold (fun var range acc -> f acc var range)
      t.ranges
      init

  module KM = S.Key.Map
  module KS = S.Key.Set

  (* Imagine that the program counter is exactly at the start of [insn]; it has
     not yet been executed.  This function calculates which available subranges
     are to start at that point, and which are to stop.  [prev_insn] is the
     instruction immediately prior to [insn], if such exists. *)
  let births_and_deaths ~(insn : L.instruction)
        ~(prev_insn : L.instruction option) =
    (* Notes for register available ranges (not relevant for lexical blocks):

       Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from
       the CFA, not from the stack pointer.

       We avoid generating ranges that overlap, since this confuses lldb.
       This pass may generate ranges that are the same as other ranges,
       but those are deduped in [Dwarf].
    *)
    let proto_births =
      match prev_insn with
      | None -> S.available_before insn
      | Some prev_insn ->
        KS.diff (S.available_before insn) (S.available_before prev_insn)
    in
    let proto_deaths =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn ->
        KS.diff (S.available_before prev_insn) (S.available_before insn)
    in
    let restart_ranges =
      if not S.maybe_restart_ranges then false
      else
        match !Clflags.debug_full with
        | Some Gdb -> false
        | Some Lldb ->
          (* Work at OCamlPro suggested that lldb requires ranges to be
             completely restarted in the event of any change. *)
          KS.cardinal proto_births <> 0 || KS.cardinal proto_deaths <> 0
        | None -> Misc.fatal_error "Shouldn't be here without [debug_full]"
    in
    let births =
      match prev_insn with
      | None -> S.available_before insn
      | Some _prev_insn ->
        if not restart_ranges then
          proto_births
        else
          S.available_before insn
    in
    let deaths =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn ->
        if not restart_ranges then
          proto_deaths
        else
          S.available_before prev_insn
    in
    births, deaths

  let rec process_instruction t ~fundecl ~first_insn ~(insn : L.instruction)
        ~(prev_insn : L.instruction) ~open_subrange_start_insns
        ~subrange_state =
    let births, deaths = births_and_deaths ~insn ~prev_insn in
    let first_insn = ref first_insn in
    let prev_insn = ref prev_insn in
    let insert_insn ~new_insn =
      assert (new_insn.next == insn);
      (* (Note that by virtue of [Lprologue], we can insert labels prior
         to the first assembly instruction of the function.) *)
      begin match !prev_insn with
      | None -> first_insn := new_insn
      | Some prev_insn ->
        assert (prev_insn.L.next == insn);
        prev_insn.next <- new_insn
      end;
      prev_insn := Some new_insn
    in
    (* Note that we can't reuse an existing label in the code since we rely
       on the ordering of range-related labels. *)
    let label = Cmm.new_label () in
    let used_label = ref false in
    (* As a result of the code above to restart subranges, we may have
       a register occurring in both [births] and [deaths]; and we would
       like the register to have an open subrange from this point.  It
       follows that we should process deaths before births. *)
    KS.fold (fun (key : S.Key.t) () ->
        S.Key.assert_valid key;
        let start_pos, start_insn =
          try KM.find key open_subrange_start_insns
          with Not_found -> assert false
        in
        let end_pos = label in
        used_label := true;
        let end_pos_offset = S.end_pos_offset ~prev_insn:!prev_insn ~key in
        match S.range_info ~fundecl ~key ~start_insn with
        | None -> ()
        | Some (index, range_info) ->
          let range =
            match Index.Tbl.find t.ranges index with
            | range -> range
            | exception Not_found ->
              let range = Available_range.create range_info in
              Index.Tbl.add t.ranges index range;
              range
          in
          let subrange =
            S.create_subrange ~fundecl ~key ~start_pos ~start_insn ~end_pos
              ~end_pos_offset ~subrange_state
          in
          Available_range.add_subrange range ~subrange)
      deaths
      ();
    let label_insn : L.instruction =
      { desc = Llabel label;
        next = insn;
        arg = [| |];
        res = [| |];
        dbg = Debuginfo.none;
        live = Reg.Set.empty;
        available_before = insn.available_before;
        phantom_available_before = insn.phantom_available_before;
        available_across = None;
      }
    in
    let open_subrange_start_insns =
      let open_subrange_start_insns =
        (KM.filter (fun reg _start_insn -> not (KS.mem reg deaths))
          open_subrange_start_insns)
      in
      KS.fold (fun key open_subrange_start_insns ->
          used_label := true;
          KM.add key (label, label_insn) open_subrange_start_insns)
        births
        open_subrange_start_insns
    in
    begin if !used_label then
      insert_insn ~new_insn:label_insn
    end;
    let first_insn = !first_insn in
    match insn.desc with
    | Lend -> first_insn
    | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Llabel _
    | Lbranch _ | Lcondbranch _ | Lcondbranch3 _ | Lswitch _
    | Lsetuptrap _ | Lpushtrap | Lpoptrap | Lraise _ ->
      let subrange_state =
        Subrange_state.advance_over_instruction subrange_state insn
      in
      process_instruction t ~fundecl ~first_insn ~insn:insn.next
        ~prev_insn:(Some insn) ~open_subrange_start_insns ~subrange_state

  let process_instructions t ~fundecl ~first_insn =
    let subrange_state = Subrange_state.create () in
    process_instruction t ~fundecl ~first_insn ~insn:first_insn
      ~prev_insn:None ~open_subrange_start_insns:KM.empty ~subrange_state

  let create (fundecl : L.fundecl) =
    if not !Clflags.debug then
      let t =
        { ranges = Index.Tbl.create 1;
        }
      in
      t, fundecl
    else
      let t = { ranges = S.Index.Tbl.create 42; } in
      let first_insn =
        Make_ranges.process_instructions t ~fundecl
          ~first_insn:fundecl.fun_body
      in
      t, { fundecl with fun_body = first_insn; }
end
