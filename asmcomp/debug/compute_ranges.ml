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

open Int_replace_polymorphic_compare

module L = Linearize

module Make (S : Compute_ranges_intf.S_functor) = struct
  module Index = S.Index
  module Key = S.Key
  module Subrange_state = S.Subrange_state
  module Subrange_info = S.Subrange_info
  module Range_info = S.Range_info

  module Available_subrange = struct
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    type t = {
      start_pos : L.label;
      start_pos_offset : int;
      end_pos : L.label;
      end_pos_offset : int;
      subrange_info : Subrange_info.t;
    }

    let create ~(start_insn : Linearize.instruction)
          ~start_pos ~start_pos_offset
          ~end_pos ~end_pos_offset
          ~subrange_info =
      match start_insn.desc with
      | Llabel _ ->
        { start_pos;
          start_pos_offset;
          end_pos;
          end_pos_offset;
          subrange_info;
        }
      | _ ->
        Misc.fatal_errorf "Available_subrange.create: bad [start_insn]: %a"
          Printlinear.instruction start_insn

    let start_pos t = t.start_pos
    let start_pos_offset t = t.start_pos_offset
    let end_pos t = t.end_pos
    let end_pos_offset t = t.end_pos_offset
  end

  module Available_range = struct
    type t = {
      mutable subranges : Available_subrange.t list;
      mutable min_pos : L.label option;
      mutable max_pos : L.label option;
      range_info : Range_info.t;
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
      (* This may seem dubious, but is correct by virtue of the way label
         counters are allocated (see [Linearize]) and the fact that, below,
         we go through the code from lowest (code) address to highest.  As
         such the label with the highest integer value should be the one with
         the highest address, and vice-versa.  (Note that we also exploit the
         ordering when constructing location lists, to ensure that they are
         sorted in increasing program counter order by start address.) *)
      assert (compare start_pos end_pos <= 0);
      begin match t.min_pos with
      | None -> t.min_pos <- Some start_pos
      | Some min_pos ->
        if compare start_pos min_pos < 0 then begin
          t.min_pos <- Some start_pos
        end
      end;
      begin
        match t.max_pos with
        | None -> t.max_pos <- Some end_pos
        | Some max_pos ->
          if compare end_pos max_pos > 0 then begin
            t.max_pos <- Some end_pos
          end
      end;
      t.subranges <- subrange::t.subranges

    let extremities t =
      (* We ignore any [end_pos_offset]s here; should be ok. *)
      match t.min_pos, t.max_pos with
      | Some min, Some max -> min, max
      | Some _, None | None, Some _ -> assert false
      | None, None ->
        Misc.fatal_error "Available_ranges.extremities on empty range"

    let iter t ~f =
      List.iter (fun available_subrange -> f ~available_subrange)
        t.subranges

    let fold t ~init ~f =
      List.fold_left (fun acc available_subrange -> f acc ~available_subrange)
        init
        t.subranges
  end

  type t = {
    ranges : Available_range.t Index.Tbl.t;
  }

  let find t ~var =
    match Index.Tbl.find t.ranges var with
    | exception Not_found -> None
    | range -> Some range

  let iter t ~f =
    Index.Tbl.iter (fun var range -> f var range)
      t.ranges

  let fold t ~init ~f =
    Index.Tbl.fold (fun var range acc -> f acc var range)
      t.ranges
      init

  module KM = S.Key.Map
  module KS = S.Key.Set

  (* The output of this pass satisfies the DWARF specification (e.g. DWARF-4
     spec. section 2.6.2, page 30) in the sense that starting addresses of
     ranges are treated as inclusive and ending addresses as exclusive.

     Imagine that, for a given [key], the program counter (PC) is exactly at the
     start of [insn]; that instruction has not yet been executed.  Assume
     a immediately-previous instruction exists called [prev_insn].  Intuitively,
     this function calculates which available subranges are to start and stop at
     that point, but these notions are subtle.

     There are eight cases, referenced in the code below.

     1. First four cases: [key] is currently unavailable, i.e. it is not a
     member of [prev_insn.available_across].

     (a) [key] is not in [S.available_before insn] and neither is it in
         [S.available_across insn].  There is nothing to do.

     (b) [key] is not in [S.available_before insn] but it is in
         [S.available_across insn].  This cannot happen---see the
         comment at the top of available_regs.ml.

     (c) [key] is in [S.available_before insn] but it is not in
         [S.available_across insn].  A new range is created with the starting
         position being the first machine instruction of [insn] and the ending
         position being the next machine address after that.

     (d) [key] is in [S.available_across insn], which means (as for (b) above)
         it is in [S.available_before insn]. A new range is created with the
         starting position being the first machine instruction of [insn] and
         left open.

     2. Second four cases: [key] is already available, i.e. a member of
     [S.available_across prev_insn].

     (a) [key] is not in [S.available_before insn] and neither is it in
         [S.available_across insn].  The range endpoint is given as the address
         of the first machine instruction of [insn].  Since endpoint bounds are
         exclusive (see above) then [key] will not be shown as available when
         the debugger is standing on [insn].

     (b) [key] is not in [S.available_before insn] but it is in
         [S.available_across insn].  This cannot happen---see the
         comment at the top of available_regs.ml.

     (c) [key] is in [S.available_before insn] but it is not in
         [S.available_across insn].  This will only happen for operation
         (i.e. [Lop]) instructions, for example calls, or allocations.  To give
         a good user experience it is necessary to show availability when
         the debugger is standing on the very first instruction of the
         operation but not thereafter.  As such we terminate the range one
         byte beyond the first machine instruction of [insn].

     (d) [key] is in [S.available_across insn], which means (as for (b) above)
         it is in [S.available_before insn].  The existing range remains open.
  *)

  let opt_available_across insn_opt =
    match insn_opt with
    | None -> KS.empty
    | Some insn -> insn.available_across

  type action =
    | Open_one_byte_range
    | Open_range
    | Close_range
    | Close_range_one_byte_after

  let actions_at_instruction ~(insn : L.instruction)
        ~(prev_insn : L.instruction option) : (Key.t * action) list =
    assert (KS.subset (S.available_across insn) (S.available_before insn));
    let case_1c =
      KS.diff (S.available_before insn)
        (KS.union (S.available_across prev_insn) (S.available_across insn))
    in
    let case_1d =
      KS.diff (KS.inter (S.available_before insn) (S.available_across insn))
        (S.available_across prev_insn)
    in
    let case_2a =
      KS.diff (S.available_across prev_insn)
        (KS.union (S.available_before insn) (S.available_across insn))
    in
    let case_2c =
      KS.diff
        (KS.inter (S.available_across prev_insn) (S.available_before insn))
        (S.available_across insn)
    in
    let handle case action result =
      KS.fold (fun key result -> (key, action) :: result) case result
    in
    let actions =
      []
      |> handle case_1c Open_one_byte_range
      |> handle case_1d Open_range
      |> handle case_2a Close_range
      |> handle case_2c Close_range_one_byte_after
    in
    let eligible_for_restart =
      if S.must_restart_ranges_upon_any_change () then
        KS.inter (S.available_across prev_insn) (S.available_before insn)
      else
        KS.empty
    in
    actions, eligible_for_restart

  let rec process_instruction t (fundecl : L.fundecl)
        ~(first_insn : L.instruction) ~(insn : L.instruction)
        ~(prev_insn : L.instruction)
        ~open_subrange_start_insns ~subrange_state =
    let actions = actions_at_instruction ~insn ~prev_insn in
    let first_insn = ref first_insn in
    let prev_insn = ref prev_insn in
    let insert_insn ~new_insn =
      assert (new_insn.next == insn);
      (* (Note that by virtue of [Lprologue], we can insert labels prior to the
         first assembly instruction of the function.) *)
      begin match !prev_insn with
      | None -> first_insn := new_insn
      | Some prev_insn ->
        assert (prev_insn.L.next == insn);
        prev_insn.next <- new_insn
      end;
      prev_insn := Some new_insn
    in
    (* Note that we can't reuse an existing label in the code since we rely on
       the ordering of range-related labels. *)
    let label = Cmm.new_label () in
    let used_label = ref false in
    let open_range ... =

    in
    let close_range key ~open_subrange_start_insns ~end_pos_offset =
      match KM.find key open_subrange_start_insns with
      | exception Not_found -> ()
      | start_pos, start_pos_offset, start_insn ->
        match Range_info.create fundecl key ~start_insn with
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
          let subrange_info = Subrange_info.create key subrange_state in
          let subrange =
            S.create_subrange ~fundecl ~key ~start_insn
              ~start_pos ~start_pos_offset
              ~end_pos ~end_pos_offset
              ~subrange_info
          in
          Available_range.add_subrange range ~subrange
    in
    List.fold_left (fun open_subrange_start_insns (key, (action : action)) ->
        match action with
        | Open_one_byte_range ->
          open_range key;
          close_range key ~end_pos_offset:1
        | Open_range -> open_range key
        | Close_range -> close_range key ~end_pos_offset:0
        | Close_range_one_byte_after -> close_range key ~end_pos_offset:1)
      open_subrange_start_insns
      actions;
(*
        let end_pos = label in
        used_label := true;
        let end_pos_offset = S.end_pos_offset ~prev_insn:!prev_insn ~key in
*)
    let label_insn : L.instruction =
      { desc = Llabel label;
        next = insn;
        arg = [| |];
        res = [| |];
        dbg = insn.dbg;
        live = insn.live;
        available_before = insn.available_before;
        phantom_available_before = insn.phantom_available_before;
        available_across = insn.available_before;
      }
    in
    let open_subrange_start_insns =
      let open_subrange_start_insns =
        (KM.filter (fun key _start_insn -> not (KS.mem key deaths))
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
        Make_ranges.process_instructions t fundecl
          ~first_insn:fundecl.fun_body
      in
      t, { fundecl with fun_body = first_insn; }
end
