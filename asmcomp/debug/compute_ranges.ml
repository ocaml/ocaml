(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module L = Linearize

module Make (S : Compute_ranges_intf.S_functor) = struct
  module Subrange_state = S.Subrange_state
  module Subrange_info = S.Subrange_info
  module Range_info = S.Range_info

  let rewrite_label env label =
    match Numbers.Int.Map.find label env with
    | exception Not_found -> label
    | label -> label

  module Subrange = struct
    (* CR-soon mshinwell: Check that function epilogues, including returns
       in the middle of functions, work ok in the debugger. *)
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
        Misc.fatal_errorf "Subrange.create: bad [start_insn]: %a"
          Printlinear.instr start_insn

    let start_pos t = t.start_pos
    let start_pos_offset t = t.start_pos_offset
    let end_pos t = t.end_pos
    let end_pos_offset t = t.end_pos_offset
    let info t = t.subrange_info

    let rewrite_labels t ~env =
      let start_pos = rewrite_label env t.start_pos in
      let end_pos = rewrite_label env t.end_pos in
      if start_pos = end_pos
        && t.start_pos_offset = 0
        && t.end_pos_offset = 0
      then None
      else
        Some {
          t with
          start_pos;
          end_pos;
        }
  end

  module Range = struct
    type t = {
      mutable subranges : Subrange.t list;
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

    let info t = t.range_info

    let add_subrange t ~subrange =
      let start_pos = Subrange.start_pos subrange in
      let end_pos = Subrange.end_pos subrange in
      (* This may seem dubious, but is correct by virtue of the way label
         counters are allocated sequentially and the fact that, below,
         we go through the code from lowest (code) address to highest.  As
         such the label with the highest integer value should be the one with
         the highest address, and vice-versa.  (Note that we also exploit the
         ordering when constructing DWARF-4 location lists, to ensure that
         they are sorted in increasing program counter order by start
         address.) *)
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
        Misc.fatal_error "Ranges.extremities on empty range"

    let lowest_address t = t.min_pos

    let fold t ~init ~f =
      List.fold_left f init t.subranges

    let no_subranges t =
      match t.subranges with
      | [] -> true
      | _ -> false

    let rewrite_labels_and_remove_empty_subranges t ~env =
      let subranges =
        List.filter_map (fun subrange ->
            Subrange.rewrite_labels subrange ~env)
          t.subranges
      in
      match subranges with
      | [] ->
        { t with
          subranges;
          min_pos = None;
          max_pos = None;
        }
      | subranges ->
        let min_pos = Misc.Stdlib.Option.map (rewrite_label env) t.min_pos in
        let max_pos = Misc.Stdlib.Option.map (rewrite_label env) t.max_pos in
        { t with
          subranges;
          min_pos;
          max_pos;
        }
  end

  type t = {
    ranges : Range.t S.Index.Tbl.t;
  }

  module KM = S.Key.Map
  module KS = S.Key.Set

  (* Whilst this pass is not DWARF-specific, the output of this pass uses
     the conventions of the DWARF specification (e.g. DWARF-4 spec.
     section 2.6.2, page 30) in the sense that starting addresses of ranges
     are treated as inclusive and ending addresses as exclusive.

     Imagine that, for a given [key], the program counter (PC) is exactly at the
     start of [insn]; that instruction has not yet been executed.  Assume
     a immediately-previous instruction exists called [prev_insn].  Intuitively,
     this function calculates which available subranges are to start and stop at
     that point, but these notions are subtle.

     There are eight cases, referenced in the code below.

     1. First four cases: [key] is currently unavailable, i.e. it is not a
     member of (roughly speaking) [S.available_across prev_insn].

     (a) [key] is not in [S.available_before insn] and neither is it in
         [S.available_across insn].  There is nothing to do.

     (b) [key] is not in [S.available_before insn] but it is in
         [S.available_across insn].  A new range is created with the starting
         position being one byte after the first machine instruction of [insn]
         and left open.

         It might seem like this case 1 (b) is impossible, likewise for 2 (b)
         below, since "available across" should always be a subset of
         "available before".  However this does not hold in general: see the
         comment in available_ranges_vars.ml.

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
         [S.available_across insn].  The range endpoint is given as the address
         of the first machine instruction of [insn]; and a new range is opened
         in the same way as for case 1 (b), above.

     (c) [key] is in [S.available_before insn] but it is not in
         [S.available_across insn]. This will only happen when calculating
         variables' available ranges for operation (i.e. [Lop]) instructions
         (for example calls or allocations). To give a good user experience it
         is necessary to show availability when the debugger is standing on the
         very first instruction of the operation but not thereafter. As such we
         terminate the range one byte beyond the first machine instruction of
         [insn].

     (d) [key] is in [S.available_across insn], which means (as for (b) above)
         it is in [S.available_before insn].  The existing range remains open.
  *)

  type action =
    | Open_one_byte_subrange
    | Open_subrange
    | Open_subrange_one_byte_after
    | Close_subrange
    | Close_subrange_one_byte_after

  (* CR mshinwell: Move to [Clflags] *)
  let check_invariants = ref true

  let actions_at_instruction ~(insn : L.instruction)
        ~(prev_insn : L.instruction option) =
    let available_before = S.available_before insn in
    let available_across = S.available_across insn in
    let opt_available_across_prev_insn =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn -> S.available_across prev_insn
    in
    let case_1b =
      KS.diff available_across
        (KS.union opt_available_across_prev_insn available_before)
    in
    let case_1c =
      KS.diff available_before
        (KS.union opt_available_across_prev_insn available_across)
    in
    let case_1d =
      KS.diff (KS.inter available_before available_across)
        opt_available_across_prev_insn
    in
    let case_2a =
      KS.diff opt_available_across_prev_insn
        (KS.union available_before available_across)
    in
    let case_2b =
      KS.inter opt_available_across_prev_insn
        (KS.diff available_across available_before)
    in
    let case_2c =
      KS.diff
        (KS.inter opt_available_across_prev_insn available_before)
        available_across
    in
    let handle case action result =
      (* We use [K.all_parents] here to circumvent a potential performance
         problem.  In the case of lexical blocks, there may be long chains
         of blocks and their parents, yet the innermost block determines the
         rest of the chain.  As such [S] (which comes from
         lexical_block_ranges.ml) only needs to use the innermost blocks in
         the "available before" sets, keeping things fast---but we still
         populate ranges for all parent blocks, thus avoiding any
         post-processing, by using [K.all_parents] here. *)
      KS.fold (fun key result ->
          List.fold_left (fun result key ->
              (key, action) :: result)
            result
            (key :: (S.Key.all_parents key)))
        case
        result
    in
    let actions =
      (* Ranges must be closed before they are opened---otherwise, when a
         variable moves between registers at a range boundary, we might end up
         with no open range for that variable.  Note that the pipeline below
         constructs the [actions] list in reverse order---later functions in
         the pipeline produce actions nearer the head of the list. *)
      []
      |> handle case_1b Open_subrange_one_byte_after
      |> handle case_1c Open_one_byte_subrange
      |> handle case_1d Open_subrange
      |> handle case_2a Close_subrange
      |> handle case_2b Open_subrange_one_byte_after
      |> handle case_2b Close_subrange
      |> handle case_2c Close_subrange_one_byte_after
    in
    let must_restart =
      if S.must_restart_ranges_upon_any_change () then
        KS.inter opt_available_across_prev_insn available_before
      else
        KS.empty
    in
    actions, must_restart

  let rec process_instruction t (fundecl : L.fundecl)
        ~(first_insn : L.instruction) ~(insn : L.instruction)
        ~(prev_insn : L.instruction option)
        ~open_subranges ~subrange_state =
    let first_insn = ref first_insn in
    let prev_insn = ref prev_insn in
    let insert_insn ~(new_insn : L.instruction) =
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
    let label_insn : L.instruction =
      { desc = Llabel label;
        next = insn;
        arg = [| |];
        res = [| |];
        dbg = Debuginfo.none;
        live = insn.live;
      }
    in
    let used_label = ref false in
    let open_subrange key ~start_pos_offset ~open_subranges =
      used_label := true;
      KM.add key (label, start_pos_offset, label_insn) open_subranges
    in
    let close_subrange key ~end_pos_offset ~open_subranges =
      match KM.find key open_subranges with
      | exception Not_found -> open_subranges
      | start_pos, start_pos_offset, start_insn ->
        let open_subranges = KM.remove key open_subranges in
        match Range_info.create fundecl key ~start_insn with
        | None -> open_subranges
        | Some (index, range_info) ->
          let range =
            match S.Index.Tbl.find t.ranges index with
            | range -> range
            | exception Not_found ->
              let range = Range.create range_info in
              S.Index.Tbl.add t.ranges index range;
              range
          in
          used_label := true;
          let subrange_info = Subrange_info.create key subrange_state in
          let subrange =
            Subrange.create ~start_insn
              ~start_pos ~start_pos_offset
              ~end_pos:label ~end_pos_offset
              ~subrange_info
          in
          Range.add_subrange range ~subrange;
          open_subranges
    in
    let actions, must_restart =
      actions_at_instruction ~insn ~prev_insn:!prev_insn
    in
    let open_subranges =
      KS.fold (fun key open_subranges ->
          let open_subranges =
            close_subrange key ~end_pos_offset:0 ~open_subranges
          in
          open_subrange key ~start_pos_offset:0 ~open_subranges)
        must_restart
        open_subranges
    in
    let open_subranges =
      List.fold_left (fun open_subranges (key, (action : action)) ->
          match action with
          | Open_one_byte_subrange ->
            let open_subranges =
              open_subrange key ~start_pos_offset:0 ~open_subranges
            in
            close_subrange key ~end_pos_offset:1 ~open_subranges
          | Open_subrange ->
            open_subrange key ~start_pos_offset:0 ~open_subranges
          | Open_subrange_one_byte_after ->
            open_subrange key ~start_pos_offset:1 ~open_subranges
          | Close_subrange ->
            close_subrange key ~end_pos_offset:0 ~open_subranges
          | Close_subrange_one_byte_after ->
            close_subrange key ~end_pos_offset:1 ~open_subranges)
        open_subranges
        actions
    in
    let open_subranges =
      match insn.desc with
      | Lend ->
        let open_subranges =
          KM.fold (fun key _ open_subranges ->
              close_subrange key ~end_pos_offset:0 ~open_subranges)
            open_subranges
            open_subranges
        in
        assert (KM.is_empty open_subranges);
        open_subranges
      | _ -> open_subranges
    in
    begin if !used_label then
      insert_insn ~new_insn:label_insn
    end;
    if !check_invariants then begin
      let open_subranges =
        KS.of_list (
          List.map (fun (key, _datum) -> key) (KM.bindings open_subranges))
      in
      let should_be_open = S.available_across insn in
      let not_open_but_should_be = KS.diff should_be_open open_subranges in
      if not (KS.is_empty not_open_but_should_be) then begin
        Misc.fatal_errorf "%s: ranges for %a are not open across the following \
            instruction:\n%a\navailable_across:@ %a\nopen_subranges: %a"
          fundecl.fun_name
          KS.print not_open_but_should_be
          Printlinear.instr { insn with L.next = L.end_instr; }
          KS.print should_be_open
          KS.print open_subranges
      end
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
      process_instruction t fundecl ~first_insn ~insn:insn.next
        ~prev_insn:(Some insn) ~open_subranges ~subrange_state

  let process_instructions t fundecl ~first_insn =
    let subrange_state = Subrange_state.create () in
    process_instruction t fundecl ~first_insn ~insn:first_insn
      ~prev_insn:None ~open_subranges:KM.empty ~subrange_state

  let all_indexes t =
    S.Index.Set.of_list (List.map fst (S.Index.Tbl.to_list t.ranges))

  let empty =
    { ranges = S.Index.Tbl.create 1;
    }

  let create (fundecl : L.fundecl) =
    let t =
      { ranges = S.Index.Tbl.create 42;
      }
    in
    let first_insn =
      process_instructions t fundecl ~first_insn:fundecl.fun_body
    in
    let fundecl : L.fundecl =
      { fundecl with fun_body = first_insn; }
    in
    t, fundecl

  let iter t ~f =
    S.Index.Tbl.iter (fun index range -> f index range)
      t.ranges

  let fold t ~init ~f =
    S.Index.Tbl.fold (fun index range acc -> f acc index range)
      t.ranges
      init

  let find t index = S.Index.Tbl.find t.ranges index

  let rewrite_labels_and_remove_empty_subranges_and_ranges t ~env =
    let empty_ranges = ref S.Index.Set.empty in
    let ranges =
      S.Index.Tbl.mapi t.ranges (fun index range ->
          let range =
            Range.rewrite_labels_and_remove_empty_subranges range ~env
          in
          if Range.no_subranges range then begin
            empty_ranges := S.Index.Set.add index !empty_ranges
          end;
          range)
    in
    S.Index.Set.iter (fun index -> S.Index.Tbl.remove t.ranges index)
      !empty_ranges;
    { ranges;
    }
end
