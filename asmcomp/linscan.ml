(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Marcell Fischbach, University of Siegen             *)
(*                     Benedikt Meurer, University of Siegen              *)
(*                                                                        *)
(*   Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,        *)
(*     Universität Siegen.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Linear scan register allocation. *)

open Interval

module IntervalSet = Set.Make (struct
    type t = Interval.t
    let compare i j =
      let c = Int.compare j.iend i.iend in
      if c = 0 then Int.compare i.reg.stamp j.reg.stamp else c
  end)

module IntSet = Set.Make(Int)

module SpilledSet = Set.Make (struct
    type t = int * int
    let compare (iend1, ss1) (iend2, ss2) =
      let c = Int.compare iend1 iend2 in
      if c = 0 then Int.compare ss1 ss2 else c
  end)

(* Live intervals per register class *)

type class_intervals =
  {
    mutable ci_fixed: IntervalSet.t;
    mutable ci_active: IntervalSet.t;
    mutable ci_inactive: IntervalSet.t;
    mutable ci_spilled: SpilledSet.t;
    mutable ci_free_slots: IntSet.t; (* stack slots available for reuse *)
  }

let active = Array.init Proc.num_register_classes (fun _ -> {
  ci_fixed = IntervalSet.empty;
  ci_active = IntervalSet.empty;
  ci_inactive = IntervalSet.empty;
  ci_spilled = SpilledSet.empty;
  ci_free_slots = IntSet.empty;
})

let release_expired_spilled ci pos =
  let (expired, divider_in_set, rest) =
    (* (-1) is strictly below all valid positions, so (pos, (-1)) splits the set
       into all spills with (iend < pos) and all spills above (iend >= 0),
       without being itself in the set *)
    SpilledSet.split (pos, (-1)) ci.ci_spilled in
  assert (not divider_in_set);
  ci.ci_free_slots <-
    SpilledSet.fold (fun (_, ss) free -> IntSet.add ss free)
      expired ci.ci_free_slots;
  ci.ci_spilled <- rest

let dummy_interval pos =
  {Interval.reg = Reg.dummy;
   ibegin = pos;
   iend = pos;
   ranges = []}

let release_expired_fixed ci pos =
  let (rest, divider_in_set, _expired) = IntervalSet.split (dummy_interval pos) ci.ci_fixed in
  assert (not divider_in_set);
  IntervalSet.iter (fun i -> Interval.remove_expired_ranges i pos) rest;
  ci.ci_fixed <- rest

let release_expired_active ci pos =
  let (rest, divider_in_set, _expired) = IntervalSet.split (dummy_interval pos) ci.ci_active in
  assert (not divider_in_set);
  let active, inactive = IntervalSet.partition (fun i -> Interval.remove_expired_ranges i pos; Interval.is_live i pos) rest in
  ci.ci_active <- active;
  ci.ci_inactive <- IntervalSet.union inactive ci.ci_inactive

let release_expired_inactive ci pos =
  let (rest, divider_in_set, _expired) = IntervalSet.split (dummy_interval pos) ci.ci_inactive in
  assert (not divider_in_set);
  let active, inactive = IntervalSet.partition (fun i -> Interval.remove_expired_ranges i pos; Interval.is_live i pos) rest in
  ci.ci_inactive <- inactive;
  ci.ci_active <- IntervalSet.union active ci.ci_active

(* Allocate a new stack slot to the interval. *)

let allocate_stack_slot num_stack_slots i =
  let cl = Proc.register_class i.reg in
  let ci = active.(cl) in
  let ss =
    match IntSet.min_elt_opt ci.ci_free_slots with
    | Some ss ->
        ci.ci_free_slots <- IntSet.remove ss ci.ci_free_slots;
        ss
    | None ->
        let ss = num_stack_slots.(cl) in
        num_stack_slots.(cl) <- succ ss;
        ss
  in
  i.reg.loc <- Stack(Local ss);
  i.reg.spill <- true;
  ci.ci_spilled <- SpilledSet.add (i.iend, ss) ci.ci_spilled

(* Find a register for the given interval and assigns this register.
   The interval is added to active. Raises Not_found if no free registers
   left. *)

let allocate_free_register num_stack_slots i =
  begin match i.reg.loc, i.reg.spill with
    Unknown, true ->
      (* Allocate a stack slot for the already spilled interval *)
      allocate_stack_slot num_stack_slots i
  | Unknown, _ ->
      (* We need to allocate a register to this interval somehow *)
      let cl = Proc.register_class i.reg in
      begin match Proc.num_available_registers.(cl) with
        0 ->
          (* There are no registers available for this class *)
          raise Not_found
      | rn ->
          let ci = active.(cl) in
          let r0 = Proc.first_available_register.(cl) in
          (* Create register mask for this class
             note: if frame pointers are enabled then some registers may have
                   indexes that are off-bounds; we hence protect write accesses
                   below (given that the assign function will not consider such
                   registers) *)
          let regmask = Array.make rn true in
          (* Remove all assigned registers from the register mask *)
          IntervalSet.iter
            (function
              {reg = {loc = Reg r}} ->
                if r - r0 < rn then regmask.(r - r0) <- false
            | _ -> ())
            ci.ci_active;
          (* Remove all overlapping registers from the register mask *)
          let remove_bound_overlapping = function
              {reg = {loc = Reg r}} as j ->
                if (r - r0 < rn) && regmask.(r - r0)
                   && Interval.overlap j i then
                regmask.(r - r0) <- false
            | _ -> () in
          IntervalSet.iter remove_bound_overlapping ci.ci_inactive;
          IntervalSet.iter remove_bound_overlapping ci.ci_fixed;
          (* Assign the first free register (if any) *)
          let rec assign r =
            if r = rn then
              raise Not_found
            else if regmask.(r) then begin
              (* Assign the free register and insert the
                 current interval into the active list *)
              i.reg.loc <- Reg (r0 + r);
              i.reg.spill <- false;
              ci.ci_active <- IntervalSet.add i ci.ci_active
            end else
              assign (succ r) in
          assign 0
      end
  | _ -> ()
  end

let allocate_blocked_register num_stack_slots i =
  let cl = Proc.register_class i.reg in
  let ci = active.(cl) in
  match IntervalSet.min_elt_opt ci.ci_active with
  | Some ilast when
      ilast.iend > i.iend &&
      (* Last interval in active is the last interval, so spill it. *)
      let chk r = r.reg.loc = ilast.reg.loc && Interval.overlap r i in
      (* But only if its physical register is admissible for the current
         interval. *)
      not (IntervalSet.exists chk ci.ci_fixed ||
           IntervalSet.exists chk ci.ci_inactive)
    ->
      let il = IntervalSet.remove ilast ci.ci_active in
      begin match ilast.reg.loc with Reg _ -> () | _ -> assert false end;
      (* Use register from last interval for current interval *)
      i.reg.loc <- ilast.reg.loc;
      (* Remove the last interval from active and insert the current *)
      ci.ci_active <- IntervalSet.add i il;
      (* Now get a new stack slot for the spilled register *)
      allocate_stack_slot num_stack_slots ilast
  | _ ->
      (* Either the current interval is last and we have to spill it,
         or there are no registers at all in the register class (i.e.
         floating point class on i386). *)
      allocate_stack_slot num_stack_slots i

let walk_interval num_stack_slots i =
  let pos = i.ibegin land (lnot 0x01) in
  (* Release all intervals that have been expired at the current position *)
  Array.iter
    (fun ci ->
      release_expired_fixed ci pos;
      release_expired_active ci pos;
      release_expired_inactive ci pos;
      release_expired_spilled ci pos)
    active;
  try
    (* Allocate free register (if any) *)
    allocate_free_register num_stack_slots i
  with
    Not_found ->
      (* No free register, need to decide which interval to spill *)
      allocate_blocked_register num_stack_slots i

let allocate_registers (intervals : Interval.result) =
  (* Initialize the stack slots and interval lists *)
  for cl = 0 to Proc.num_register_classes - 1 do
    (* Start with empty interval lists *)
    active.(cl) <- {
      ci_fixed = IntervalSet.empty;
      ci_active = IntervalSet.empty;
      ci_inactive = IntervalSet.empty;
      ci_spilled = SpilledSet.empty;
      ci_free_slots = IntSet.empty;
    };
  done;
  (* Reset the stack slot counts *)
  let num_stack_slots = Array.make Proc.num_register_classes 0 in
  (* Add all fixed intervals (sorted by end position) *)
  List.iter
    (fun i ->
      let ci = active.(Proc.register_class i.reg) in
      ci.ci_fixed <- IntervalSet.add i ci.ci_fixed)
    intervals.fixed_intervals;
  (* Walk all the intervals within the list *)
  List.iter (walk_interval num_stack_slots) intervals.intervals;
  num_stack_slots
