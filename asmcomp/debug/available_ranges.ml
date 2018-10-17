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

(* CR-soon mshinwell: pull this type forward so other passes can use it *)
type is_parameter =
  | Local
  | Parameter of { index : int; }

module Scope = struct
  type t = {
    id : int;
    start_pos : Linearize.label;
    mutable end_pos : Linearize.label option;
    mutable parent : t option;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 = Stdlib.compare t1.id t2.id
    let equal t1 t2 = (t1.id = t2.id)
    let hash t = Hashtbl.hash t

    let print ppf { id; start_pos; end_pos; parent = _; } =
      match end_pos with
      | None ->
        Format.fprintf ppf "(open (id %d) (start %d))"
          id start_pos
      | Some end_pos ->
        Format.fprintf ppf "(open (id %d) (start %d) (end %d))"
          id start_pos end_pos

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let next_id = ref 0

  let create ~start_pos ~parent =
    let id = !next_id in
    incr next_id;
    { id;
      start_pos;
      end_pos = None;
      parent;
    }

  let is_closed t =
    match t.end_pos with
    | None -> false
    | Some _ -> true

  let close t ~end_pos =
    match t.end_pos with
    | Some _ -> Misc.fatal_errorf "Scope is already closed: %a" print t
    | None -> t.end_pos <- Some end_pos

  let start_pos t = t.start_pos

  let end_pos t =
    match t.end_pos with
    | None -> Misc.fatal_errorf "Scope is not closed: %a" print t
    | Some end_pos -> end_pos

  let parent t = t.parent

  let reparent t ~new_parent = t.parent <- new_parent
end

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
    -> stack_offset:int
    -> t

  val create_phantom
     : start_pos:Linearize.label
    -> end_pos:Linearize.label
    -> t

  val start_pos : t -> L.label
  val end_pos : t -> L.label
  val end_pos_offset : t -> int option
  val location : t -> unit location
  val offset_from_stack_ptr_in_bytes : t -> int
end = struct
  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom

  type start_insn_or_phantom = L.instruction location

  type t = {
    (* CR-soon mshinwell: find a better name for [start_insn] *)
    start_insn : start_insn_or_phantom;
    start_pos : L.label;
    (* CR mshinwell: we need to check exactly what happens with function
       epilogues, including returns in the middle of functions. *)
    end_pos : L.label;
    end_pos_offset : int option;
    offset_from_stack_ptr_in_bytes : int option;
  }

  let create ~(reg : Reg.t) ~(start_insn : Linearize.instruction)
        ~start_pos ~end_pos ~end_pos_offset ~stack_offset =
    let offset_from_stack_ptr_in_bytes =
      match reg.loc with
      | Stack loc ->
        let frame_size = Proc.frame_size ~stack_offset in
        let slot_offset =
          Proc.slot_offset loc ~reg_class:(Proc.register_class reg)
            ~stack_offset
        in
        Some (frame_size - slot_offset)
      | Reg _ | Unknown -> None
    in
    match start_insn.desc with
    | L.Llabel _ ->
      { start_insn = Reg (reg, start_insn);
        start_pos;
        end_pos;
        end_pos_offset;
        offset_from_stack_ptr_in_bytes = offset_from_stack_ptr_in_bytes;
      }
    | _ -> failwith "Available_subrange.create"

  let create_phantom ~start_pos ~end_pos =
    { start_insn = Phantom;
      start_pos;
      end_pos;
      end_pos_offset = None;
      offset_from_stack_ptr_in_bytes = None;
    }

  let start_pos t = t.start_pos
  let end_pos t = t.end_pos
  let end_pos_offset t = t.end_pos_offset

  let location t : unit location =
    let convert_location (start_insn : start_insn_or_phantom) : unit location =
      match start_insn with
      | Reg (reg, _insn) -> Reg (reg, ())
      | Phantom -> Phantom
    in
    convert_location t.start_insn

  let offset_from_stack_ptr_in_bytes t =
    match t.offset_from_stack_ptr_in_bytes with
    | Some offset -> offset
    | None ->
      Misc.fatal_error "No offset from stack pointer available (this is either \
        a phantom available subrange or one whose corresponding register is \
        not assigned to the stack)"
end

type type_info =
  | From_cmt_file of Backend_var.Provenance.t option
  | Phantom of
      Backend_var.Provenance.t option * Mach.phantom_defining_expr option

let _type_info_has_provenance = function
  | From_cmt_file None -> false
  | From_cmt_file (Some _) -> true
  | Phantom (None, _) -> false
  | Phantom (Some _, _) -> true

module Available_range : sig
  type t

  val create
     : scope:Scope.t option
    -> type_info:type_info
    -> is_parameter:is_parameter
    -> var:Backend_var.t
    -> t

  val scope : t -> Scope.t option
  val type_info : t -> type_info
  val is_parameter : t -> is_parameter
  val var : t -> Backend_var.t
  val var_location : t -> Debuginfo.t
  val add_subrange : t -> subrange:Available_subrange.t -> unit
  val extremities : t -> L.label * L.label

  val set_scope : t -> Scope.t option -> unit

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
    mutable scope : Scope.t option;
    mutable subranges : Available_subrange.t list;
    mutable min_pos : L.label option;
    mutable max_pos : L.label option;
    type_info : type_info;
    is_parameter : is_parameter;
    var : Backend_var.t;
    var_location : Debuginfo.t;
  }

  let create ~scope ~type_info ~is_parameter ~var =
    let var_location =
      match type_info with
      | From_cmt_file None | Phantom (None, _) -> Debuginfo.none
      | From_cmt_file (Some provenance) | Phantom (Some provenance, _) ->
        Backend_var.Provenance.location provenance
    in
    { scope; subranges = []; min_pos = None; max_pos = None;
      type_info; is_parameter; var; var_location;
    }

  let scope t = t.scope
  let type_info t = t.type_info
  let is_parameter t = t.is_parameter
  let var t = t.var
  let var_location t = t.var_location

  let set_scope t scope =
    t.scope <- scope

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
  mutable scopes : Scope.t list;
}

let find t ~var =
  match Backend_var.Tbl.find t.ranges var with
  | exception Not_found -> None
  | range -> Some range

let fold t ~init ~f =
  Backend_var.Tbl.fold (fun var range acc ->
      (* CR-soon mshinwell: improve efficiency *)
      let with_same_name =
        List.filter (fun (var', range') ->
            Backend_var.name var = Backend_var.name var'
              && Available_range.is_parameter range
                = Available_range.is_parameter range')
          (Backend_var.Tbl.to_list t.ranges)
      in
      let with_same_location =
        List.filter (fun (_, range') ->
            Available_range.var_location range
                = Available_range.var_location range'
              && Available_range.is_parameter range
                = Available_range.is_parameter range')
          (Backend_var.Tbl.to_list t.ranges)
      in
      let name_is_unique = List.length with_same_name <= 1 in
      let location_is_unique = List.length with_same_location <= 1 in
      f acc ~var ~name_is_unique ~location_is_unique ~range)
    t.ranges
    init

let scopes t = List.rev t.scopes

module Make (S : sig
  module Key : sig
    type t

    val assert_valid : t -> unit

    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t
  end

  val used : L.instruction -> Key.Set.t
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
    -> stack_offset:int
    -> Available_subrange.t

  val end_pos_offset
     : prev_insn:L.instruction option
    -> key:Key.t
    -> int option
end) = struct
  module KM = S.Key.Map
  module KS = S.Key.Set

  type scope_kind =
    | From_let_or_otherwise
    | From_linear_code
    (* [From_linear_code] scopes must match up exactly with [Lstart_scope]
       and [Lend_scope] pseudo-instructions in the linearised code.  All
       other scopes (e.g. ones that kind of correspond to "let"s, deduced
       from availability information) must be marked as
       [From_let_or_otherwise]. *)

  type new_scope =
    | Start_new_scope of scope_kind
    | Keep_existing_scope

  (* Imagine that the program counter is exactly at the start of [insn]; it has
     not yet been executed.  This function calculates which available subranges
     are to start at that point, and which are to stop.  [prev_insn] is the
     instruction immediately prior to [insn], if such exists.

     This function also calculates whether a new scope should be started based
     on the availability information and which variables have previously been
     assigned scopes.
  *)
  let births_and_deaths ~(insn : L.instruction)
        ~(prev_insn : L.instruction option) ~already_have_scopes =
    (* Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from
       the CFA, not from the stack pointer.

       This pass may generate ranges that are the same as other ranges,
       but those are deduped in [Dwarf].
    *)
    let proto_births =
      match prev_insn with
      | None -> S.available_before insn
      | Some prev_insn ->
        KS.diff (S.available_before insn) (S.available_before prev_insn)
    in
    let new_scope =
      let proto_births_without_scopes =
        KS.diff proto_births (KS.of_list (
          List.map (fun (key, _data) -> key) (KM.bindings already_have_scopes)))
      in
      if KS.is_empty proto_births_without_scopes then Keep_existing_scope
      else Start_new_scope From_let_or_otherwise
    in
    let proto_deaths =
      match prev_insn with
      | None -> KS.empty
      | Some prev_insn ->
        KS.diff (S.available_before prev_insn) (S.available_before insn)
    in
    let restart_ranges =
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
    new_scope, births, deaths

  type scope_stack = (scope_kind * Scope.t) list

  (* Given a distinguished [current_stack] and a list of other [stacks],
     find the point---starting from the outermost scopes on the stacks---where
     the stacks diverge.  The three values returned are:
     1. the stack prefix (in the usual order, with the outermost scope at its
        tail) that is common to all of the stacks;
     2. the set of scopes (which may be empty) that occur immediately
        inner to the point of divergence, but without including such a scope
        from the [current_stack];
     3. the remainder of the current stack, again in the usual order, from
        immediately after the point of divergence up to and including the
        current scope. *)
  let closest_common_ancestor ~(current_stack : scope_stack)
        (stacks : scope_stack list) =
    let rec loop ~current_stack stacks ~stack_prefix =
      let found_discrepancy ~all_subsequent_scopes =
        stack_prefix, all_subsequent_scopes, List.rev current_stack
      in
      let all_stacks = current_stack :: stacks in
      let heads_and_tails =
        Misc.Stdlib.List.filter_map (fun stack ->
            match stack with
            | [] -> None
            | head::tail -> Some (head, tail))
          (current_stack :: stacks)
      in
      let heads, tails = List.split heads_and_tails in
      let scopes_at_heads =
        Scope.Set.of_list (List.map (fun (_kind, scope) -> scope) heads)
      in
      let all_subsequent_scopes =
        match current_stack with
        | [] -> scopes_at_heads
        | (_kind, scope)::_ -> Scope.Set.remove scope scopes_at_heads
      in
      if List.compare_lengths heads all_stacks <> 0 then
        found_discrepancy ~all_subsequent_scopes
      else
        if Scope.Set.cardinal scopes_at_heads <> 1 then
          found_discrepancy ~all_subsequent_scopes
        else
          let kind_and_scope = List.hd heads in
          let current_stack = List.hd tails in
          let stacks = List.tl tails in
          let stack_prefix = kind_and_scope :: stack_prefix in
          loop ~current_stack stacks ~stack_prefix
    in
    let current_stack = List.rev current_stack in
    let stacks = List.map (fun stack -> List.rev stack) stacks in
    loop ~current_stack stacks ~stack_prefix:[]

  (* This function scans the Linearize code and does two things: firstly,
     creation of available ranges; secondly, inference of approximate
     lexical scopes. *)
  (* CR-someday mshinwell: The lexical scope inference should be altered to
     use scope information propagated from the middle end. *)
  let rec process_instruction t ~fundecl ~first_insn ~(insn : L.instruction)
        ~prev_insn ~open_subrange_start_insns ~stack_offset
        ~(scope_stack : scope_stack)
        ~(already_have_scopes : (Available_range.t * scope_stack) KM.t) =
    let new_scope_from_lets, births, deaths =
      births_and_deaths ~insn ~prev_insn ~already_have_scopes
    in
    let new_scope_from_blocks =
      match insn.desc with
      | Lstart_scope -> Start_new_scope From_linear_code
      | _ -> Keep_existing_scope
    in
    let new_scope =
      (* We must always favour [Start_new_scope] when it comes from the
         Linearize code, since the start/end delimiters in that code are
         checked against our scopes, to ensure valid scoping. *)
      match scope_stack with
      | [] ->
        begin match insn.desc with
        | Lstart_scope -> Start_new_scope From_linear_code
        | _ -> Start_new_scope From_let_or_otherwise
        end
      | _::_ ->
        match new_scope_from_lets, new_scope_from_blocks with
        | Start_new_scope _, Start_new_scope _ ->
          Start_new_scope From_linear_code
        | Start_new_scope _, Keep_existing_scope ->
          Start_new_scope From_let_or_otherwise
        | Keep_existing_scope, Start_new_scope _ ->
          Start_new_scope From_linear_code
        | Keep_existing_scope, Keep_existing_scope ->
          Keep_existing_scope
    in
    let first_insn = ref first_insn in
    let prev_insn = ref prev_insn in
    let insert_insn ~new_insn =
      assert (new_insn.L.next == insn);
      (* (Note that by virtue of [Lprologue], we can insert labels prior
         to the first assembly instruction of the function.) *)
      begin match !prev_insn with
      | None -> first_insn := new_insn
      | Some prev_insn ->
        assert (prev_insn.L.next == insn);
        prev_insn.L.next <- new_insn
      end;
      prev_insn := Some new_insn
    in
    let scope, scope_stack =
      match new_scope with
      | Start_new_scope scope_kind ->
        (* The [start_pos] must always be a new label to ensure such labels
           stay in order.  We rely on the order below when inserting new scopes
           for keys which are still available but currently marked as out
           of scope. *)
        let start_pos = Cmm.new_label () in
        let start_pos_insn =
          { L.
            desc = Llabel start_pos;
            next = insn;
            arg = [| |];
            res = [| |];
            dbg = insn.dbg;
            live = insn.live;
            available_before = insn.available_before;
            phantom_available_before = insn.phantom_available_before;
            available_across = None;
          }
        in
        insert_insn ~new_insn:start_pos_insn;
        let scope, scope_stack =
          match scope_stack with
          | [] ->
            let scope = Scope.create ~start_pos ~parent:None in
            let scope_stack = [scope_kind, scope] in
            scope, scope_stack
          | (_kind, scope)::_ ->
            let scope = Scope.create ~start_pos ~parent:(Some scope) in
            let scope_stack = (scope_kind, scope)::scope_stack in
            scope, scope_stack
        in
        t.scopes <- scope::t.scopes;
        scope, scope_stack
      | Keep_existing_scope ->
        let scope =
          match scope_stack with
          | [] -> Misc.fatal_error "Keep_existing_scope without current scope"
          | (_kind, scope)::_ -> scope
        in
        scope, scope_stack
    in
    (* Variables which we see uses of, but which are now out of scope, have
       their scopes corrected by inserting a new scope.  This helps to ensure
       visibility in the debugger (for example where a register holding the
       value of some variable is shared between the results of switch arms,
       and as such is initially marked as going out of scope at the end of
       such arms, even though the variable may be bound as an enclosing
       "let" around the whole switch) and thus should be visible
       subsequently. *)
    let used_but_now_out_of_scope =
      let used = S.used insn in
      KS.filter (fun key ->
          match KM.find key already_have_scopes with
          | exception Not_found -> false
          | (range, _scope_stack) ->
            match Available_range.scope range with
            | None -> false
            | Some scope -> Scope.is_closed scope)
        used
    in
    let scope, scope_stack =
      if not (KS.is_empty used_but_now_out_of_scope) then begin
        (* To make life easier, we only ever insert one new scope for keys that
           are still available, but currently marked as out of scope.  (The
           alternative would be one new scope for each distinct scope found
           amongst such keys.) *)
        let earliest_start_pos_for_used_but_now_out_of_scope,
            all_stacks_for_used_but_now_out_of_scope =
          KS.fold (fun key (earliest_start_pos, all_stacks) ->
              match KM.find key already_have_scopes with
              | exception Not_found -> assert false  (* see above *)
              | (range, scope_stack) ->
                let scope = Available_range.scope range in
                match scope with
                | None -> assert false  (* see above *)
                | Some scope ->
                  let earliest_start_pos =
                    (* This relies on the ordering noted in the comment
                       above. *)
                    min (Scope.start_pos scope) earliest_start_pos
                  in
                  earliest_start_pos, scope_stack::all_stacks)
            used_but_now_out_of_scope
            (max_int, [])
        in
        assert (earliest_start_pos_for_used_but_now_out_of_scope
          <> max_int);
        let common_prefix, all_subsequent_scopes,
            remainder_of_current_stack =
          closest_common_ancestor ~current_stack:scope_stack
            all_stacks_for_used_but_now_out_of_scope
        in
        let parent_scope_for_used_but_now_out_of_scope =
          match common_prefix with
          | [] -> None
          | (_kind, innermost_scope)::_ -> Some innermost_scope
        in
        let new_scope_for_used_but_now_out_of_scope =
          Scope.create
            ~start_pos:earliest_start_pos_for_used_but_now_out_of_scope
            ~parent:parent_scope_for_used_but_now_out_of_scope
        in
        begin match parent_scope_for_used_but_now_out_of_scope with
        | None ->
          t.scopes <- t.scopes @ [new_scope_for_used_but_now_out_of_scope]
        | Some parent ->
          let scopes =
            List.fold_right (fun scope scopes ->
                if not (Scope.equal scope parent) then
                  scope :: scopes
                else
                  new_scope_for_used_but_now_out_of_scope :: scope :: scopes)
              t.scopes
              []
          in
          t.scopes <- scopes
        end;
        let scope_stack =
          remainder_of_current_stack
            @ (From_let_or_otherwise, new_scope_for_used_but_now_out_of_scope)
            :: common_prefix
        in
        let _kind, scope = List.hd scope_stack in
        Scope.Set.iter (fun subsequent_scope ->
            assert (not (Scope.equal scope subsequent_scope));
            Scope.reparent subsequent_scope ~new_parent:(
              Some new_scope_for_used_but_now_out_of_scope))
          all_subsequent_scopes;
        KS.iter (fun key ->
            match KM.find key already_have_scopes with
            | exception Not_found -> assert false  (* see above *)
            | (range, _scope_stack) ->
              Format.eprintf "[%a] Scope for %a: %a --> %a\n%!"
                Backend_sym.print fundecl.L.fun_name
                Backend_var.print (Available_range.var range)
                (Misc.Stdlib.Option.print Scope.print)
                (Available_range.scope range)
                Scope.print
                new_scope_for_used_but_now_out_of_scope;
              Available_range.set_scope range
                (Some new_scope_for_used_but_now_out_of_scope))
          used_but_now_out_of_scope;
        scope, scope_stack
      end else begin
        scope, scope_stack
      end
    in
    (* Note that we can't reuse an existing label in the code since we rely
       on the ordering of range-related labels. *)
    let label = Cmm.new_label () in
    let used_label = ref false in
    (* As a result of the code above to restart subranges, we may have
       a register occurring in both [births] and [deaths]; and we would
       like the register to have an open subrange from this point.  It
       follows that we should process deaths before births. *)
    let already_have_scopes =
      KS.fold (fun (key : S.Key.t) already_have_scopes ->
          S.Key.assert_valid key;
          let start_pos, start_insn =
            try KM.find key open_subrange_start_insns
            with Not_found -> assert false
          in
          let end_pos = label in
          used_label := true;
          let end_pos_offset = S.end_pos_offset ~prev_insn:!prev_insn ~key in
          match S.range_info ~fundecl ~key ~start_insn with
          | None -> already_have_scopes
          | Some (var, type_info, is_parameter) ->
            let range, already_have_scopes =
              match Backend_var.Tbl.find t.ranges var with
              | range -> range, already_have_scopes
              | exception Not_found ->
                let range =
                  Available_range.create ~scope:(Some scope) ~type_info
                    ~is_parameter ~var
                in
                Backend_var.Tbl.add t.ranges var range;
                assert (not (KM.mem key already_have_scopes));
                let already_have_scopes =
                  KM.add key (range, scope_stack) already_have_scopes
                in
                range, already_have_scopes
            in
            let subrange =
              S.create_subrange ~fundecl ~key ~start_pos ~start_insn ~end_pos
                ~end_pos_offset ~stack_offset
            in
            Available_range.add_subrange range ~subrange;
            already_have_scopes)
        deaths
        already_have_scopes
    in
    let label_insn =
      { L.
        desc = Llabel label;
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
    let insert_end_of_scope_label () =
      let end_pos = Cmm.new_label () in
      let end_pos_insn =
        { L.
          desc = Llabel end_pos;
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
      insert_insn ~new_insn:end_pos_insn;
      end_pos
    in
    match insn.desc with
    | Lend ->
      let end_pos = insert_end_of_scope_label () in
      List.iter (fun (kind, scope) ->
          begin match kind with
          | From_let_or_otherwise -> ()
          | From_linear_code ->
            Misc.fatal_error "Unmatched [Lstart_scope] / [Lend_scope] at Lend"
          end;
          Scope.close scope ~end_pos)
        scope_stack;
      first_insn
    | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Llabel _
    | Lbranch _ | Lcondbranch _ | Lcondbranch3 _ | Lswitch _
    | Lsetuptrap _ | Lpushtrap | Lpoptrap | Lraise _
    | Lstart_scope | Lend_scope ->
      let stack_offset =
        match insn.desc with
        | Lop (Istackoffset delta) -> stack_offset + delta
        | Lpushtrap -> stack_offset + Proc.trap_frame_size_in_bytes
        | Lpoptrap -> stack_offset - Proc.trap_frame_size_in_bytes
        | Lend | Lprologue | Lop _ | Lreloadretaddr | Lreturn
        | Llabel _ | Lbranch _ | Lcondbranch _ | Lcondbranch3 _
        | Lswitch _ | Lsetuptrap _ | Lraise _
        | Lstart_scope | Lend_scope -> stack_offset
      in
      let scope_stack =
        match insn.desc with
        | Lend_scope ->
          let end_pos = insert_end_of_scope_label () in
          let rec close scope_stack =
            match scope_stack with
            | [] -> [], false
            | (From_let_or_otherwise, scope)::scope_stack ->
              Scope.close scope ~end_pos;
              close scope_stack
            | (From_linear_code, scope)::scope_stack ->
              Scope.close scope ~end_pos;
              scope_stack, true
          in
          let scope_stack, found_block_scope = close scope_stack in
          if not found_block_scope then begin
            Misc.fatal_error "Unmatched [Lstart_scope] / [Lend_scope]"
          end;
          scope_stack
        | _ -> scope_stack
      in
      process_instruction t ~fundecl ~first_insn ~insn:insn.L.next
        ~prev_insn:(Some insn) ~open_subrange_start_insns ~stack_offset
        ~scope_stack ~already_have_scopes

  let process_instructions t ~fundecl ~first_insn =
    let stack_offset = Proc.initial_stack_offset in
    process_instruction t ~fundecl ~first_insn ~insn:first_insn
      ~prev_insn:None ~open_subrange_start_insns:KM.empty ~stack_offset
      ~scope_stack:[] ~already_have_scopes:KM.empty
end

module Make_ranges = Make (struct
  module RD = Reg_with_debug_info

  (* By the time this pass has run, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that identifies two registers iff they have the same name and
     location. *)
  module Key = struct
    type t = RD.t

    (* CR mshinwell: check this *)
    let assert_valid (_t : t) = ()
     (*  assert (t.name <> None);  (* cf. [Available_filtering] *) *)

    module Map = RD.Map_distinguishing_names_and_locations
    module Set = RD.Set_distinguishing_names_and_locations
  end

  (* CR mshinwell: improve efficiency *)
  let available_before (insn : L.instruction) =
    match insn.available_before with
    | Unreachable -> Key.Set.empty
    | Ok available_before -> Key.Set.of_list (RD.Set.elements available_before)

  let used (insn : L.instruction) =
    let stamps_of_used_regs =
      Numbers.Int.Set.of_list (
        List.map (fun (reg : Reg.t) -> reg.stamp)
          (Array.to_list insn.arg))
    in
    Key.Set.filter (fun rd ->
        Numbers.Int.Set.mem (RD.reg rd).stamp stamps_of_used_regs)
      (available_before insn)

  let end_pos_offset ~prev_insn ~key:reg =
    (* If the range is for a register destroyed by a call and which
       ends immediately after a call instruction, move the end of the
       range back very slightly.  The effect is that the register is seen
       in the debugger as available when standing on the call instruction
       but unavailable when we are in the callee (and move to the previous
       frame). *)
    (* CR-someday mshinwell: I wonder if this should be more
       conservative for Iextcall.  If the C callee is compiled with
       debug info then it should describe where any callee-save
       registers have been saved, so when we step back to the OCaml frame
       in the debugger, the unwinding procedure should get register
       values correct.  (I think.)  However if it weren't compiled with
       debug info, this wouldn't happen, and when looking back up into
       the OCaml frame I suspect registers would be wrong.  This may
       not be a great problem once libmonda is hardened, although it
       is possible for this to be subtle and misleading (e.g. an integer
       value being 1 instead of 2 or something.) *)
    match prev_insn with
    | None -> None
    | Some prev_insn ->
      match prev_insn.L.desc with
      | Lop ((Icall_ind _ | Icall_imm _ | Iextcall _) as op) ->
        let destroyed_locations =
          Array.map (fun (reg : Reg.t) -> reg.loc)
            (Proc.destroyed_at_oper (Mach.Iop op))
        in
        let holds_immediate = RD.holds_non_pointer reg in
        let on_stack = RD.assigned_to_stack reg in
        let live_across = Reg.Set.mem (RD.reg reg) prev_insn.L.live in
        let remains_available =
          live_across
            || (holds_immediate && on_stack)
        in
        if Array.mem (RD.location reg) destroyed_locations
            || not remains_available
        then
          Some (-1)
        else
          None
      | _ -> None

  let range_info ~fundecl:_ ~key:reg ~start_insn:_ =
    match RD.debug_info reg with
    | None -> None
    | Some debug_info ->
      let is_parameter =
        match RD.Debug_info.which_parameter debug_info with
        | None -> Local
        | Some index -> Parameter { index; }
      in
      let var = RD.Debug_info.holds_value_of debug_info in
      Some (var, From_cmt_file (RD.Debug_info.provenance debug_info),
        is_parameter)

  let create_subrange ~fundecl:_ ~key:reg ~start_pos ~start_insn ~end_pos
        ~end_pos_offset ~stack_offset =
    Available_subrange.create ~reg:(RD.reg reg)
      ~start_pos ~start_insn
      ~end_pos ~end_pos_offset
      ~stack_offset
end)

module Make_phantom_ranges = Make (struct
  module Key = struct
    include Ident

    let assert_valid _t = ()
  end

  let used (_insn : L.instruction) = Key.Set.empty

  let available_before (insn : L.instruction) = insn.phantom_available_before

  let end_pos_offset ~prev_insn:_ ~key:_ = None

  let range_info ~fundecl ~key ~start_insn:_ =
    match Backend_var.Map.find key fundecl.L.fun_phantom_lets with
    | exception Not_found ->
      Misc.fatal_errorf "Available_ranges.Make_phantom_ranges.create_range: \
          phantom variable occurs in [phantom_available_before] but not in \
          [fun_phantom_lets]: %a"
        Key.print key
    | provenance, defining_expr ->
      (* CR-someday mshinwell: Presumably the "Local" only changes to indicate
         a parameter when we can represent the inlined frames properly in
         DWARF.  (Otherwise the function into which an inlining occurs ends up
         having more parameters than it should in the debugger.)
      *)
      Some (key, Phantom (provenance, Some defining_expr), Local)

  let create_subrange ~fundecl:_ ~key:_ ~start_pos ~start_insn:_ ~end_pos
        ~end_pos_offset:_ ~stack_offset:_ =
    (* Ranges for phantom variables are emitted as contiguous blocks
       which are designed to approximately indicate their scope.
       Some such phantom variables' values may ultimately be derived
       from the values of normal variables (e.g. "Read_var_field") and
       thus will be unavailable when those normal variables are
       unavailable.  This effective intersecting of available ranges
       is handled automatically in the debugger since we emit DWARF that
       explains properly how the phantom variables relate to other
       (normal or phantom) ones. *)
    Available_subrange.create_phantom ~start_pos ~end_pos
end)

let create ~fundecl =
  let t = {
    ranges = Backend_var.Tbl.create 42;
    scopes = [];
  }
  in
  let first_insn =
    let first_insn = fundecl.L.fun_body in
    Make_ranges.process_instructions t ~fundecl ~first_insn
  in
  let first_insn =
    Make_phantom_ranges.process_instructions t ~fundecl ~first_insn
  in
  (* It is unfortunately the case that variables can be named in the defining
     expressions of phantom ranges without actually having any available range
     themselves.  This might be caused by, for example, a "name for debugger"
     operation on a register assigned to %rax immediately before an allocation
     on x86-64 (which clobbers %rax).  The register is explicitly removed from
     the availability sets by [Available_regs], and the name never appears on
     any available range.
     To prevent this situation from causing problems later on, we add empty
     ranges for any such variables.  We assume such variables are local
     variables. *)
  let variables_without_ranges =
    fold t ~init:[]
      ~f:(fun acc ~var:_ ~name_is_unique:_ ~location_is_unique:_ ~range ->
        match Available_range.type_info range with
        | From_cmt_file _ -> acc
        | Phantom (_, defining_expr) ->
          let vars =
            match defining_expr with
            | None -> []
            | Some defining_expr ->
              match defining_expr with
              | Iphantom_const_int _
              | Iphantom_const_symbol _
              | Iphantom_read_symbol_field _ -> []
              | Iphantom_var var
              | Iphantom_read_field { var; _ }
              | Iphantom_offset_var { var; _ } -> [var]
              | Iphantom_block { fields; _ } ->
                Misc.Stdlib.List.filter_map (fun field -> field) fields
          in
          let without_ranges =
            List.filter (fun var -> not (Backend_var.Tbl.mem t.ranges var)) vars
          in
          acc @ without_ranges)
  in
  List.iter (fun var ->
      let range =
        Available_range.create ~scope:None ~type_info:(Phantom (None, None))
          ~is_parameter:Local ~var
      in
      Backend_var.Tbl.add t.ranges var range)
    variables_without_ranges;
  t, { fundecl with L.fun_body = first_insn; }

let create ~fundecl =
  if not !Clflags.debug then
    let t =
      { ranges = Backend_var.Tbl.create 1;
        scopes = [];
      }
    in
    t, fundecl
  else
    create ~fundecl

type label_classification =
  | Start of {
      end_pos : Linearize.label;
      location : unit Available_subrange.location;
    }
  | End

let classify_label t label =
  Backend_var.Tbl.fold (fun var range result ->
      Available_range.fold range ~init:result
        ~f:(fun result ~available_subrange:subrange ->
          if Available_subrange.start_pos subrange = label then
            let location = Available_subrange.location subrange in
            let end_pos = Available_subrange.end_pos subrange in
            (Start { end_pos; location; }, var, subrange) :: result
          else if Available_subrange.end_pos subrange = label then
            (End, var, subrange) :: result
          else
            result))
    t.ranges
    []
