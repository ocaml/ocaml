(* TEST
   modules = "polling.c"
   compare_programs = "false"
   * arch64
   ** native
*)

(* This set of tests examine poll insertion behaviour. We do this by requesting
   and checking the number of minor collections at various points to determine
   whether a poll was correctly added. There are some subtleties because
   [caml_empty_minor_heap] will not increment the minor_collections stat if
   nothing has been allocated on the minor heap, so we sometimes need to
   add an allocation before we call [request_minor_gc]. The [minor_gcs]
   function returns the number of minor collections so far without allocating.

   ignore(Sys.opaque_identity(ref 41)) is used wherever we want to do an
   allocation in order to use some minor heap so the minor collections stat is
   incremented.

   ignore(Sys.opaque_identity(ref 42)) is used wherever we want an allocation
   for the purposes of testing whether a poll would be elided or not.
*)

external request_minor_gc : unit -> unit = "request_minor_gc"
external minor_gcs : unit -> int = "minor_gcs"

(* This function tests that polls are added to loops *)
let polls_added_to_loops () =
  let minors_before = minor_gcs () in
  request_minor_gc ();
  for a = 0 to 1 do
    ignore (Sys.opaque_identity 42)
  done;
  let minors_now = minor_gcs () in
  assert (minors_before < minors_now)


(* This function should have no prologue poll but will have
   one in the loop. *)
let func_with_added_poll_because_loop () =
  (* We do two loop iterations so that the poll is triggered whether
     in poll-at-top or poll-at-bottom mode. *)
  for a = 0 to Sys.opaque_identity(1) do
    ignore (Sys.opaque_identity 42)
  done
  [@@inline never]

let func_with_no_prologue_poll () =
  (* this function does not have indirect or 'forward' tail call nor
      does it call a synthesised function with suppressed polls. *)
  ignore(Sys.opaque_identity(minor_gcs ()))
  [@@inline never]

let prologue_polls_in_functions () =
  ignore(Sys.opaque_identity(ref 41));
  let minors_before = minor_gcs () in
  request_minor_gc ();
  func_with_added_poll_because_loop ();
  let minors_now = minor_gcs () in
  assert (minors_before < minors_now);

  ignore(Sys.opaque_identity(ref 41));
  let minors_before = minor_gcs () in
  request_minor_gc ();
  func_with_no_prologue_poll ();
  let minors_now = minor_gcs () in
  assert (minors_before = minors_now)

(* These next functions test that polls are not added to functions that
   unconditionally allocate.
   [allocating_func] allocates unconditionally
   [allocating_func_if] allocates unconditionally but does so
   on two separate branches *)
let allocating_func minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before = minors_now);
  (* No poll yet *)
  ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before + 1 = minors_now2);
  (* Polled at alloc *)
  [@@inline never]

let allocating_func_if minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before = minors_now);
  (* No poll yet *)
  if minors_before > 0 then ignore (Sys.opaque_identity (ref 42))
  else ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before < minors_now2);
  (* Polled at alloc *)
  [@@inline never]

let allocating_func_nested_ifs minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before = minors_now);
  (* No poll yet *)
  if Sys.opaque_identity(minors_before) > 0 then
    if Sys.opaque_identity(minors_before) > 1 then
      ignore (Sys.opaque_identity (ref 42))
    else
      ignore (Sys.opaque_identity (ref 42))
  else
    if Sys.opaque_identity(minors_before) < 5 then
      ignore (Sys.opaque_identity (ref 42))
    else
      ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before < minors_now2);
  (* Polled at alloc *)
  [@@inline never]

let allocating_func_match minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before = minors_now);
  (* No poll yet *)
  match minors_before with
  | 0 -> ignore (Sys.opaque_identity (ref 42))
  | _ -> ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before < minors_now2);
  (* Polled at alloc *)
  [@@inline never]

let polls_not_added_unconditionally_allocating_functions () =
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func minors_before;
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func_if minors_before;
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func_nested_ifs minors_before;
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func_match minors_before

(* This function tests that polls are not added to the back edge of
   where loop bodies allocate unconditionally *)
let polls_not_added_to_allocating_loops () =
  let current_minors = ref (minor_gcs ()) in
  request_minor_gc ();
  for a = 0 to 1 do
    (* Since the loop body allocates there should be no poll points *)
    let minors_now = minor_gcs () in
      assert(minors_now = !current_minors);
      ignore(Sys.opaque_identity(ref 42));
      let minors_now2 = minor_gcs () in
        assert(minors_now < minors_now2);
        current_minors := minors_now2;
        ignore(Sys.opaque_identity(ref 41));
        request_minor_gc ()
  done

(* this next set of functions tests that self tail recursive functions
   have polls added correctly *)
let rec self_rec_func n =
  match n with
  | 0 -> 0
  | _ ->
    begin
    let n1 = Sys.opaque_identity(n-1) in
      (self_rec_func[@tailcall]) n1
    end

let polls_added_to_self_recursive_functions () =
  let minors_before = minor_gcs () in
    request_minor_gc ();
    ignore(self_rec_func 2);
    let minors_after = minor_gcs () in
      (* should be at least one minor gc from polls in self_rec_func *)
      assert(minors_before < minors_after)

(* this pair of mutually recursive functions is to test that a poll is
   correctly placed in the first one compiled *)
let rec mut_rec_func_even d =
  match d with
  | 0 -> 0
  | _ -> mut_rec_func_odd (d-1)
and mut_rec_func_odd d =
  mut_rec_func_even (d-1)
and mut_rec_func d =
  match d with
  | n when n mod 2 == 0
    -> mut_rec_func_even n
  | n -> mut_rec_func_odd n

let polls_added_to_mutually_recursive_functions () =
  let minors_before = minor_gcs () in
  request_minor_gc ();
  ignore(mut_rec_func 3);
  let minors_after = minor_gcs () in
    (* should be at least one minor gc from polls in mut_rec_func *)
    assert(minors_before < minors_after)

(* this is to test that indirect tail calls (which might result in a self
   call) have polls inserted in them.
   These correspond to Itailcall_ind at Mach *)
let do_indirect_tail_call f n =
  f (n-1)
  [@@inline never]

let polls_added_to_indirect_tail_calls () =
  let f = fun n -> n+1 in
  let minors_before = minor_gcs () in
  request_minor_gc ();
  ignore(do_indirect_tail_call f 3);
  let minors_after = minor_gcs () in
    (* should be at one minor gc from the poll in do_indirect_tail_call *)
    assert(minors_before < minors_after)

(* this is to test that indirect non-tail calls do not have a poll placed
   in them. These correspond to Icall_ind at Mach *)
let do_indirect_call f n =
  n * f (n-1)
  [@@inline never]

let polls_not_added_to_indirect_calls () =
  let f = fun n -> n+1 in
  let minors_before = minor_gcs () in
  request_minor_gc ();
  ignore(do_indirect_call f 3);
  let minors_after = minor_gcs () in
    (* should be at one minor gc from the poll in do_indirect_tail_call *)
    assert(minors_before = minors_after)

(* this set of functions tests that we don't poll for immediate
  (non-tail) calls. These correspond to Icall_imm at Mach *)
let call_func1 n =
  Sys.opaque_identity(n-1)
  [@@inline never]

let call_func2 n =
  n * (call_func1 (Sys.opaque_identity(n+1)))
  [@@inline never]

let polls_not_added_to_immediate_calls () =
  let minors_before = minor_gcs () in
  request_minor_gc ();
  ignore(call_func1 100);
  let minors_after = minor_gcs () in
    (* should be no minor collections *)
    assert(minors_before = minors_after)

let[@inline never][@local never] app minors_before f x y =
  let minors_after_prologue = minor_gcs () in
    assert(minors_before < minors_after_prologue);
    request_minor_gc ();
    f x y

let polls_not_added_in_caml_apply () =
  let minors_before = minor_gcs() in
    request_minor_gc();
    ignore(Sys.opaque_identity(app minors_before (fun x y -> x * y) 5 4));
    let minors_after = minor_gcs() in
      assert(minors_before < minors_after)

let () =
  ignore(Sys.opaque_identity(ref 41));
  polls_added_to_loops (); (* relies on there being some minor heap usage *)

  ignore(Sys.opaque_identity(ref 41));
  prologue_polls_in_functions ();

  ignore(Sys.opaque_identity(ref 41));
  polls_added_to_self_recursive_functions ();

  ignore(Sys.opaque_identity(ref 41));
  polls_added_to_mutually_recursive_functions ();

  ignore(Sys.opaque_identity(ref 41));
  polls_added_to_indirect_tail_calls ();

  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_to_indirect_calls ();

  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_to_immediate_calls ();

  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_unconditionally_allocating_functions ();

  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_to_allocating_loops ();

  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_in_caml_apply ()
