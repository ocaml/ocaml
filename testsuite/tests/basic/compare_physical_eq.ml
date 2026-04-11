(* TEST *)

(* Tests for the physical equality shortcut in compare.c (default case).
   A record with a closure field (fn) will raise Invalid_argument if compare
   recurses into it. The shortcut fires when v1 == v2, skipping field traversal.
   To verify the patch is load-bearing: comment out the line
     [if (v1 == v2) goto next_item;]
   in runtime/compare.c (default case, ~line 299), rebuild, and confirm that
   tests 1-4 and 6 fail (raise Invalid_argument). Restore the line to pass. *)

let test n check res =
  print_string "Test "; print_int n;
  if check res then print_string " passed.\n" else print_string " FAILED.\n";
  flush stdout

let eqtrue (b : bool) = b

let eqfun delayed_check =
  match delayed_check () with
  | exception Invalid_argument _ -> true
  | _ -> false

(* fn field: without the patch, any (=) that recurses into it will raise *)
type foo = { id: int; fn: unit -> unit }
type bar = { id: int; fn: unit -> unit; data: int list; extra: float }
type outer = { id: int; fn: unit -> unit; inner: inner }
and inner = { id: int; fn: unit -> unit; x: int; y: float }

let () =
  (* Test 1: simple record, compared to itself (shortcut at record level) *)
  let v = { id = 1; fn = (fun () -> ()) } in
  test 1 eqtrue (v = v);

  (* Test 2: record with more fields, compared to itself *)
  let v = { id = 2; fn = (fun () -> ()); data = [1; 2; 3]; extra = 3.14 } in
  test 2 eqtrue (v = v);

  (* Test 3: nested record, both levels carry a closure *)
  let v = { id = 3; fn = (fun () -> ());
            inner = { id = 30; fn = (fun () -> ()); x = 10; y = 2.71 } } in
  test 3 eqtrue (v = v);

  (* Test 4: two distinct list allocations containing the same (shared) item.
     The list cons cells are different pointers so no shortcut there, but when
     compare recurses into elements it finds item == item, so shortcut fires. *)
  let item = { id = 4; fn = (fun () -> ()) } in
  let lst1 = [item; item] in
  let lst2 = Sys.opaque_identity ([item]@[item]) in
  assert (lst1 != lst2); (* distinct list allocations *)
  test 4 eqtrue (lst1 = lst2);

  (* Test 5: two different foo allocations with same id but distinct closures.
     They are NOT physically equal, so the shortcut does not fire and compare
     will recurse into fn, which should raise Invalid_argument. *)
  let v1 = { id = 42; fn = (fun () -> ()) } in
  let v2 = { id = 42; fn = (fun () -> ()) } in
  test 5 eqfun (fun () -> v1 = v2);

  (* Test 6: physical equality and structural equality coincide *)
  let v = { id = 7; fn = ignore } in
  test 6 eqtrue (v == v && v = v);

  ()
