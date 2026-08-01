(* TEST
 include runtime_events;
*)

(* Check that [Runtime_events.User.write] does not allocate. *)

open Runtime_events

type User.tag += Alloc_test

let counter = User.register "alloc.counter" Alloc_test Type.int

let custom_type =
  let encode buf () = Bytes.set buf 0 'x'; 1 in
  let decode _buf _size = () in
  Type.register ~encode ~decode

let custom = User.register "alloc.custom" Alloc_test custom_type

let n = 1000

let measure f =
  let before = Gc.minor_words () in
  f ();
  let after = Gc.minor_words () in
  after -. before

let () =
  start ();
  User.write counter 0;
  User.write custom ();
  let baseline = measure (fun () -> ()) in
  let int_words = measure (fun () ->
    for i = 1 to n do User.write counter i done)
  in
  let custom_words = measure (fun () ->
    for _ = 1 to n do User.write custom () done)
  in
  assert (int_words = baseline);
  assert (custom_words = baseline);
  (* sanity check that the events reached  event ring *)
  let seen = ref 0 in
  let handler _domain_id _ts _event _value = incr seen in
  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create ()
    |> Callbacks.add_user_event Type.int handler
    |> Callbacks.add_user_event custom_type handler
  in
  ignore (read_poll cursor callbacks None);
  free_cursor cursor;
  assert (!seen > 0)
