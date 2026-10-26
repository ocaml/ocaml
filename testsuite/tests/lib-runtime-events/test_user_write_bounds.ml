(* TEST
 include runtime_events;
*)

(* Check that [Runtime_events.User.write] rejects a length returned by an
   encoder that does not fit the write buffer, rather than writing past the
   end of it. Based on the reproducer in #15074. *)

open Runtime_events

type User.tag += Bounds

(* The maximum value length documented for [Type.register].
   See runtime_events.ml for the write buffer size. *)
let max_len = 1024

let string_type =
  let encode buf value =
    let l = String.length value in
    Bytes.blit_string value 0 buf 0 l;
    l
  in
  let decode buf size = Bytes.sub_string buf 0 size in
  Type.register ~encode ~decode

(* An encoder that reports a length without writing that much. A blitting
   encoder cannot exercise this: [Bytes.blit_string] would reject an
   oversized value before the length reached the runtime. *)
let claimed_type =
  let encode _buf len = len in
  let decode _buf size = size in
  Type.register ~encode ~decode

let custom = User.register "bounds.custom" Bounds string_type
let claimed = User.register "bounds.claimed" Bounds claimed_type

let rejects f =
  match f () with
  | () -> false
  | exception Invalid_argument _ -> true

let () =
  start ();
  (* Over the maximum length, one over maximum length,
     and negative lengths are rejected. *)
  assert (rejects (fun () -> User.write claimed 20000));
  assert (rejects (fun () -> User.write claimed (max_len + 1)));
  assert (rejects (fun () -> User.write claimed (-1)));

  (* The documented maximum is accepted, and so is one byte under it. *)
  User.write custom (String.make (max_len - 1) 'y');
  User.write custom (String.make max_len 'x')

(* consumer *)

let received = ref []

let custom_handler _domain_id _ts e v =
  match User.tag e with
  | Bounds -> received := v :: !received
  | _ -> ()

let () =
  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create () |> Callbacks.add_user_event string_type custom_handler
  in
  for _ = 0 to 100 do
    ignore (read_poll cursor callbacks None)
  done;
  (* Only the two accepted writes reach the ring, and both round-trip. *)
  assert (!received = [String.make max_len 'x';
                       String.make (max_len - 1) 'y'])
