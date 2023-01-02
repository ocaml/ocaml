(* TEST
include runtime_events
*)
open Runtime_events

(* let's register some custom events *)
type User.tag += Libname | Counters of int

let event = User.register "libname.event" Libname Type.unit

let span = User.register "libname.phase" Libname Type.span

let counter = User.register "libname.counter" (Counters 1) Type.int

let counter2 = User.register "libname.counter2" (Counters 2) Type.int

let custom_type =
  let encode buf value =
    let l = String.length value in
    Bytes.blit_string value 0 buf 0 l;
    l
  in
  let decode buf size =
    let target = Bytes.create size in
    Bytes.blit buf 0 target 0 size;
    Bytes.unsafe_to_string target
  in
  Type.register ~encode ~decode

let custom = User.register "libname.custom" Libname custom_type

let () =
  start ();
  (* registering custom events after runtime event started *)
  User.write span Begin;
  User.write event ();
  User.write counter 17;
  User.write counter2 18;
  User.write custom "hello";
  User.write span End

(* consumer *)

let got_event = ref false
let got_span_begin = ref false
let got_span_end = ref false
let counter_value = ref 0
let custom_value = ref ""

let event_handler domain_id ts e () =
  match User.tag e with
  | Libname -> got_event := true
  | _ -> ()

let counter_handler domain_id ts e v =
  match User.tag e with
  | Counters 2 -> counter_value := v
  | _ -> ()

let span_handler domain_id ts e v =
  match User.tag e with
  | Libname when v = Type.Begin -> got_span_begin := true
  | Libname when v = Type.End -> got_span_end := true
  | _ -> ()

let custom_handler domain_id ts e v =
  match User.tag e with
  | Libname -> custom_value := v
  | _ -> ()

let () =
  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create ()
    |> Callbacks.add_user_event Type.unit event_handler
    |> Callbacks.add_user_event Type.int counter_handler
    |> Callbacks.add_user_event Type.span span_handler
    |> Callbacks.add_user_event custom_type custom_handler
  in
  for _ = 0 to 100 do
    ignore(read_poll cursor callbacks None)
  done;
  assert (!got_event);
  assert (!counter_value = 18);
  assert (!got_span_begin);
  assert (!got_span_end);
  assert (!custom_value = "hello")
