(* TEST
include runtime_events
*)
open Runtime_events

(* let's register some custom events *)
type _ User.tag +=
  | MyEvent : unit User.tag
  | MyCounter : int User.tag
  | MyCounter2 : int User.tag
  | MySpan : Type.span User.tag
  | MyString : string User.tag
let event = User.register "libname.event" MyEvent Type.event

let span = User.register "libname.phase" MySpan Type.span

let counter = User.register "libname.counter" MyCounter Type.counter

let counter2 = User.register "libname.counter2" MyCounter2 Type.counter

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

let custom = User.register "libname.custom" MyString custom_type

let () =
  start ();
  (* registering custom events after runtime event started *)
  User.write span Begin;
  User.write event ();
  User.write counter 17;
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
  | MyEvent -> got_event := true
  | _ -> ()

let counter_handler domain_id ts e v =
  match User.tag e with
  | MyCounter -> counter_value := v
  | _ -> ()

let span_handler domain_id ts e v =
  match User.tag e with
  | MySpan when v = Type.Begin -> got_span_begin := true
  | MySpan when v = Type.End -> got_span_end := true
  | _ -> ()

let custom_handler domain_id ts e v =
  match User.tag e with
  | MyString -> custom_value := v
  | _ -> ()

let () =
  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create ()
    |> Callbacks.add Type.event event_handler
    |> Callbacks.add Type.counter counter_handler
    |> Callbacks.add Type.span span_handler
    |> Callbacks.add custom_type custom_handler
  in
  for _ = 0 to 100 do
    ignore(read_poll cursor callbacks None)
  done;
  assert (!got_event);
  assert (!counter_value = 17);
  assert (!got_span_begin);
  assert (!got_span_end);
  assert (!custom_value = "hello")
