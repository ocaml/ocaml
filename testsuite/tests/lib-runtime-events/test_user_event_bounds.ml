(* TEST
 include runtime_events;
*)
open Runtime_events

type Runtime_events.User.tag += Test

let callbacks = Runtime_events.Callbacks.create ()

let test =
  let encode buf src =
    (* simulate a buggy encoder,
       it must not return >1024,
       but there are no checks here *)
    String.length src
  and decode buf len =
    Bytes.sub_string buf 0 len
  in
  Runtime_events.Type.register ~encode ~decode

let test_event =
  Runtime_events.User.register "test" Test test

let () =
  Runtime_events.start ();
  (* must succeed *)
  Runtime_events.User.write test_event (String.make 1024 ' ');

  (* we could allow up to 1031 due to padding, but the documented limit is 1024,
     so check that it is rejected strictly *)
  [1025; 1032; 4096; 20000; Int.max_int - 8; Int.max_int]
  |> List.iter (fun len ->
    try
      Runtime_events.User.write test_event (String.make len ' ');
      failwith (Printf.sprintf "encode with length %d must not succeed" len)
    with Invalid_argument _ -> ())
