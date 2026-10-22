(* TEST
 include runtime_events;
 not target-windows;
 ocamlrunparam += ",e=8";
 exit_status = "-6";
 {
   bytecode;
 }{
   native;
 }
*)

(* An event is only ever written whole, so one that would straddle the end of
   the ring is preceded by padding and written at the start instead. Making
   room therefore has to account for the event plus that padding, which can be
   one word short of the event itself, so anything larger than half the ring
   can fail to fit however far the head is advanced. Before this was rejected
   the loop in write_to_ring spun forever.

   e=8 is a 256 word ring and the payload below is 128 words, which with the
   header and timestamp is 131, comfortably over half. *)

type Runtime_events.User.tag += Big

let big_encoding =
  let encode buf () = Bytes.fill buf 0 1024 'x'; 1024 in
  let decode _ _ = () in
  Runtime_events.Type.register ~encode ~decode

let big = Runtime_events.User.register "big" Big big_encoding

let () =
  Runtime_events.start ();
  Runtime_events.User.write big ();
  (* Not reached: the write above must fatal error rather than hang. *)
  print_string "wrote an event larger than half the ring\n"
