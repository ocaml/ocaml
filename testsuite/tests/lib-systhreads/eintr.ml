(* TEST

* hassysthreads
include systhreads
** not-windows
*** bytecode
*** native
*)

let signals_requested = Atomic.make 0
let signal_delay = 0.1
let _ = Thread.create (fun () ->
  let signals_sent = ref 0 in
  ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigint]);
  while true do
    if Atomic.get signals_requested > !signals_sent then begin
      Thread.delay signal_delay;
      Unix.kill (Unix.getpid ()) Sys.sigint;
      incr signals_sent
    end else begin
      Thread.yield ()
    end
  done) ()
let request_signal () = Atomic.incr signals_requested

let () =
  let (rd, wr) = Unix.pipe () in
  Sys.catch_break true;
  request_signal ();
  begin match Unix.read rd (Bytes.make 1 'a') 0 1 with
  | _ -> assert false
  | exception Sys.Break -> print_endline "break: ok" end;
  Sys.catch_break false;
  Unix.close rd;
  Unix.close wr

let () =
  let (rd, wr) = Unix.pipe () in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> Gc.full_major ()));
  request_signal ();
  begin match Unix.read rd (Bytes.make 1 'a') 0 1 with
  | _ -> assert false
  | exception Unix.Unix_error(Unix.EINTR, "read", _) ->
     print_endline "eintr: ok" end;
  Sys.set_signal Sys.sigint Signal_default;
  Unix.close rd;
  Unix.close wr


(* Doing I/O on stdout would be more realistic, but seeking has the
   same locking & scheduling effects, without actually producing any
   output *)
let poke_stdout () =
  match out_channel_length stdout with
  | _ -> ()
  | exception Sys_error _ -> ()

let () =
  let r = Atomic.make true in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ ->
    poke_stdout (); Atomic.set r false));
  request_signal ();
  while Atomic.get r do
    poke_stdout ()
  done;
  Sys.set_signal Sys.sigint Signal_default;
  print_endline "chan: ok"

let () =
  let mklist () = List.init 1000 (fun i -> (i, i)) in
  let before = Sys.opaque_identity (ref (mklist ())) in
  let during = Atomic.make (Sys.opaque_identity (mklist ())) in
  let siglist = ref [] in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ ->
    Gc.full_major (); poke_stdout (); Gc.compact ();
    siglist := mklist ();
    raise Sys.Break));
  request_signal ();
  begin match
    while true do
      poke_stdout ();
      Atomic.set during (mklist ())
    done
  with
  | () -> assert false
  | exception Sys.Break -> () end;
  let expected = Sys.opaque_identity (mklist ()) in
  assert (!before = expected);
  assert (Atomic.get during = expected);
  assert (!siglist = expected);
  print_endline "gc: ok"
