(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)

let cnt = ref 0
let alloc_thread = 50000

let (rd1, wr1) = Unix.pipe ()
let (rd2, wr2) = Unix.pipe ()

let main_thread = Thread.self ()
let cb_main = ref 0 and cb_other = ref 0
let stopped = ref false
let alloc_callback alloc =
  if !stopped then
    None
  else begin
    let t = Thread.self () in
    if t == main_thread then begin
      assert (alloc.Gc.Memprof.size < 10 || alloc.Gc.Memprof.size mod 2 = 0);
      let do_stop = !cb_main >= alloc_thread in
      if do_stop then stopped := true;
      incr cb_main;

      assert (Unix.write wr2 (Bytes.make 1 'a') 0 1 = 1);
      if not do_stop then
        assert (Unix.read rd1 (Bytes.make 1 'a') 0 1 = 1)
    end else begin
      assert (alloc.Gc.Memprof.size < 10 || alloc.Gc.Memprof.size mod 2 = 1);
      let do_stop = !cb_other >= alloc_thread in
      if do_stop then stopped := true;
      incr cb_other;

      assert (Unix.write wr1 (Bytes.make 1 'a') 0 1 = 1);
      if not do_stop then
        assert (Unix.read rd2 (Bytes.make 1 'a') 0 1 = 1)
    end;
    Some ()
  end

let mut = Mutex.create ()
let () = Mutex.lock mut

let rec go alloc_num tid =
  Mutex.lock mut;
  Mutex.unlock mut;
  if alloc_num < alloc_thread then begin
    let len = 2 * (Random.int 200 + 1) + tid in
    Sys.opaque_identity (Array.make len 0) |> ignore;
    go (alloc_num + 1) tid
  end else begin
    cnt := !cnt + 1;
    if !cnt < 2 then begin
      Gc.minor ();    (* check for callbacks *)
      Thread.yield ();
      go alloc_num tid
    end else begin
      Gc.minor ()    (* check for callbacks *)
    end
  end

let () =
  let t = Thread.create (fun () -> go 0 1) () in
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with
      alloc_minor = alloc_callback;
      alloc_major = alloc_callback });
  Mutex.unlock mut;
  go 0 0;
  Thread.join t;
  Gc.Memprof.stop ();
  assert (!cb_main >= alloc_thread);
  assert (!cb_other >= alloc_thread);
  assert (abs (!cb_main - !cb_other) <= 1)
