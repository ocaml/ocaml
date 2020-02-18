(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)

let cnt = ref 0
let alloc_num = ref 0
let alloc_tot = 100000

let (rd1, wr1) = Unix.pipe ()
let (rd2, wr2) = Unix.pipe ()

let main_thread = Thread.self ()
let cb_main = ref 0 and cb_other = ref 0
let stopped = ref false
let minor_alloc_callback _ =
  if !stopped then
    None
  else begin
    let do_stop = !cb_main + !cb_other >= alloc_tot in
    if do_stop then stopped := true;
    let t = Thread.self () in
    if t == main_thread then begin
      incr cb_main;
      assert (Unix.write wr2 (Bytes.make 1 'a') 0 1 = 1);
      if not do_stop then
        assert (Unix.read rd1 (Bytes.make 1 'a') 0 1 = 1)
    end else begin
      incr cb_other;
      assert (Unix.write wr1 (Bytes.make 1 'a') 0 1 = 1);
      if not do_stop then
        assert (Unix.read rd2 (Bytes.make 1 'a') 0 1 = 1)
    end;
    Some ()
  end

let mut = Mutex.create ()
let () = Mutex.lock mut

let rec go () =
  Mutex.lock mut;
  Mutex.unlock mut;
  if !alloc_num < alloc_tot then begin
    alloc_num := !alloc_num + 1;
    Sys.opaque_identity (Bytes.make (Random.int 300) 'a') |> ignore;
    go ()
  end else begin
    cnt := !cnt + 1;
    if !cnt < 2 then begin
      Gc.minor ();    (* check for callbacks *)
      Thread.yield ();
      go ()
    end else begin
      Gc.minor ()    (* check for callbacks *)
    end
  end

let () =
  let t = Thread.create go () in
  Gc.Memprof.start
    ~callstack_size:10
    ~minor_alloc_callback
    ~major_alloc_callback:(fun _ -> None)
    ~sampling_rate:1. ();
  Mutex.unlock mut;
  go ();
  Thread.join t;
  Gc.Memprof.stop ();
  assert (abs (!cb_main - !cb_other) <= 1);
  assert (!cb_main + !cb_other >= alloc_tot)
