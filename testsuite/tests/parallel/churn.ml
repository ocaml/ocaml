(* TEST *)

type t = {
  sender: int;
  code: int;
  msgsize: int;
  message: int array;
}

let size = 20_000
let table = Array.init size (fun _ -> Atomic.make None)
let go = Atomic.make true
let log = false

let run me msgsize iters =
  (* domain 0 keeps a bunch of extra local data,
     to unbalance sweeping loads *)
  let kept = ref [] in
  if me = 0 then kept := [Array.init 10000 ref];
  let count = ref iters in
  let from0 = ref 0 in
  while !count > 0 && Atomic.get go do
    ignore (Sys.opaque_identity (ref []));
    let slot = Random.int size in
    match Atomic.get table.(slot) with
    | None as prev ->
      let code = Random.bits () in
      let msg = {sender = me; code; msgsize; message = Array.make msgsize code} in
      if me = 0 then kept := Array.init 5 ref :: !kept;
      (* pointless string formatting to create minor garbage *)
      let dbg =
        Printf.sprintf "[%d]:      %03d: %d %08x --->\n" me slot msg.msgsize msg.code in
      if Sys.opaque_identity log then print_string dbg;
      if Atomic.compare_and_set table.(slot) prev (Some msg) then
        decr count
    | Some msg as prev when
        msg.sender <> me &&
        Atomic.compare_and_set table.(slot) prev None ->

      let dbg = Printf.sprintf "[%d]: ---> %03d: %d %08x\n" me slot msg.msgsize msg.code in
      if Sys.opaque_identity log then print_string dbg;
      assert (Array.length msg.message = msg.msgsize);
      for i = 0 to msg.msgsize - 1 do
        assert (msg.message.(i) = msg.code)
      done;
      if msg.sender = 0 then incr from0;
    | Some _ -> ()
  done;
  ignore (Sys.opaque_identity !kept);
  !from0

let () =
  let iters = 200_000 in
  let d1 = Domain.spawn (fun () -> run 1 100 max_int) in
  let d2 = Domain.spawn (fun () -> run 2 5 max_int) in
  let recv_local = run 0 20 iters in
  assert (recv_local = 0);
  Atomic.set go false;
  let r = Domain.join d1 + Domain.join d2 in
  let remaining =
    table
    |> Array.to_list
    |> List.filter (fun x -> match Atomic.get x with Some {sender=0; _} -> true | _ -> false)
    |> List.length in
  Printf.printf "%d\n" (r+remaining)
