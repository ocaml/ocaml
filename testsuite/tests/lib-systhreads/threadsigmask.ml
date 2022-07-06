(* TEST

* hassysthreads
include systhreads
** not-windows
*** bytecode
*** native
*)

let stopped = ref false

(* This function is purposed to do some computations which allocate,
   so that the corresponding thread is likely to handle signals if it
   is allowed to. *)
let rec loop () =
  let rec generate_list n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (float n :: acc) (n-1)
    in
    aux [] n
  in
  let long_list = generate_list 100000 in
  let res = List.length (List.rev_map sin long_list) in
  ignore (Sys.opaque_identity res)

let thread s =
  ignore (Thread.sigmask Unix.SIG_UNBLOCK [s]);
  while not !stopped do loop () done

let handler tid_exp cnt signal =
  incr cnt;
  if Thread.id (Thread.self ()) != !tid_exp then
    Printf.printf "Signal received in an unexpected thread !\n"

let _ =
  ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigusr1; Sys.sigusr2]);

  (* Install the signal handlers *)
  let (tid1, tid2) = (ref 0, ref 0) in
  let (cnt1, cnt2) = (ref 0, ref 0) in
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (handler tid1 cnt1));
  Sys.set_signal Sys.sigusr2 (Sys.Signal_handle (handler tid2 cnt2));

  (* Spawn the other thread and unblock sigusr2 in the main thread *)
  let t1 = Thread.create thread Sys.sigusr1 in
  let t2 = Thread.self () in
  ignore (Thread.sigmask Unix.SIG_UNBLOCK [Sys.sigusr2]);
  tid1 := Thread.id t1;
  tid2 := Thread.id t2;

  (* Send signals to the current process. They should be received by the
     correct respective threads. *)
  let pid = Unix.getpid () in
  let cntsent = ref 0 in
  (* We loop until each thread has received at least 5 signals and we
    have sent more than 50 signals in total. We do not check that all
    signals get handled, because they could be missed because of the
    lack of fairness of the scheduler. *)
  while !cntsent < 50 || !cnt1 < 5 || !cnt2 < 5 do
    Unix.kill pid Sys.sigusr1;
    Unix.kill pid Sys.sigusr2;
    incr cntsent;
    Thread.delay 0.05;

    (* Still, if too many signals have been sent, we interrupt the
       test to avoid a timeout. *)
    if !cntsent > 2000 then begin
        stopped := true;
        Thread.join t1;
        Printf.printf "A thread does not receive signals. %d %d %d\n" !cnt1 !cnt2 !cntsent;
        exit 0
    end
  done;

  (* Join worker thread *)
  stopped := true;
  Thread.join t1;

  Printf.printf "OK\n"
