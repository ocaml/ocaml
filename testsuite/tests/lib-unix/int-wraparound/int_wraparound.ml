(* TEST
 include unix;
 hasunix;
 not target-windows;
 arch64;
 {
   bytecode;
 }{
   native;
 }
*)

(* Integers that do not fit in the C argument type must be rejected, not
   truncated modulo 2^32. *)

let big = 1 lsl 32

let rejects f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let tm year =
  { Unix.tm_sec = 0; tm_min = 0; tm_hour = 0; tm_mday = 1; tm_mon = 0;
    tm_year = year; tm_wday = 0; tm_yday = 0; tm_isdst = false }

let () =
  rejects (fun () -> Unix.getpwuid (big + Unix.getuid ()));
  rejects (fun () -> Unix.getgrgid (big + Unix.getgid ()));
  rejects (fun () -> Unix.setuid (big + Unix.getuid ()));
  rejects (fun () -> Unix.setgid (big + Unix.getgid ()));
  rejects (fun () -> Unix.initgroups "nobody" (big + Unix.getgid ()));
  rejects (fun () -> Sys.set_signal (big + 2) Sys.Signal_ignore);
  assert (Sys.signal_to_int (Sys.signal_of_int (big + 2)) = big + 2);
  rejects (fun () -> Unix.kill (big + Unix.getpid ()) 0);
  rejects (fun () -> Unix.kill (Unix.getpid ()) big);
  rejects (fun () -> Unix.waitpid [Unix.WNOHANG] big);
  rejects (fun () -> Unix.alarm big);
  rejects (fun () -> Unix.mktime (tm (big + 70)));

  (try ignore (Unix.getpwuid (Unix.getuid ())) with Not_found -> ());
  (try ignore (Unix.getgrgid (Unix.getgid ())) with Not_found -> ());
  Sys.set_signal Sys.sigusr1 Sys.Signal_ignore;
  ignore (Sys.signal_to_int Sys.sigusr1);
  (try ignore (Unix.waitpid [Unix.WNOHANG] (-1)) with Unix.Unix_error _ -> ());
  ignore (Unix.mktime (tm 70))
