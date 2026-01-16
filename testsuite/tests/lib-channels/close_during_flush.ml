(* TEST
 not-windows;
 include unix;
 hasunix;
 native;
*)

let () =
  let rd, wr = Unix.pipe () in
  (* create a full pipe (so that writes block) *)
  Unix.set_nonblock wr;
  let buf = Bytes.make 1000 '!' in
  begin match
    for i = 1 to 10_000 do
      ignore (Unix.write wr buf 0 1000)
    done
  with
  | () -> failwith "pipe doesn't seem to fill on this OS?!"
  | exception Unix.Unix_error((EAGAIN|EWOULDBLOCK), _, _) -> ()
  end;

  (* block in a write, then unblock & close from a signal handler *)
  Unix.clear_nonblock wr;
  let ch = Unix.out_channel_of_descr wr in
  output ch buf 0 1000;
  let alarm_handler _ =
    (* clear some space *)
    Unix.read rd (Bytes.make 100_000 '?') 0 100_000 |> ignore;
    close_out ch
  in
  Sys.signal Sys.sigalrm (Signal_handle alarm_handler) |> ignore;
  Unix.alarm 1 |> ignore;
  flush ch;
  print_endline "ok"
