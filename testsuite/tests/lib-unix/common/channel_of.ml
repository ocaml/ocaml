(* TEST
* hasunix
include unix
** bytecode
** native
*)

open Printf

let shouldpass msg fn arg =
  try
    ignore (fn arg); printf "%s: passed (no error)\n" msg
  with Unix.Unix_error(err, _, _) ->
    printf "%s: FAILED (error %s)\n" msg (Unix.error_message err)

let shouldfail msg fn arg =
  try
    ignore (fn arg); printf "%s: FAILED (no error raised)\n" msg
  with Unix.Unix_error(err, _, _) ->
    printf "%s: passed (error raised)\n" msg

let _ =
  (* Files *)
  begin
    let fd = Unix.(openfile "file.tmp"
                            [O_WRONLY;O_CREAT;O_TRUNC;O_SHARE_DELETE] 0o666) in
    shouldpass "File 1" Unix.in_channel_of_descr fd;
    shouldpass "File 2" Unix.out_channel_of_descr fd;
    Unix.close fd
  end;
  (* Pipes *)
  begin
    let out, inp = Unix.pipe () in
    shouldpass "Pipe 1" Unix.in_channel_of_descr out;
    shouldpass "Pipe 2" Unix.out_channel_of_descr inp;
    Unix.close out; Unix.close inp
  end;
  (* Sockets *)
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
  begin
    let sock =
      Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
    shouldpass "Stream socket 1" Unix.in_channel_of_descr sock;
    shouldpass "Stream socket 2" Unix.out_channel_of_descr sock;
    Unix.close sock
  end;
  begin
    let sock =
      Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_DGRAM 0 in
    shouldfail "Datagram socket 1" Unix.in_channel_of_descr sock;
   shouldfail "Datagram socket 2" Unix.out_channel_of_descr sock;
    Unix.close sock
  end;
  (* Whatever is connected to standard descriptors; hopefully a terminal *)
  begin
    shouldpass "stdin" Unix.in_channel_of_descr Unix.stdin;
    shouldpass "stderr" Unix.out_channel_of_descr Unix.stderr
  end;
  (* A closed file descriptor should now fail *)
  begin
    let fd = Unix.(openfile "file.tmp"
                            [O_WRONLY;O_CREAT;O_TRUNC;O_SHARE_DELETE] 0o666) in
    Unix.close fd;
    shouldfail "Closed file 1" Unix.in_channel_of_descr fd;
    shouldfail "Closed file 2" Unix.out_channel_of_descr fd
  end;
  Sys.remove "file.tmp";
  (* Send something to stdout, but don't flush and don't close the channel.
     This tests proper auto-flushing at exit of channels created by
     Unix.out_channel_of_descr.  (PR#11384) *)
  flush stdout;
  let oc = Unix.out_channel_of_descr Unix.stdout in
  output_string oc "Test completed normally\n"
  (* End of test *)
