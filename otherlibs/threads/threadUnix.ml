(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [ThreadUnix]: thread-compatible system calls *)

open Unix

(*** Process handling *)

(* Disable the timer interrupt before doing exec, because some OS
   keep sending timer interrupts to the exec'ed code.
   Make sure we're not preempted just after disabling the timer... *)
let execv proc args =
  Thread.critical_section := true;
  let _ = Unix.setitimer ITIMER_VIRTUAL {it_interval = 0.0; it_value = 0.0} in
  Unix.execv proc args

let execve proc args env =
  Thread.critical_section := true;
  let _ = Unix.setitimer ITIMER_VIRTUAL {it_interval = 0.0; it_value = 0.0} in
  Unix.execve proc args env

let execvp proc args =
  Thread.critical_section := true;
  let _ = Unix.setitimer ITIMER_VIRTUAL {it_interval = 0.0; it_value = 0.0} in
  Unix.execvp proc args

let wait () =
  Thread.wait_pid (-1)
  
let waitpid flags pid =
  if List.mem WNOHANG flags
  then Unix.waitpid flags pid
  else Thread.wait_pid pid

let system cmd =
  match fork() with
     0 -> execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]; exit 127
  | id -> snd(waitpid [] id)

(*** File I/O *)

let rec read fd buff ofs len =
  Thread.wait_read fd;
  try Unix.read fd buff ofs len
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) -> read fd buff ofs len

let rec write fd buff ofs len =
  Thread.wait_write fd;
  try Unix.write fd buff ofs len
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) -> write fd buff ofs len

let rec timed_read fd buff ofs len timeout =
  if Thread.wait_timed_read fd timeout
  then begin try Unix.read fd buff ofs len
             with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
                    timed_read fd buff ofs len timeout
       end
  else raise (Unix_error(ETIMEDOUT, "timed_read", ""))

let rec timed_write fd buff ofs len timeout =
  if Thread.wait_timed_write fd timeout
  then begin try Unix.write fd buff ofs len
             with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
                    timed_write fd buff ofs len timeout
       end
  else raise (Unix_error(ETIMEDOUT, "timed_write", ""))

let select = Thread.select

(*** Pipes *)

let pipe() =
  let (out_fd, in_fd as fd_pair) = Unix.pipe() in
  Unix.set_nonblock in_fd;
  fd_pair

let open_process_out cmd =
  let oc = Unix.open_process_out cmd in
  Unix.set_nonblock(Unix.descr_of_out_channel oc);
  oc

let open_process cmd =
  let (ic, oc as channels) = Unix.open_process cmd in
  Unix.set_nonblock(Unix.descr_of_out_channel oc);
  channels

(*** Time *)

let sleep secs =
  Thread.delay (float secs)

(*** Sockets *)

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  Unix.set_nonblock s;
  s

let socketpair dom typ proto =
  let (s1, s2 as spair) = Unix.socketpair dom typ proto in
  Unix.set_nonblock s1; Unix.set_nonblock s2;
  spair

let rec accept req =
  Thread.wait_read req;
  try
    let (s, caller as result) = Unix.accept req in
    Unix.set_nonblock s;
    result
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) -> accept req

let connect s addr =
  try
    Unix.connect s addr
  with Unix_error((EINPROGRESS | EWOULDBLOCK | EAGAIN), _, _) ->
    Thread.wait_write s;
    (* Check if it really worked *)
    let _ = Unix.getpeername s in
    ()

let rec recv fd buf ofs len flags =
  Thread.wait_read fd;
  try Unix.recv fd buf ofs len flags
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) -> recv fd buf ofs len flags

let rec recvfrom fd buf ofs len flags =
  Thread.wait_read fd;
  try Unix.recvfrom fd buf ofs len flags
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
              recvfrom fd buf ofs len flags

let rec send fd buf ofs len flags =
  Thread.wait_write fd;
  try Unix.send fd buf ofs len flags
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) -> send fd buf ofs len flags
  
let rec sendto fd buf ofs len flags addr =
  Thread.wait_write fd;
  try Unix.sendto fd buf ofs len flags addr
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
              sendto fd buf ofs len flags addr

let open_connection sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  connect sock sockaddr;
  (in_channel_of_descr sock, out_channel_of_descr sock)

let establish_server server_fun sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 3;
  while true do
    let (s, caller) = accept sock in
    (* The "double fork" trick, the process which calls server_fun will not
       leave a zombie process *)
    match fork() with
       0 -> if fork() <> 0 then exit 0; (* The son exits, the grandson works *)
            let inchan = in_channel_of_descr s in
            let outchan = out_channel_of_descr s in
            server_fun inchan outchan;
            close_in inchan;
            close_out outchan;
            exit 0
    | id -> close s; let _ = waitpid [] id (* Reclaim the son *) in ()
  done

