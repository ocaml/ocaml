(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [ThreadUnix]: thread-compatible system calls *)

(* This module reimplements some of the functions from [Unix]
   so that they only block the calling thread, not all threads
   in the program, if they cannot complete immediately.
   See the documentation of the [Unix] module for more
   precise descriptions of the functions below. *)

(*** Process handling *)

val execv : string -> string array -> unit
val execve : string -> string array -> string array -> unit
val execvp : string -> string array -> unit
val wait : unit -> int * Unix.process_status
val waitpid : Unix.wait_flag list -> int -> int * Unix.process_status
val system : string -> Unix.process_status

(*** Basic input/output *)

val read : Unix.file_descr -> string -> int -> int -> int
val write : Unix.file_descr -> string -> int -> int -> int

(*** Input/output with timeout *)

val timed_read : Unix.file_descr -> string -> int -> int -> float -> int
val timed_write : Unix.file_descr -> string -> int -> int -> float -> int
      (* Behave as [read] and [write], except that 
         [Unix_error(ETIMEDOUT,_,_)] is raised if no data is
         available for reading or ready for writing after [d] seconds.
         The delay [d] is given in the fifth argument, in seconds. *)

(*** Pipes and redirections *)

val pipe : unit -> Unix.file_descr * Unix.file_descr
val open_process_out: string -> out_channel
val open_process: string -> in_channel * out_channel

(*** Time *)

val sleep : int -> unit

(*** Sockets *)

val socket : Unix.socket_domain -> Unix.socket_type -> int -> Unix.file_descr
val socketpair : Unix.socket_domain -> Unix.socket_type -> int ->
                 Unix.file_descr * Unix.file_descr
val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
val connect : Unix.file_descr -> Unix.sockaddr -> unit
val recv : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int
val recvfrom : Unix.file_descr -> string -> int -> int ->
               Unix.msg_flag list -> int * Unix.sockaddr
val send : Unix.file_descr -> string -> int -> int ->
           Unix.msg_flag list -> int
val sendto : Unix.file_descr -> string -> int -> int ->
             Unix.msg_flag list -> Unix.sockaddr -> int
val open_connection : Unix.sockaddr -> in_channel * out_channel
val establish_server :
      (in_channel -> out_channel -> 'a) -> Unix.sockaddr -> unit

