(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [ThreadUnix]: thread-compatible system calls *)

(* This module is deprecated: its functionality has been merged back into
   the [Unix] module.  Threaded programs can now call the functions
   from module [Unix] directly, and still get the correct behavior
   (block the calling thread, if required, but do not block all threads
   in the process).  *)

(*** Process handling *)

val execv : prog:string -> args:string array -> unit
val execve : prog:string -> args:string array -> env:string array -> unit
val execvp : prog:string -> args:string array -> unit
val wait : unit -> int * Unix.process_status
val waitpid : mode:Unix.wait_flag list -> int -> int * Unix.process_status
val system : string -> Unix.process_status

(*** Basic input/output *)

val read : Unix.file_descr -> buf:string -> pos:int -> len:int -> int
val write : Unix.file_descr -> buf:string -> pos:int -> len:int -> int

(*** Input/output with timeout *)

val timed_read :
      Unix.file_descr ->
      buf:string -> pos:int -> len:int -> timeout:float -> int
val timed_write :
      Unix.file_descr ->
      buf:string -> pos:int -> len:int -> timeout:float -> int
      (* Behave as [read] and [write], except that
         [Unix_error(ETIMEDOUT,_,_)] is raised if no data is
         available for reading or ready for writing after [d] seconds.
         The delay [d] is given in the fifth argument, in seconds. *)

(*** Polling *)

val select :
  read:Unix.file_descr list -> write:Unix.file_descr list ->
  except:Unix.file_descr list -> timeout:float ->
        Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

(*** Pipes and redirections *)

val pipe : unit -> Unix.file_descr * Unix.file_descr
val open_process_in: string -> in_channel
val open_process_out: string -> out_channel
val open_process: string -> in_channel * out_channel
val open_process_full:
      string -> env:string array -> in_channel * out_channel * in_channel

(*** Time *)

val sleep : int -> unit

(*** Sockets *)

val socket : domain:Unix.socket_domain ->
             kind:Unix.socket_type -> protocol:int -> Unix.file_descr
val socketpair : domain:Unix.socket_domain -> kind:Unix.socket_type ->
                 protocol:int -> Unix.file_descr * Unix.file_descr
val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
val connect : Unix.file_descr -> addr:Unix.sockaddr -> unit
val recv : Unix.file_descr -> buf:string ->
           pos:int -> len:int -> mode:Unix.msg_flag list -> int
val recvfrom : Unix.file_descr -> buf:string -> pos:int -> len:int ->
               mode:Unix.msg_flag list -> int * Unix.sockaddr
val send : Unix.file_descr -> buf:string -> pos:int -> len:int ->
           mode:Unix.msg_flag list -> int
val sendto : Unix.file_descr -> buf:string -> pos:int -> len:int ->
             mode:Unix.msg_flag list -> addr:Unix.sockaddr -> int
val open_connection : Unix.sockaddr -> in_channel * out_channel
val establish_server :
      (in_channel -> out_channel -> unit) ->
      addr:Unix.sockaddr -> unit
