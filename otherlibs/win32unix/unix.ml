(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Initialization *)

external startup: unit -> unit = "win_startup"
external cleanup: unit -> unit = "win_cleanup"

let _ = startup(); at_exit cleanup

(* Errors *)

type error =
    E2BIG
  | EACCESS
  | EAGAIN
  | EBADF
  | EBUSY
  | ECHILD
  | EDEADLK
  | EDOM
  | EEXIST
  | EFAULT
  | EFBIG
  | EINTR
  | EINVAL
  | EIO
  | EISDIR
  | EMFILE
  | EMLINK
  | ENAMETOOLONG
  | ENFILE
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOLCK
  | ENOMEM
  | ENOSPC
  | ENOSYS
  | ENOTDIR
  | ENOTEMPTY
  | ENOTTY
  | ENXIO
  | EPERM
  | EPIPE
  | ERANGE
  | EROFS
  | ESPIPE
  | ESRCH
  | EXDEV
  | EWOULDBLOCK
  | EINPROGRESS
  | EALREADY
  | ENOTSOCK
  | EDESTADDRREQ
  | EMSGSIZE
  | EPROTOTYPE
  | ENOPROTOOPT
  | EPROTONOSUPPORT
  | ESOCKTNOSUPPORT
  | EOPNOTSUPP
  | EPFNOSUPPORT
  | EAFNOSUPPORT
  | EADDRINUSE
  | EADDRNOTAVAIL
  | ENETDOWN
  | ENETUNREACH
  | ENETRESET
  | ECONNABORTED
  | ECONNRESET
  | ENOBUFS
  | EISCONN
  | ENOTCONN
  | ESHUTDOWN
  | ETOOMANYREFS
  | ETIMEDOUT
  | ECONNREFUSED
  | EHOSTDOWN
  | EHOSTUNREACH
  | ELOOP
  | EUNKNOWNERR

exception Unix_error of error * string * string

let _ = Callback.register_exception "Unix.Unix_error"
                                    (Unix_error(EUNKNOWNERR, "", ""))

external error_message : error -> string = "unix_error_message"

let handle_unix_error f arg =
  try
    f arg
  with Unix_error(err, fun_name, arg) ->
    prerr_string Sys.argv.(0);
    prerr_string ": \"";
    prerr_string fun_name;
    prerr_string "\" failed";
    if String.length arg > 0 then begin
      prerr_string " on \"";
      prerr_string arg;
      prerr_string "\""
    end;
    prerr_string ": ";
    prerr_endline (error_message err);
    exit 2

external environment : unit -> string array = "unix_environment"

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag =
    WNOHANG
  | WUNTRACED

type file_descr = int

external execv : string -> string array -> unit = "unix_execv"
external execve : string -> string array -> string array -> unit = "unix_execve"
external execvp : string -> string array -> unit = "unix_execvp"

external waitpid : wait_flag list -> int -> int * process_status
                 = "win_waitpid"
external getpid : unit -> int = "unix_getpid"

let stdin = 0
let stdout = 1
let stderr = 2

type open_flag =
    O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL
  | O_BINARY
  | O_TEXT

type file_perm = int

external openfile : string -> open_flag list -> file_perm -> file_descr
           = "unix_open"
external close : file_descr -> unit = "unix_close"
external unsafe_read : file_descr -> string -> int -> int -> int
                     = "unix_read"
external unsafe_write : file_descr -> string -> int -> int -> int
                      = "unix_write"

let read fd buf ofs len =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.read"
  else unsafe_read fd buf ofs len
let write fd buf ofs len =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.write"
  else unsafe_write fd buf ofs len

external in_channel_of_descr : file_descr -> in_channel = "open_descriptor"
external out_channel_of_descr : file_descr -> out_channel = "open_descriptor"
external descr_of_in_channel : in_channel -> file_descr = "channel_descriptor"
external descr_of_out_channel : out_channel -> file_descr = "channel_descriptor"

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

external lseek : file_descr -> int -> seek_command -> int = "unix_lseek"

type file_kind =
    S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

type stats =
  { st_dev : int;
    st_ino : int;
    st_kind : file_kind;
    st_perm : file_perm;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_size : int;
    st_atime : int;
    st_mtime : int;
    st_ctime : int }

external stat : string -> stats = "unix_stat"

external unlink : string -> unit = "unix_unlink"
external rename : string -> string -> unit = "unix_rename"

type access_permission =
    R_OK
  | W_OK
  | X_OK
  | F_OK

external access : string -> access_permission list -> unit = "unix_access"

external dup : file_descr -> file_descr = "unix_dup"
external dup2 : file_descr -> file_descr -> unit = "unix_dup2"

external set_close_on_exec : file_descr -> unit = "win_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit = "win_clear_close_on_exec"

external mkdir : string -> file_perm -> unit = "unix_mkdir"
external rmdir : string -> unit = "unix_rmdir"
external chdir : string -> unit = "unix_chdir"
external getcwd : unit -> string = "unix_getcwd"

type dir_entry =
    Dir_empty
  | Dir_read of string
  | Dir_toread

type dir_handle = { handle: int; mutable entry_read: dir_entry }

external findfirst : string -> string * int = "win_findfirst"
external findnext : int -> string= "win_findnext"

let opendir dirname =
  try
    let (first_entry, handle) = findfirst (dirname ^ "\\*.*") in
    { handle = handle; entry_read = Dir_read first_entry }
  with End_of_file ->
    { handle = 0; entry_read = Dir_empty }

let readdir d =
  match d.entry_read with
    Dir_empty -> raise End_of_file
  | Dir_read name -> d.entry_read <- Dir_toread; name
  | Dir_toread -> findnext d.handle

external win_findclose : int -> unit = "win_findclose"

let closedir d =
  match d.entry_read with
    Dir_empty -> ()
  | _ -> win_findclose d.handle

external pipe : unit -> file_descr * file_descr = "unix_pipe"

type tm =
  { tm_sec : int;
    tm_min : int;
    tm_hour : int;
    tm_mday : int;
    tm_mon : int;
    tm_year : int;
    tm_wday : int;
    tm_yday : int;
    tm_isdst : bool }

external time : unit -> int = "unix_time"
external gmtime : int -> tm = "unix_gmtime"
external localtime : int -> tm = "unix_localtime"
external mktime : tm -> int * tm = "unix_mktime"
external sleep : int -> unit = "unix_sleep"
external utimes : string -> int -> int -> unit = "unix_utimes"

let getlogin () =
  try Sys.getenv "USERNAME" with Not_found -> ""

type inet_addr

external inet_addr_of_string : string -> inet_addr
                                    = "unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr -> string
                                    = "unix_string_of_inet_addr"

let inet_addr_any = inet_addr_of_string "0.0.0.0"

type socket_domain =
    PF_UNIX
  | PF_INET

type socket_type =
    SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int

type shutdown_command =
    SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

type socket_option =
    SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE

external socket : socket_domain -> socket_type -> int -> file_descr
                                  = "unix_socket"

external accept : file_descr -> file_descr * sockaddr = "unix_accept"
external bind : file_descr -> sockaddr -> unit = "unix_bind"
external connect : file_descr -> sockaddr -> unit = "unix_connect"
external listen : file_descr -> int -> unit = "unix_listen"
external shutdown : file_descr -> shutdown_command -> unit = "unix_shutdown" 
external getsockname : file_descr -> sockaddr = "unix_getsockname"
external getpeername : file_descr -> sockaddr = "unix_getpeername"

external unsafe_recv :
  file_descr -> string -> int -> int -> msg_flag list -> int
                                  = "unix_recv"
external unsafe_recvfrom :
  file_descr -> string -> int -> int -> msg_flag list -> int * sockaddr
                                  = "unix_recvfrom"
external unsafe_send :
  file_descr -> string -> int -> int -> msg_flag list -> int
                                  = "unix_send"
external unsafe_sendto :
  file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int
                                  = "unix_sendto" "unix_sendto_native"

let recv fd buf ofs len flags =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.recv"
  else unsafe_recv fd buf ofs len flags
let recvfrom fd buf ofs len flags =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.recvfrom"
  else unsafe_recvfrom fd buf ofs len flags
let send fd buf ofs len flags =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.send"
  else unsafe_send fd buf ofs len flags
let sendto fd buf ofs len flags addr =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.sendto"
  else unsafe_sendto fd buf ofs len flags addr

external getsockopt : file_descr -> socket_option -> bool = "unix_getsockopt"
external setsockopt : file_descr -> socket_option -> bool -> unit
                                                          = "unix_setsockopt"


type host_entry =
  { h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array }

type protocol_entry =
  { p_name : string;
    p_aliases : string array;
    p_proto : int }

type service_entry =
  { s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string }

external gethostname : unit -> string = "unix_gethostname"
external gethostbyname : string -> host_entry = "unix_gethostbyname"
external gethostbyaddr : inet_addr -> host_entry = "unix_gethostbyaddr"
external getprotobyname : string -> protocol_entry
                                         = "unix_getprotobyname"
external getprotobynumber : int -> protocol_entry
                                         = "unix_getprotobynumber"

external getservbyname : string -> string -> service_entry
                                         = "unix_getservbyname"
external getservbyport : int -> string -> service_entry
                                         = "unix_getservbyport"

(* High-level process management (system, popen) *)

external win_create_process : string -> string -> string option ->
                              file_descr -> file_descr -> file_descr -> int
                            = "win_create_process" "win_create_process_native"

let create_process prog args fd1 fd2 fd3 =
  win_create_process prog (String.concat " " (Array.to_list args)) None
                     fd1 fd2 fd3

let create_process_env prog args env fd1 fd2 fd3 =
  win_create_process prog (String.concat " " (Array.to_list args))
                     (Some(String.concat "\000" (Array.to_list env) ^ "\000"))
                     fd1 fd2 fd3 

external system: string -> process_status = "win_system"

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output =
  let shell =
    try Sys.getenv "COMSPEC"
    with Not_found -> raise(Unix_error(ENOEXEC, "open_proc", cmd)) in
  let pid =
    create_process shell [|shell; "/c"; cmd|] input output stderr in
  Hashtbl.add popen_processes proc pid

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  set_close_on_exec in_read;
  open_proc cmd (Process_in inchan) stdin in_write;
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  let outchan = out_channel_of_descr out_write in
  set_close_on_exec out_write;
  open_proc cmd (Process_out outchan) out_read stdout;
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  set_close_on_exec in_read;
  set_close_on_exec out_write;
  open_proc cmd (Process(inchan, outchan)) out_read in_write;
  (inchan, outchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid [] pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid [] pid)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  snd(waitpid [] pid)

(* High-level network functions *)

let open_connection sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  connect sock sockaddr;
  (in_channel_of_descr sock, out_channel_of_descr sock)

let shutdown_connection inchan =
  shutdown (descr_of_in_channel inchan) SHUTDOWN_SEND

(* Dummy functions *)

let set_nonblock fd = ()
let clear_nonblock fd = ()

type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string }

type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array }

let getpwnam x = raise Not_found
let getgrnam = getpwnam
let getpwuid = getpwnam
let getgrgid = getpwnam

let getuid () = 1
let getgid () = 1
