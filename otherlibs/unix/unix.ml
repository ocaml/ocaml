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

external register_unix_error: exn -> unit = "unix_register_error"

let _ = register_unix_error(Unix_error(EUNKNOWNERR, "", ""))

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

external execv : string -> string array -> unit = "unix_execv"
external execve : string -> string array -> string array -> unit = "unix_execve"
external execvp : string -> string array -> unit = "unix_execvp"
external fork : unit -> int = "unix_fork"
external wait : unit -> int * process_status = "unix_wait"
external waitpid : wait_flag list -> int -> int * process_status = "unix_waitpid"
external getpid : unit -> int = "unix_getpid"
external getppid : unit -> int = "unix_getppid"
external nice : int -> int = "unix_nice"

type file_descr = int

let stdin = 0
let stdout = 1
let stderr = 2

let max_opened_descr = ref 2

let record_descr fd =
  if fd > !max_opened_descr then max_opened_descr := fd

type open_flag =
    O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL

type file_perm = int


external sys_openfile : string -> open_flag list -> file_perm -> file_descr
           = "unix_open"

let openfile name flags perm =
  let fd = sys_openfile name flags perm in
  record_descr fd;
  fd

external close : file_descr -> unit = "unix_close"
external unsafe_read : file_descr -> string -> int -> int -> int = "unix_read"
external unsafe_write : file_descr -> string -> int -> int -> int = "unix_write"

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
external truncate : string -> int -> unit = "unix_truncate"
external ftruncate : file_descr -> int -> unit = "unix_ftruncate"

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
external lstat : string -> stats = "unix_lstat"
external fstat : file_descr -> stats = "unix_fstat"
external unlink : string -> unit = "unix_unlink"
external rename : string -> string -> unit = "unix_rename"
external link : string -> string -> unit = "unix_link"

type access_permission =
    R_OK
  | W_OK
  | X_OK
  | F_OK

external chmod : string -> file_perm -> unit = "unix_chmod"
external fchmod : file_descr -> file_perm -> unit = "unix_fchmod"
external chown : string -> int -> int -> unit = "unix_chown"
external fchown : file_descr -> int -> int -> unit = "unix_fchown"
external umask : int -> int = "unix_umask"
external access : string -> access_permission list -> unit = "unix_access"

external sys_dup : file_descr -> file_descr = "unix_dup"

let dup fd =
  let newfd = sys_dup fd in record_descr newfd; newfd

external dup2 : file_descr -> file_descr -> unit = "unix_dup2"
external set_nonblock : file_descr -> unit = "unix_set_nonblock"
external clear_nonblock : file_descr -> unit = "unix_clear_nonblock"
external set_close_on_exec : file_descr -> unit = "unix_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit = "unix_clear_close_on_exec"

external mkdir : string -> file_perm -> unit = "unix_mkdir"
external rmdir : string -> unit = "unix_rmdir"
external chdir : string -> unit = "unix_chdir"
external getcwd : unit -> string = "unix_getcwd"

type dir_handle

external opendir : string -> dir_handle = "unix_opendir"
external readdir : dir_handle -> string = "unix_readdir"
external rewinddir : dir_handle -> unit = "unix_rewinddir"
external closedir : dir_handle -> unit = "unix_closedir"

external sys_pipe : unit -> file_descr * file_descr = "unix_pipe"

let pipe () =
  let (fd1, fd2 as fdpair) = sys_pipe() in
  record_descr fd1; record_descr fd2; fdpair

external symlink : string -> string -> unit = "unix_symlink"
external readlink : string -> string = "unix_readlink"
external mkfifo : string -> file_perm -> unit = "unix_mkfifo"
external ioctl_int : file_descr -> int -> int -> int = "unix_ioctl_int"
external ioctl_ptr : file_descr -> int -> string -> int = "unix_ioctl_ptr"
external select :
  file_descr list -> file_descr list -> file_descr list -> float ->
        file_descr list * file_descr list * file_descr list = "unix_select"

type lock_command =
    F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST

external lockf : file_descr -> lock_command -> int -> unit = "unix_lockf"
external kill : int -> int -> unit = "unix_kill"
external pause : unit -> unit = "unix_pause"

type process_times =
  { tms_utime : float;
    tms_stime : float;
    tms_cutime : float;
    tms_cstime : float }

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
external gettimeofday : unit -> float = "unix_gettimeofday"
external gmtime : int -> tm = "unix_gmtime"
external localtime : int -> tm = "unix_localtime"
external mktime : tm -> int * tm = "unix_mktime"
external alarm : int -> int = "unix_alarm"
external sleep : int -> unit = "unix_sleep"
external times : unit -> process_times =
              "unix_times_bytecode" "unix_times_native"
external utimes : string -> int -> int -> unit = "unix_utimes"

type interval_timer =
    ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF

type interval_timer_status =
  { it_interval: float;                 (* Period *)
    it_value: float }                   (* Current value of the timer *)

external getitimer: interval_timer -> interval_timer_status = "unix_getitimer"
external setitimer:
  interval_timer -> interval_timer_status -> interval_timer_status
  = "unix_setitimer"

external getuid : unit -> int = "unix_getuid"
external geteuid : unit -> int = "unix_geteuid"
external setuid : int -> unit = "unix_setuid"
external getgid : unit -> int = "unix_getgid"
external getegid : unit -> int = "unix_getegid"
external setgid : int -> unit = "unix_setgid"
external getgroups : unit -> int array = "unix_getgroups"

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


external getlogin : unit -> string = "unix_getlogin"
external getpwnam : string -> passwd_entry = "unix_getpwnam"
external getgrnam : string -> group_entry = "unix_getgrnam"
external getpwuid : int -> passwd_entry = "unix_getpwuid"
external getgrgid : int -> group_entry = "unix_getgrgid"

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

external sys_socket : socket_domain -> socket_type -> int -> file_descr
                                  = "unix_socket"
let socket domain typ proto =
  let fd = sys_socket domain typ proto in
  record_descr fd; fd

external sys_socketpair :
        socket_domain -> socket_type -> int -> file_descr * file_descr
                                  = "unix_socketpair"
let socketpair domain typ proto =
  let (fd1, fd2 as fdpair) = sys_socketpair domain typ proto in
  record_descr fd1; record_descr fd2; fdpair

external sys_accept : file_descr -> file_descr * sockaddr = "unix_accept"

let accept fd =
  let (newfd, addr as result) = sys_accept fd in
  record_descr newfd; result

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
type terminal_io = {
    mutable c_ignbrk: bool;
    mutable c_brkint: bool;
    mutable c_ignpar: bool;
    mutable c_parmrk: bool;
    mutable c_inpck: bool;
    mutable c_istrip: bool;
    mutable c_inlcr: bool;
    mutable c_igncr: bool;
    mutable c_icrnl: bool;
    mutable c_ixon: bool;
    mutable c_ixoff: bool;
    mutable c_opost: bool;
    mutable c_obaud: int;
    mutable c_ibaud: int;
    mutable c_csize: int;
    mutable c_cstopb: int;
    mutable c_cread: bool;
    mutable c_parenb: bool;
    mutable c_parodd: bool;
    mutable c_hupcl: bool;
    mutable c_clocal: bool;
    mutable c_isig: bool;
    mutable c_icanon: bool;
    mutable c_noflsh: bool;
    mutable c_echo: bool;
    mutable c_echoe: bool;
    mutable c_echok: bool;
    mutable c_echonl: bool;
    mutable c_vintr: char;
    mutable c_vquit: char;
    mutable c_verase: char;
    mutable c_vkill: char;
    mutable c_veof: char;
    mutable c_veol: char;
    mutable c_vmin: int;
    mutable c_vtime: int;
    mutable c_vstart: char;
    mutable c_vstop: char
  }

external tcgetattr: file_descr -> terminal_io = "unix_tcgetattr"

type setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

external tcsetattr: file_descr -> setattr_when -> terminal_io -> unit
               = "unix_tcsetattr"
external tcsendbreak: file_descr -> int -> unit = "unix_tcsendbreak"
external tcdrain: file_descr -> unit = "unix_tcdrain"

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH

external tcflush: file_descr -> flush_queue -> unit = "unix_tcflush"

type flow_action = TCOOFF | TCOON | TCIOFF | TCION

external tcflow: file_descr -> flow_action -> unit = "unix_tcflow"

(* High-level process management (system, popen) *)

let closeall () =
  for i = 3 to !max_opened_descr do close i done

let system cmd =
  match fork() with
     0 -> closeall();
          execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
          exit 127
  | id -> snd(waitpid [] id)

let create_process cmd args new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      if new_stdin  <> stdin  then dup2 new_stdin stdin;
      if new_stdout <> stdout then dup2 new_stdout stdout;
      if new_stderr <> stderr then dup2 new_stderr stderr;
      closeall();
      execvp cmd args;
      exit 127
  | id -> id

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output =
  let pid =
    create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] input output stderr in
  Hashtbl.add popen_processes proc pid

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) stdin in_write;
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read stdout;
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write;
  close in_write;
  close out_read;
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
            close_out outchan
    | id -> close s; waitpid [] id (* Reclaim the son *); ()
  done
