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
  (* Errors defined in the POSIX standard *)
    E2BIG               (* Argument list too long *)
  | EACCES              (* Permission denied *)
  | EAGAIN              (* Resource temporarily unavailable; try again *)
  | EBADF               (* Bad file descriptor *)
  | EBUSY               (* Resource unavailable *)
  | ECHILD              (* No child process *)
  | EDEADLK             (* Resource deadlock would occur *)
  | EDOM                (* Domain error for math functions, etc. *)
  | EEXIST              (* File exists *)
  | EFAULT              (* Bad address *)
  | EFBIG               (* File too large *)
  | EINTR               (* Function interrupted by signal *)
  | EINVAL              (* Invalid argument *)
  | EIO                 (* Hardware I/O error *)
  | EISDIR              (* Is a directory *)
  | EMFILE              (* Too many open files by the process *)
  | EMLINK              (* Too many links *)
  | ENAMETOOLONG        (* Filename too long *)
  | ENFILE              (* Too many open files in the system *)
  | ENODEV              (* No such device *)
  | ENOENT              (* No such file or directory *)
  | ENOEXEC             (* Not an executable file *)
  | ENOLCK              (* No locks available *)
  | ENOMEM              (* Not enough memory *)
  | ENOSPC              (* No space left on device *)
  | ENOSYS              (* Function not supported *)
  | ENOTDIR             (* Not a directory *)
  | ENOTEMPTY           (* Directory not empty *)
  | ENOTTY              (* Inappropriate I/O control operation *)
  | ENXIO               (* No such device or address *)
  | EPERM               (* Operation not permitted *)
  | EPIPE               (* Broken pipe *)
  | ERANGE              (* Result too large *)
  | EROFS               (* Read-only file system *)
  | ESPIPE              (* Invalid seek e.g. on a pipe *)
  | ESRCH               (* No such process *)
  | EXDEV               (* Invalid link *)
  (* Additional errors, mostly BSD *)
  | EWOULDBLOCK         (* Operation would block *)
  | EINPROGRESS         (* Operation now in progress *)
  | EALREADY            (* Operation already in progress *)
  | ENOTSOCK            (* Socket operation on non-socket *)
  | EDESTADDRREQ        (* Destination address required *)
  | EMSGSIZE            (* Message too long *)
  | EPROTOTYPE          (* Protocol wrong type for socket *)
  | ENOPROTOOPT         (* Protocol not available *)
  | EPROTONOSUPPORT     (* Protocol not supported *)
  | ESOCKTNOSUPPORT     (* Socket type not supported *)
  | EOPNOTSUPP          (* Operation not supported on socket *)
  | EPFNOSUPPORT        (* Protocol family not supported *)
  | EAFNOSUPPORT        (* Address family not supported by protocol family *)
  | EADDRINUSE          (* Address already in use *)
  | EADDRNOTAVAIL       (* Can't assign requested address *)
  | ENETDOWN            (* Network is down *)
  | ENETUNREACH         (* Network is unreachable *)
  | ENETRESET           (* Network dropped connection on reset *)
  | ECONNABORTED        (* Software caused connection abort *)
  | ECONNRESET          (* Connection reset by peer *)
  | ENOBUFS             (* No buffer space available *)
  | EISCONN             (* Socket is already connected *)
  | ENOTCONN            (* Socket is not connected *)
  | ESHUTDOWN           (* Can't send after socket shutdown *)
  | ETOOMANYREFS        (* Too many references: can't splice *)
  | ETIMEDOUT           (* Connection timed out *)
  | ECONNREFUSED        (* Connection refused *)
  | EHOSTDOWN           (* Host is down *)
  | EHOSTUNREACH        (* No route to host *)
  | ELOOP               (* Too many levels of symbolic links *)
  (* All other errors are mapped to EUNKNOWNERR *)
  | EUNKNOWNERR of int  (* Unknown error *)

exception Unix_error of error * string * string

let _ = Callback.register_exception "Unix.Unix_error"
                                    (Unix_error(E2BIG, "", ""))

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
external getenv: string -> string = "sys_getenv"
external putenv: string -> string -> unit = "unix_putenv"

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag =
    WNOHANG
  | WUNTRACED

type file_descr

external execv : string -> string array -> unit = "unix_execv"
external execve : string -> string array -> string array -> unit = "unix_execve"
external execvp : string -> string array -> unit = "unix_execvp"
external execvpe : string -> string array -> string array -> unit = "unix_execvpe"

external waitpid : wait_flag list -> int -> int * process_status
                 = "win_waitpid"
external getpid : unit -> int = "unix_getpid"

let fork () = invalid_arg "Unix.fork not implemented"
let wait () = invalid_arg "Unix.wait not implemented" 
let getppid () = invalid_arg "Unix.getppid not implemented"
let nice prio = invalid_arg "Unix.nice not implemented"

(* Basic file input/output *)

type standard_handle = STD_INPUT | STD_OUTPUT | STD_ERROR

external stdhandle : standard_handle -> file_descr = "win_stdhandle"

let stdin = stdhandle STD_INPUT
let stdout = stdhandle STD_OUTPUT
let stderr = stdhandle STD_ERROR

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

(* Interfacing with the standard input/output library *)

external open_read_descriptor : int -> in_channel = "caml_open_descriptor"
external open_write_descriptor : int -> out_channel = "caml_open_descriptor"
external fd_of_in_channel : in_channel -> int = "channel_descriptor"
external fd_of_out_channel : out_channel -> int = "channel_descriptor"

external open_handle : file_descr -> int = "win_fd_handle"
external filedescr_of_fd : int -> file_descr = "win_handle_fd"

let in_channel_of_descr handle =
  open_read_descriptor(open_handle handle)
let out_channel_of_descr handle =
  open_write_descriptor(open_handle handle)

let descr_of_in_channel inchan =
  filedescr_of_fd(fd_of_in_channel inchan)
let descr_of_out_channel outchan =
  filedescr_of_fd(fd_of_out_channel outchan)

(* Seeking and truncating *)

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

external lseek : file_descr -> int -> seek_command -> int = "unix_lseek"

let truncate name len = invalid_arg "Unix.truncate not implemented"
let ftruncate fd len = invalid_arg "Unix.ftruncate not implemented"

(* File statistics *)

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
    st_atime : float;
    st_mtime : float;
    st_ctime : float }

external stat : string -> stats = "unix_stat"
let lstat = stat
let fstat fd = invalid_arg "Unix.fstat not implemented"

(* Operations on file names *)

external unlink : string -> unit = "unix_unlink"
external rename : string -> string -> unit = "unix_rename"
let link f1 f2 = invalid_arg "Unix.link not implemented"

(* File permissions and ownership *)

type access_permission =
    R_OK
  | W_OK
  | X_OK
  | F_OK

let chmod file perm = invalid_arg "Unix.chmod not implemented"
let fchmod fd perm = invalid_arg "Unix.fchmod not implemented"
let chown file perm = invalid_arg "Unix.chown not implemented"
let fchown fd perm = invalid_arg "Unix.fchown not implemented"
let umask msk = invalid_arg "Unix.umask not implemented"

external access : string -> access_permission list -> unit = "unix_access"

(* Operations on file descriptors *)

external dup : file_descr -> file_descr = "unix_dup"
external dup2 : file_descr -> file_descr -> unit = "unix_dup2"

let set_nonblock fd = ()
let clear_nonblock fd = ()

external set_close_on_exec : file_descr -> unit = "win_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit = "win_clear_close_on_exec"

(* Directories *)

external mkdir : string -> file_perm -> unit = "unix_mkdir"
external rmdir : string -> unit = "unix_rmdir"
external chdir : string -> unit = "unix_chdir"
external getcwd : unit -> string = "unix_getcwd"
let chroot _ = invalid_arg "Unix.chroot not implemented"

type dir_entry =
    Dir_empty
  | Dir_read of string
  | Dir_toread

type dir_handle =
  { dirname: string; mutable handle: int; mutable entry_read: dir_entry }

external findfirst : string -> string * int = "win_findfirst"
external findnext : int -> string= "win_findnext"

let opendir dirname =
  try
    let (first_entry, handle) = findfirst (dirname ^ "\\*.*") in
    { dirname = dirname; handle = handle; entry_read = Dir_read first_entry }
  with End_of_file ->
    { dirname = dirname; handle = 0; entry_read = Dir_empty }

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

let rewinddir d =
  closedir d;
  try
    let (first_entry, handle) = findfirst (d.dirname ^ "\\*.*") in
    d.handle <- handle; d.entry_read <- Dir_read first_entry
  with End_of_file ->
    d.handle <- 0; d.entry_read <- Dir_empty

(* Pipes *)

external pipe : unit -> file_descr * file_descr = "unix_pipe"

let mkfifo name perm = invalid_arg "Unix.mkfifo not implemented"

(* Symbolic links *)

let readlink path = invalid_arg "Unix.readlink not implemented"
let symlink path1 path2 = invalid_arg "Unix.symlink not implemented"

(* Locking *)

type lock_command =
    F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK

let lockf fd cmd exten = invalid_arg "Unix.lockf not implemented"
let kill pid signo = invalid_arg "Unix.kill not implemented"
type sigprocmask_command = SIG_SETMASK | SIG_BLOCK | SIG_UNBLOCK
let sigprocmask cmd sigs = invalid_arg "Unix.sigprocmask not implemented"
let sigpending () = invalid_arg "Unix.sigpending not implemented"
let sigsuspend sigs = invalid_arg "Unix.sigsuspend not implemented"
let pause () = invalid_arg "Unix.pause not implemented"

(* Time functions *)

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

external time : unit -> float = "unix_time"
external gettimeofday : unit -> float = "unix_gettimeofday"
external gmtime : float -> tm = "unix_gmtime"
external localtime : float -> tm = "unix_localtime"
external mktime : tm -> float * tm = "unix_mktime"
let alarm n = invalid_arg "Unix.alarm not implemented"
external sleep : int -> unit = "unix_sleep"
let times () =
  { tms_utime = Sys.time(); tms_stime = 0.0;
    tms_cutime = 0.0; tms_cstime = 0.0 }
external utimes : string -> float -> float -> unit = "unix_utimes"

type interval_timer =
    ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF

type interval_timer_status =
  { it_interval: float;
    it_value: float }

let getitimer it = invalid_arg "Unix.getitimer not implemented"
let setitimer it tm = invalid_arg "Unix.setitimer not implemented"

(* User id, group id *)

let getuid () = 1
let geteuid = getuid
let setuid id = invalid_arg "Unix.setuid not implemented"

let getgid () = 1
let getegid = getgid
let setgid id = invalid_arg "Unix.setgid not implemented"

let getgroups () = [|1|]

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

let getlogin () = try Sys.getenv "USERNAME" with Not_found -> ""
let getpwnam x = raise Not_found
let getgrnam = getpwnam
let getpwuid = getpwnam
let getgrgid = getpwnam

(* Internet addresses *)

type inet_addr

external inet_addr_of_string : string -> inet_addr
                                    = "unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr -> string
                                    = "unix_string_of_inet_addr"

let inet_addr_any = inet_addr_of_string "0.0.0.0"

(* Sockets *)

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

external socket_internal :
  socket_domain -> socket_type -> int -> bool -> file_descr
  = "unix_socket"

let socket dom typ proto = socket_internal dom typ proto true
let async_socket dom typ proto = socket_internal dom typ proto false
let socketpair dom ty proto = invalid_arg "Unix.socketpair not implemented"

external accept_internal :
  file_descr -> bool -> file_descr * sockaddr = "unix_accept"
let accept s = accept_internal s true
let async_accept s = accept_internal s false
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
(* Host and protocol databases *)

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

(* Polling *)

external select :
  file_descr list -> file_descr list -> file_descr list -> float ->
        file_descr list * file_descr list * file_descr list = "unix_select"

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
  invalid_arg "Unix.establish_server not implmented"

(* Terminal interface *)

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

type setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

let tcgetattr fd = invalid_arg "Unix.tcgetattr not implemented"
let tcsetattr fd wh = invalid_arg "Unix.tcsetattr not implemented"
let tcsendbreak fd n = invalid_arg "Unix.tcsendbreak not implemented"
let tcdrain fd = invalid_arg "Unix.tcdrain not implemented"

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH
let tcflush fd q = invalid_arg "Unix.tcflush not implemented"
type flow_action = TCOOFF | TCOON | TCIOFF | TCION
let tcflow fd fl = invalid_arg "Unix.tcflow not implemented"
let setsid () = invalid_arg "Unix.setsid not implemented"
