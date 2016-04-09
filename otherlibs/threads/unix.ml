(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* An alternate implementation of the Unix module from ../unix
   which is safe in conjunction with bytecode threads. *)

(* Type definitions that matter for thread operations *)

type file_descr = int

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

(* We can't call functions from Thread because of type circularities,
   so we redefine here the functions that we need *)

type resumption_status =
    Resumed_wakeup
  | Resumed_delay
  | Resumed_join
  | Resumed_io
  | Resumed_select of file_descr list * file_descr list * file_descr list
  | Resumed_wait of int * process_status

(* to avoid warning *)
let _ = [Resumed_wakeup; Resumed_delay; Resumed_join;
         Resumed_io; Resumed_select ([], [], []);
         Resumed_wait (0, WEXITED 0)]

external thread_initialize : unit -> unit = "thread_initialize"
external thread_wait_read : file_descr -> unit = "thread_wait_read"
external thread_wait_write : file_descr -> unit = "thread_wait_write"
external thread_select :
  file_descr list * file_descr list * file_descr list * float
       -> resumption_status
  = "thread_select"
external thread_wait_pid : int -> resumption_status = "thread_wait_pid"
external thread_delay : float -> unit = "thread_delay"

let wait_read fd = thread_wait_read fd
let wait_write fd = thread_wait_write fd
let select_aux arg = thread_select arg
let wait_pid_aux pid = thread_wait_pid pid
let delay duration = thread_delay duration

(* Make sure that threads are initialized (PR#1516). *)

let _ = thread_initialize()

(* Back to the Unix module *)

type error =
    E2BIG
  | EACCES
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
  | EOVERFLOW
  | EUNKNOWNERR of int

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
external getenv: string -> string = "caml_sys_getenv"
external putenv: string -> string -> unit = "unix_putenv"

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

type wait_flag =
    WNOHANG
  | WUNTRACED

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
  | O_NOCTTY
  | O_DSYNC
  | O_SYNC
  | O_RSYNC
  | O_SHARE_DELETE
  | O_CLOEXEC

type file_perm = int


external openfile : string -> open_flag list -> file_perm -> file_descr
           = "unix_open"

external close : file_descr -> unit = "unix_close"
external unsafe_read : file_descr -> bytes -> int -> int -> int = "unix_read"
external unsafe_write : file_descr -> bytes -> int -> int -> int
    = "unix_write"
external unsafe_single_write : file_descr -> bytes -> int -> int -> int
    = "unix_single_write"

let rec read fd buf ofs len =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.read"
    else unsafe_read fd buf ofs len
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_read fd; read fd buf ofs len

let rec write fd buf ofs len =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.write"
    else unsafe_write fd buf ofs len
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_write fd; write fd buf ofs len

let rec single_write fd buf ofs len =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.single_write"
    else unsafe_single_write fd buf ofs len
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_write fd; single_write fd buf ofs len

let write_substring fd buf ofs len =
  write fd (Bytes.unsafe_of_string buf) ofs len

let single_write_substring fd buf ofs len =
  single_write fd (Bytes.unsafe_of_string buf) ofs len

external in_channel_of_descr : file_descr -> in_channel
                             = "caml_ml_open_descriptor_in"
external out_channel_of_descr : file_descr -> out_channel
                              = "caml_ml_open_descriptor_out"
external descr_of_in_channel : in_channel -> file_descr
                             = "caml_channel_descriptor"
external descr_of_out_channel : out_channel -> file_descr
                              = "caml_channel_descriptor"

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
    st_atime : float;
    st_mtime : float;
    st_ctime : float }

external stat : string -> stats = "unix_stat"
external lstat : string -> stats = "unix_lstat"
external fstat : file_descr -> stats = "unix_fstat"
external isatty : file_descr -> bool = "unix_isatty"
external unlink : string -> unit = "unix_unlink"
external rename : string -> string -> unit = "unix_rename"
external link : string -> string -> unit = "unix_link"

module LargeFile =
  struct
    external lseek : file_descr -> int64 -> seek_command -> int64
                   = "unix_lseek_64"
    external truncate : string -> int64 -> unit = "unix_truncate_64"
    external ftruncate : file_descr -> int64 -> unit = "unix_ftruncate_64"
    type stats =
      { st_dev : int;
        st_ino : int;
        st_kind : file_kind;
        st_perm : file_perm;
        st_nlink : int;
        st_uid : int;
        st_gid : int;
        st_rdev : int;
        st_size : int64;
        st_atime : float;
        st_mtime : float;
        st_ctime : float;
      }
    external stat : string -> stats = "unix_stat_64"
    external lstat : string -> stats = "unix_lstat_64"
    external fstat : file_descr -> stats = "unix_fstat_64"
  end

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

external dup : file_descr -> file_descr = "unix_dup"
external dup2 : file_descr -> file_descr -> unit = "unix_dup2"
external set_nonblock : file_descr -> unit = "unix_set_nonblock"
external clear_nonblock : file_descr -> unit = "unix_clear_nonblock"
external set_close_on_exec : file_descr -> unit = "unix_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit = "unix_clear_close_on_exec"

external mkdir : string -> file_perm -> unit = "unix_mkdir"
external rmdir : string -> unit = "unix_rmdir"
external chdir : string -> unit = "unix_chdir"
external getcwd : unit -> string = "unix_getcwd"
external chroot : string -> unit = "unix_chroot"

type dir_handle

external opendir : string -> dir_handle = "unix_opendir"
external readdir : dir_handle -> string = "unix_readdir"
external rewinddir : dir_handle -> unit = "unix_rewinddir"
external closedir : dir_handle -> unit = "unix_closedir"

external _pipe : unit -> file_descr * file_descr = "unix_pipe"

let pipe() =
  let (out_fd, in_fd as fd_pair) = _pipe() in
  set_nonblock in_fd;
  set_nonblock out_fd;
  fd_pair

external symlink : ?to_dir:bool -> string -> string -> unit = "unix_symlink"
external has_symlink : unit -> bool = "unix_has_symlink"
external readlink : string -> string = "unix_readlink"
external mkfifo : string -> file_perm -> unit = "unix_mkfifo"

let select readfds writefds exceptfds delay =
  match select_aux (readfds, writefds, exceptfds, delay) with
    Resumed_select(r, w, e) -> (r, w, e)
  | _ -> ([], [], [])

type lock_command =
    F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK

external lockf : file_descr -> lock_command -> int -> unit = "unix_lockf"

external _execv : string -> string array -> 'a = "unix_execv"
external _execve : string -> string array -> string array -> 'a = "unix_execve"
external _execvp : string -> string array -> 'a = "unix_execvp"
external _execvpe : string -> string array -> string array -> 'a
                  = "unix_execvpe"

(* Disable the timer interrupt before doing exec, because some OS
   keep sending timer interrupts to the exec'ed code.
   Also restore blocking mode on stdin, stdout and stderr,
   since this is what most programs expect! *)

let safe_clear_nonblock fd =
  try clear_nonblock fd with Unix_error(_,_,_) -> ()
let safe_set_nonblock fd =
  try set_nonblock fd with Unix_error(_,_,_) -> ()

let do_exec fn =
  let oldtimer =
    setitimer ITIMER_VIRTUAL {it_interval = 0.0; it_value = 0.0} in
  safe_clear_nonblock stdin;
  safe_clear_nonblock stdout;
  safe_clear_nonblock stderr;
  try
    fn ()
  with Unix_error(_,_,_) as exn ->
    ignore(setitimer ITIMER_VIRTUAL oldtimer);
    safe_set_nonblock stdin;
    safe_set_nonblock stdout;
    safe_set_nonblock stderr;
    raise exn

let execv proc args =
  do_exec (fun () -> _execv proc args)

let execve proc args env =
  do_exec (fun () -> _execve proc args env)

let execvp proc args =
  do_exec (fun () -> _execvp proc args)

let execvpe proc args =
  do_exec (fun () -> _execvpe proc args)

external fork : unit -> int = "unix_fork"
external _waitpid : wait_flag list -> int -> int * process_status
                  = "unix_waitpid"

let wait_pid pid =
  match wait_pid_aux pid with
    Resumed_wait(pid, status) -> (pid, status)
  | _ -> invalid_arg "Thread.wait_pid"

let wait () = wait_pid (-1)

let waitpid flags pid =
  if List.mem WNOHANG flags
  then _waitpid flags pid
  else wait_pid pid

external getpid : unit -> int = "unix_getpid"
external getppid : unit -> int = "unix_getppid"
external nice : int -> int = "unix_nice"

external kill : int -> int -> unit = "unix_kill"
type sigprocmask_command = SIG_SETMASK | SIG_BLOCK | SIG_UNBLOCK
external sigprocmask: sigprocmask_command -> int list -> int list
        = "unix_sigprocmask"
external sigpending: unit -> int list = "unix_sigpending"
external sigsuspend: int list -> unit = "unix_sigsuspend"

let pause() =
  let sigs = sigprocmask SIG_BLOCK [] in sigsuspend sigs

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
external alarm : int -> int = "unix_alarm"

let sleepf = delay
let sleep secs = delay (float secs)

external times : unit -> process_times = "unix_times"
external utimes : string -> float -> float -> unit = "unix_utimes"

external getuid : unit -> int = "unix_getuid"
external geteuid : unit -> int = "unix_geteuid"
external setuid : int -> unit = "unix_setuid"
external getgid : unit -> int = "unix_getgid"
external getegid : unit -> int = "unix_getegid"
external setgid : int -> unit = "unix_setgid"
external getgroups : unit -> int array = "unix_getgroups"
external setgroups : int array -> unit = "unix_setgroups"
external initgroups : string -> int -> unit = "unix_initgroups"

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

type inet_addr = string

external inet_addr_of_string : string -> inet_addr
                                    = "unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr -> string
                                    = "unix_string_of_inet_addr"

let inet_addr_any = inet_addr_of_string "0.0.0.0"
let inet_addr_loopback = inet_addr_of_string "127.0.0.1"
let inet6_addr_any =
  try inet_addr_of_string "::" with Failure _ -> inet_addr_any
let inet6_addr_loopback =
  try inet_addr_of_string "::1" with Failure _ -> inet_addr_loopback

let is_inet6_addr s = String.length s = 16

type socket_domain =
    PF_UNIX
  | PF_INET
  | PF_INET6

type socket_type =
    SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int

let domain_of_sockaddr = function
    ADDR_UNIX _ -> PF_UNIX
  | ADDR_INET(a, _) -> if is_inet6_addr a then PF_INET6 else PF_INET

type shutdown_command =
    SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

external _socket : socket_domain -> socket_type -> int -> file_descr
                                  = "unix_socket"
external _socketpair :
        socket_domain -> socket_type -> int -> file_descr * file_descr
                                  = "unix_socketpair"

let socket dom typ proto =
  let s = _socket dom typ proto in
  set_nonblock s;
  s

let socketpair dom typ proto =
  let (s1, s2 as spair) = _socketpair dom typ proto in
  set_nonblock s1; set_nonblock s2;
  spair

external _accept : file_descr -> file_descr * sockaddr = "unix_accept"

let rec accept req =
  wait_read req;
  try
    let (s, caller as result) = _accept req in
    set_nonblock s;
    result
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) -> accept req

external bind : file_descr -> sockaddr -> unit = "unix_bind"
external listen : file_descr -> int -> unit = "unix_listen"
external shutdown : file_descr -> shutdown_command -> unit = "unix_shutdown"
external getsockname : file_descr -> sockaddr = "unix_getsockname"
external getpeername : file_descr -> sockaddr = "unix_getpeername"

external _connect : file_descr -> sockaddr -> unit = "unix_connect"

let connect s addr =
  try
    _connect s addr
  with Unix_error((EINPROGRESS | EWOULDBLOCK | EAGAIN), _, _) ->
    wait_write s;
    (* Check if it really worked *)
    ignore(getpeername s)

external unsafe_recv :
  file_descr -> bytes -> int -> int -> msg_flag list -> int
                                  = "unix_recv"
external unsafe_recvfrom :
  file_descr -> bytes -> int -> int -> msg_flag list -> int * sockaddr
                                  = "unix_recvfrom"
external unsafe_send :
  file_descr -> bytes -> int -> int -> msg_flag list -> int
                                  = "unix_send"
external unsafe_sendto :
  file_descr -> bytes -> int -> int -> msg_flag list -> sockaddr -> int
                                  = "unix_sendto" "unix_sendto_native"

let rec recv fd buf ofs len flags =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.recv"
    else unsafe_recv fd buf ofs len flags
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_read fd; recv fd buf ofs len flags

let rec recvfrom fd buf ofs len flags =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.recvfrom"
    else unsafe_recvfrom fd buf ofs len flags
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_read fd;
    recvfrom fd buf ofs len flags

let rec send fd buf ofs len flags =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.send"
    else unsafe_send fd buf ofs len flags
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_write fd;
    send fd buf ofs len flags

let rec sendto fd buf ofs len flags addr =
  try
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
    then invalid_arg "Unix.sendto"
    else unsafe_sendto fd buf ofs len flags addr
  with Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    wait_write fd;
    sendto fd buf ofs len flags addr

let send_substring fd buf ofs len flags =
  send fd (Bytes.unsafe_of_string buf) ofs len flags

let sendto_substring fd buf ofs len flags addr =
  sendto fd (Bytes.unsafe_of_string buf) ofs len flags addr

type socket_bool_option =
    SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE
  | SO_ACCEPTCONN
  | TCP_NODELAY
  | IPV6_ONLY


type socket_int_option =
    SO_SNDBUF
  | SO_RCVBUF
  | SO_ERROR
  | SO_TYPE
  | SO_RCVLOWAT
  | SO_SNDLOWAT

type socket_optint_option = SO_LINGER

type socket_float_option =
    SO_RCVTIMEO
  | SO_SNDTIMEO

type socket_error_option = SO_ERROR

module SO: sig
  type ('opt, 'v) t
  val bool: (socket_bool_option, bool) t
  val int: (socket_int_option, int) t
  val optint: (socket_optint_option, int option) t
  val float: (socket_float_option, float) t
  val error: (socket_error_option, error option) t
  val get: ('opt, 'v) t -> file_descr -> 'opt -> 'v
  val set: ('opt, 'v) t -> file_descr -> 'opt -> 'v -> unit
end = struct
  type ('opt, 'v) t = int
  let bool = 0
  let int = 1
  let optint = 2
  let float = 3
  let error = 4
  external get: ('opt, 'v) t -> file_descr -> 'opt -> 'v
              = "unix_getsockopt"
  external set: ('opt, 'v) t -> file_descr -> 'opt -> 'v -> unit
              = "unix_setsockopt"
end

let getsockopt fd opt = SO.get SO.bool fd opt
let setsockopt fd opt v = SO.set SO.bool fd opt v

let getsockopt_int fd opt = SO.get SO.int fd opt
let setsockopt_int fd opt v = SO.set SO.int fd opt v

let getsockopt_optint fd opt = SO.get SO.optint fd opt
let setsockopt_optint fd opt v = SO.set SO.optint fd opt v

let getsockopt_float fd opt = SO.get SO.float fd opt
let setsockopt_float fd opt v = SO.set SO.float fd opt v

let getsockopt_error fd = SO.get SO.error fd SO_ERROR

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
type addr_info =
  { ai_family : socket_domain;
    ai_socktype : socket_type;
    ai_protocol : int;
    ai_addr : sockaddr;
    ai_canonname : string }

type getaddrinfo_option =
    AI_FAMILY of socket_domain
  | AI_SOCKTYPE of socket_type
  | AI_PROTOCOL of int
  | AI_NUMERICHOST
  | AI_CANONNAME
  | AI_PASSIVE

external getaddrinfo_system
  : string -> string -> getaddrinfo_option list -> addr_info list
  = "unix_getaddrinfo"

let getaddrinfo_emulation node service opts =
  (* Parse options *)
  let opt_socktype = ref None
  and opt_protocol = ref 0
  and opt_passive = ref false in
  List.iter
    (function AI_SOCKTYPE s -> opt_socktype := Some s
            | AI_PROTOCOL p -> opt_protocol := p
            | AI_PASSIVE -> opt_passive := true
            | _ -> ())
    opts;
  (* Determine socket types and port numbers *)
  let get_port ty kind =
    if service = "" then [ty, 0] else
      try
        [ty, int_of_string service]
      with Failure _ ->
      try
        [ty, (getservbyname service kind).s_port]
      with Not_found -> []
  in
  let ports =
    match !opt_socktype with
    | None ->
        get_port SOCK_STREAM "tcp" @ get_port SOCK_DGRAM "udp"
    | Some SOCK_STREAM ->
        get_port SOCK_STREAM "tcp"
    | Some SOCK_DGRAM ->
        get_port SOCK_DGRAM "udp"
    | Some ty ->
        if service = "" then [ty, 0] else [] in
  (* Determine IP addresses *)
  let addresses =
    if node = "" then
      if List.mem AI_PASSIVE opts
      then [inet_addr_any, "0.0.0.0"]
      else [inet_addr_loopback, "127.0.0.1"]
    else
      try
        [inet_addr_of_string node, node]
      with Failure _ ->
      try
        let he = gethostbyname node in
        List.map
          (fun a -> (a, he.h_name))
          (Array.to_list he.h_addr_list)
      with Not_found ->
        [] in
  (* Cross-product of addresses and ports *)
  List.flatten
    (List.map
      (fun (ty, port) ->
        List.map
          (fun (addr, name) ->
            { ai_family = PF_INET;
              ai_socktype = ty;
              ai_protocol = !opt_protocol;
              ai_addr = ADDR_INET(addr, port);
              ai_canonname = name })
          addresses)
      ports)

let getaddrinfo node service opts =
  try
    List.rev(getaddrinfo_system node service opts)
  with Invalid_argument _ ->
    getaddrinfo_emulation node service opts

type name_info =
  { ni_hostname : string;
    ni_service : string }

type getnameinfo_option =
    NI_NOFQDN
  | NI_NUMERICHOST
  | NI_NAMEREQD
  | NI_NUMERICSERV
  | NI_DGRAM

external getnameinfo_system
  : sockaddr -> getnameinfo_option list -> name_info
  = "unix_getnameinfo"

let getnameinfo_emulation addr opts =
  match addr with
  | ADDR_UNIX f ->
      { ni_hostname = ""; ni_service = f } (* why not? *)
  | ADDR_INET(a, p) ->
      let hostname =
        try
          if List.mem NI_NUMERICHOST opts then raise Not_found;
          (gethostbyaddr a).h_name
        with Not_found ->
          if List.mem NI_NAMEREQD opts then raise Not_found;
          string_of_inet_addr a in
      let service =
        try
          if List.mem NI_NUMERICSERV opts then raise Not_found;
          let kind = if List.mem NI_DGRAM opts then "udp" else "tcp" in
          (getservbyport p kind).s_name
        with Not_found ->
          string_of_int p in
      { ni_hostname = hostname; ni_service = service }

let getnameinfo addr opts =
  try
    getnameinfo_system addr opts
  with Invalid_argument _ ->
    getnameinfo_emulation addr opts

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

external setsid : unit -> int = "unix_setsid"

(* High-level process management (system, popen) *)

let system cmd =
  match fork() with
     0 -> begin try
            execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ ->
            exit 127
          end
  | id -> snd(waitpid [] id)

let rec safe_dup fd =
  let new_fd = dup fd in
  if new_fd >= 3 then
    new_fd
  else begin
    let res = safe_dup fd in
    close new_fd;
    res
  end

let safe_close fd =
  try close fd with Unix_error(_,_,_) -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let newnewstdin = safe_dup new_stdin in
  let newnewstdout = safe_dup new_stdout in
  let newnewstderr = safe_dup new_stderr in
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr;
  dup2 newnewstdin stdin; close newnewstdin;
  dup2 newnewstdout stdout; close newnewstdout;
  dup2 newnewstderr stderr; close newnewstderr

let create_process cmd args new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvp cmd args
      with _ ->
        exit 127
      end
  | id -> id

let create_process_env cmd args env new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvpe cmd args env
      with _ ->
        exit 127
      end
  | id -> id

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output toclose =
  match fork() with
     0 -> if input <> stdin then begin dup2 input stdin; close input end;
          if output <> stdout then begin dup2 output stdout; close output end;
          List.iter close toclose;
          begin try execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ -> exit 127
          end
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) stdin in_write [in_read];
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read stdout [out_write];
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
  close out_read;
  close in_write;
  (inchan, outchan)

let open_proc_full cmd env proc input output error toclose =
  match fork() with
     0 -> dup2 input stdin; close input;
          dup2 output stdout; close output;
          dup2 error stderr; close error;
          List.iter close toclose;
          begin try execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env
          with _ -> exit 127
          end
  | id -> Hashtbl.add popen_processes proc id

let open_process_full cmd env =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let (err_read, err_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  open_proc_full cmd env (Process_full(inchan, outchan, errchan))
                 out_read in_write err_write [in_read; out_write; err_read];
  close out_read;
  close in_write;
  close err_write;
  (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let rec waitpid_non_intr pid =
  try waitpid [] pid
  with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid_non_intr pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid_non_intr pid)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  snd(waitpid_non_intr pid)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  close_in errchan;
  snd(waitpid_non_intr pid)

(* High-level network functions *)

let open_connection sockaddr =
  let sock =
    socket (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  try
    connect sock sockaddr;
    (in_channel_of_descr sock, out_channel_of_descr sock)
  with exn ->
    close sock; raise exn

let shutdown_connection inchan =
  shutdown (descr_of_in_channel inchan) SHUTDOWN_SEND

let establish_server server_fun sockaddr =
  let sock =
    socket (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 5;
  while true do
    let (s, caller) = accept sock in
    (* The "double fork" trick, the process which calls server_fun will not
       leave a zombie process *)
    match fork() with
       0 -> if fork() <> 0 then exit 0; (* The son exits, the grandson works *)
            let inchan = in_channel_of_descr s in
            let outchan = out_channel_of_descr s in
            server_fun inchan outchan;
            close_out outchan;
            (* The file descriptor was already closed by close_out.
               close_in inchan;
            *)
            exit 0
    | id -> close s; ignore(waitpid [] id) (* Reclaim the son *)
  done
