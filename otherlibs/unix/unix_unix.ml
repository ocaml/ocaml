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

let shell = "/bin/sh"

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
  | EOVERFLOW
  (* All other errors are mapped to EUNKNOWNERR *)
  | EUNKNOWNERR of int  (* Unknown error *)

exception Unix_error of error * string * string

let _ = Callback.register_exception "Unix.Unix_error"
                                    (Unix_error(E2BIG, "", ""))

external error_message : error -> string = "caml_unix_error_message"

let () =
  Printexc.register_printer
    (function
      | Unix_error (e, s, s') ->
          let msg = match e with
          | E2BIG -> "E2BIG"
          | EACCES -> "EACCES"
          | EAGAIN -> "EAGAIN"
          | EBADF -> "EBADF"
          | EBUSY -> "EBUSY"
          | ECHILD -> "ECHILD"
          | EDEADLK -> "EDEADLK"
          | EDOM -> "EDOM"
          | EEXIST -> "EEXIST"
          | EFAULT -> "EFAULT"
          | EFBIG -> "EFBIG"
          | EINTR -> "EINTR"
          | EINVAL -> "EINVAL"
          | EIO -> "EIO"
          | EISDIR -> "EISDIR"
          | EMFILE -> "EMFILE"
          | EMLINK -> "EMLINK"
          | ENAMETOOLONG -> "ENAMETOOLONG"
          | ENFILE -> "ENFILE"
          | ENODEV -> "ENODEV"
          | ENOENT -> "ENOENT"
          | ENOEXEC -> "ENOEXEC"
          | ENOLCK -> "ENOLCK"
          | ENOMEM -> "ENOMEM"
          | ENOSPC -> "ENOSPC"
          | ENOSYS -> "ENOSYS"
          | ENOTDIR -> "ENOTDIR"
          | ENOTEMPTY -> "ENOTEMPTY"
          | ENOTTY -> "ENOTTY"
          | ENXIO -> "ENXIO"
          | EPERM -> "EPERM"
          | EPIPE -> "EPIPE"
          | ERANGE -> "ERANGE"
          | EROFS -> "EROFS"
          | ESPIPE -> "ESPIPE"
          | ESRCH -> "ESRCH"
          | EXDEV -> "EXDEV"
          | EWOULDBLOCK -> "EWOULDBLOCK"
          | EINPROGRESS -> "EINPROGRESS"
          | EALREADY -> "EALREADY"
          | ENOTSOCK -> "ENOTSOCK"
          | EDESTADDRREQ -> "EDESTADDRREQ"
          | EMSGSIZE -> "EMSGSIZE"
          | EPROTOTYPE -> "EPROTOTYPE"
          | ENOPROTOOPT -> "ENOPROTOOPT"
          | EPROTONOSUPPORT -> "EPROTONOSUPPORT"
          | ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"
          | EOPNOTSUPP -> "EOPNOTSUPP"
          | EPFNOSUPPORT -> "EPFNOSUPPORT"
          | EAFNOSUPPORT -> "EAFNOSUPPORT"
          | EADDRINUSE -> "EADDRINUSE"
          | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
          | ENETDOWN -> "ENETDOWN"
          | ENETUNREACH -> "ENETUNREACH"
          | ENETRESET -> "ENETRESET"
          | ECONNABORTED -> "ECONNABORTED"
          | ECONNRESET -> "ECONNRESET"
          | ENOBUFS -> "ENOBUFS"
          | EISCONN -> "EISCONN"
          | ENOTCONN -> "ENOTCONN"
          | ESHUTDOWN -> "ESHUTDOWN"
          | ETOOMANYREFS -> "ETOOMANYREFS"
          | ETIMEDOUT -> "ETIMEDOUT"
          | ECONNREFUSED -> "ECONNREFUSED"
          | EHOSTDOWN -> "EHOSTDOWN"
          | EHOSTUNREACH -> "EHOSTUNREACH"
          | ELOOP -> "ELOOP"
          | EOVERFLOW -> "EOVERFLOW"
          | EUNKNOWNERR x -> Printf.sprintf "EUNKNOWNERR %d" x in
          Some (Printf.sprintf "Unix.Unix_error(Unix.%s, %S, %S)" msg s s')
      | _ -> None)

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

external environment : unit -> string array = "caml_unix_environment"
external unsafe_environment : unit -> string array
                            = "caml_unix_environment_unsafe"
external getenv: string -> string = "caml_sys_getenv"
external unsafe_getenv: string -> string = "caml_sys_unsafe_getenv"
external putenv: string -> string -> unit = "caml_unix_putenv"

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag =
    WNOHANG
  | WUNTRACED

external execv : string -> string array -> 'a = "caml_unix_execv"
external execve : string -> string array -> string array -> 'a
                = "caml_unix_execve"
external execvp : string -> string array -> 'a = "caml_unix_execvp"
external execvpe : string -> string array -> string array -> 'a
                 = "caml_unix_execvpe"

external fork : unit -> int = "caml_unix_fork"
external wait : unit -> int * process_status = "caml_unix_wait"
external waitpid : wait_flag list -> int -> int * process_status
   = "caml_unix_waitpid"
external _exit : int -> 'a = "caml_unix_exit"
external getpid : unit -> int = "caml_unix_getpid"
external getppid : unit -> int = "caml_unix_getppid"
external nice : int -> int = "caml_unix_nice"

(* Basic file input/output *)

type file_descr = int

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
  | O_KEEPEXEC

type file_perm = int


external openfile : string -> open_flag list -> file_perm -> file_descr
           = "caml_unix_open"
external close : file_descr -> unit = "caml_unix_close"
external fsync : file_descr -> unit = "caml_unix_fsync"
external unsafe_read : file_descr -> bytes -> int -> int -> int
   = "caml_unix_read"
external unsafe_write : file_descr -> bytes -> int -> int -> int
                      = "caml_unix_write"
external unsafe_single_write : file_descr -> bytes -> int -> int -> int
   = "caml_unix_single_write"

let read fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.read"
  else unsafe_read fd buf ofs len
let write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.write"
  else unsafe_write fd buf ofs len
(* write misbehaves because it attempts to write all data by making repeated
   calls to the Unix write function (see comment in write.c and unix.mli).
   single_write fixes this by never calling write twice. *)
let single_write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.single_write"
  else unsafe_single_write fd buf ofs len

let write_substring fd buf ofs len =
  write fd (Bytes.unsafe_of_string buf) ofs len

let single_write_substring fd buf ofs len =
  single_write fd (Bytes.unsafe_of_string buf) ofs len

(* Interfacing with the standard input/output library *)

external in_channel_of_descr : file_descr -> in_channel
                             = "caml_unix_inchannel_of_filedescr"
external out_channel_of_descr : file_descr -> out_channel
                              = "caml_unix_outchannel_of_filedescr"
external descr_of_in_channel : in_channel -> file_descr
                             = "caml_channel_descriptor"
external descr_of_out_channel : out_channel -> file_descr
                              = "caml_channel_descriptor"

(* Seeking and truncating *)

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

external lseek : file_descr -> int -> seek_command -> int = "caml_unix_lseek"
external truncate : string -> int -> unit = "caml_unix_truncate"
external ftruncate : file_descr -> int -> unit = "caml_unix_ftruncate"

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

external stat : string -> stats = "caml_unix_stat"
external lstat : string -> stats = "caml_unix_lstat"
external fstat : file_descr -> stats = "caml_unix_fstat"
external isatty : file_descr -> bool = "caml_unix_isatty"

(* Operations on file names *)

external unlink : string -> unit = "caml_unix_unlink"
external rename : string -> string -> unit = "caml_unix_rename"
external link : ?follow:bool -> string -> string -> unit = "caml_unix_link"
external realpath : string -> string = "caml_unix_realpath"

(* Operations on large files *)

module LargeFile =
  struct
    external lseek : file_descr -> int64 -> seek_command -> int64
       = "caml_unix_lseek_64"
    external truncate : string -> int64 -> unit = "caml_unix_truncate_64"
    external ftruncate : file_descr -> int64 -> unit = "caml_unix_ftruncate_64"
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
    external stat : string -> stats = "caml_unix_stat_64"
    external lstat : string -> stats = "caml_unix_lstat_64"
    external fstat : file_descr -> stats = "caml_unix_fstat_64"
  end

(* Mapping files into memory *)

external map_internal:
   file_descr -> ('a, 'b) Stdlib.Bigarray.kind
              -> 'c Stdlib.Bigarray.layout
              -> bool -> int array -> int64
              -> ('a, 'b, 'c) Stdlib.Bigarray.Genarray.t
     = "caml_unix_map_file_bytecode" "caml_unix_map_file"

let map_file fd ?(pos=0L) kind layout shared dims =
  map_internal fd kind layout shared dims pos

(* File permissions and ownership *)

type access_permission =
    R_OK
  | W_OK
  | X_OK
  | F_OK

external chmod : string -> file_perm -> unit = "caml_unix_chmod"
external fchmod : file_descr -> file_perm -> unit = "caml_unix_fchmod"
external chown : string -> int -> int -> unit = "caml_unix_chown"
external fchown : file_descr -> int -> int -> unit = "caml_unix_fchown"
external umask : int -> int = "caml_unix_umask"
external access : string -> access_permission list -> unit = "caml_unix_access"

(* Operations on file descriptors *)

external dup : ?cloexec: bool -> file_descr -> file_descr = "caml_unix_dup"
external dup2 :
   ?cloexec: bool -> file_descr -> file_descr -> unit = "caml_unix_dup2"
external set_nonblock : file_descr -> unit = "caml_unix_set_nonblock"
external clear_nonblock : file_descr -> unit = "caml_unix_clear_nonblock"
external set_close_on_exec : file_descr -> unit = "caml_unix_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit
                             = "caml_unix_clear_close_on_exec"

(* Directories *)

external mkdir : string -> file_perm -> unit = "caml_unix_mkdir"
external rmdir : string -> unit = "caml_unix_rmdir"
external chdir : string -> unit = "caml_unix_chdir"
external getcwd : unit -> string = "caml_unix_getcwd"
external chroot : string -> unit = "caml_unix_chroot"

type dir_handle

external opendir : string -> dir_handle = "caml_unix_opendir"
external readdir : dir_handle -> string = "caml_unix_readdir"
external rewinddir : dir_handle -> unit = "caml_unix_rewinddir"
external closedir : dir_handle -> unit = "caml_unix_closedir"

(* Pipes *)

external pipe :
  ?cloexec: bool -> unit -> file_descr * file_descr = "caml_unix_pipe"
external mkfifo : string -> file_perm -> unit = "caml_unix_mkfifo"

(* Symbolic links *)

external readlink : string -> string = "caml_unix_readlink"
external symlink : ?to_dir:bool -> string -> string -> unit
                 = "caml_unix_symlink"
external has_symlink : unit -> bool = "caml_unix_has_symlink"

(* Locking *)

type lock_command =
    F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK

external lockf : file_descr -> lock_command -> int -> unit = "caml_unix_lockf"
external kill : int -> int -> unit = "caml_unix_kill"
type sigprocmask_command = SIG_SETMASK | SIG_BLOCK | SIG_UNBLOCK
external sigprocmask: sigprocmask_command -> int list -> int list
        = "caml_unix_sigprocmask"
external sigpending: unit -> int list = "caml_unix_sigpending"
external sigsuspend: int list -> unit = "caml_unix_sigsuspend"

let pause() =
  let sigs = sigprocmask SIG_BLOCK [] in sigsuspend sigs

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

external time : unit -> (float [@unboxed]) =
  "caml_unix_time" "caml_unix_time_unboxed" [@@noalloc]
external gettimeofday : unit -> (float [@unboxed]) =
  "caml_unix_gettimeofday" "caml_unix_gettimeofday_unboxed" [@@noalloc]
external gmtime : float -> tm = "caml_unix_gmtime"
external localtime : float -> tm = "caml_unix_localtime"
external mktime : tm -> float * tm = "caml_unix_mktime"
external alarm : int -> int = "caml_unix_alarm"
external sleepf : float -> unit = "caml_unix_sleep"
let sleep duration = sleepf (float duration)
external times : unit -> process_times = "caml_unix_times"
external utimes : string -> float -> float -> unit = "caml_unix_utimes"

type interval_timer =
    ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF

type interval_timer_status =
  { it_interval: float;                 (* Period *)
    it_value: float }                   (* Current value of the timer *)

external getitimer:
  interval_timer -> interval_timer_status
  = "caml_unix_getitimer"
external setitimer:
  interval_timer -> interval_timer_status -> interval_timer_status
  = "caml_unix_setitimer"

external getuid : unit -> int = "caml_unix_getuid"
external geteuid : unit -> int = "caml_unix_geteuid"
external setuid : int -> unit = "caml_unix_setuid"
external getgid : unit -> int = "caml_unix_getgid"
external getegid : unit -> int = "caml_unix_getegid"
external setgid : int -> unit = "caml_unix_setgid"
external getgroups : unit -> int array = "caml_unix_getgroups"
external setgroups : int array -> unit = "caml_unix_setgroups"
external initgroups : string -> int -> unit = "caml_unix_initgroups"

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


external getlogin : unit -> string = "caml_unix_getlogin"
external getpwnam : string -> passwd_entry = "caml_unix_getpwnam"
external getgrnam : string -> group_entry = "caml_unix_getgrnam"
external getpwuid : int -> passwd_entry = "caml_unix_getpwuid"
external getgrgid : int -> group_entry = "caml_unix_getgrgid"

(* Internet addresses *)

type inet_addr = string

let is_inet6_addr s = String.length s = 16

external inet_addr_of_string : string -> inet_addr
                                    = "caml_unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr -> string
                                    = "caml_unix_string_of_inet_addr"

let inet_addr_any = inet_addr_of_string "0.0.0.0"
let inet_addr_loopback = inet_addr_of_string "127.0.0.1"
let inet6_addr_any =
  try inet_addr_of_string "::" with Failure _ -> inet_addr_any
let inet6_addr_loopback =
  try inet_addr_of_string "::1" with Failure _ -> inet_addr_loopback

(* Sockets *)

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

external socket :
  ?cloexec: bool -> socket_domain -> socket_type -> int -> file_descr
  = "caml_unix_socket"
external socketpair :
  ?cloexec: bool -> socket_domain -> socket_type -> int ->
                                           file_descr * file_descr
  = "caml_unix_socketpair"
external accept :
  ?cloexec: bool -> file_descr -> file_descr * sockaddr = "caml_unix_accept"
external bind : file_descr -> sockaddr -> unit = "caml_unix_bind"
external connect : file_descr -> sockaddr -> unit = "caml_unix_connect"
external listen : file_descr -> int -> unit = "caml_unix_listen"
external shutdown : file_descr -> shutdown_command -> unit
                  = "caml_unix_shutdown"
external getsockname : file_descr -> sockaddr = "caml_unix_getsockname"
external getpeername : file_descr -> sockaddr = "caml_unix_getpeername"

external unsafe_recv :
  file_descr -> bytes -> int -> int -> msg_flag list -> int
                                  = "caml_unix_recv"
external unsafe_recvfrom :
  file_descr -> bytes -> int -> int -> msg_flag list -> int * sockaddr
                                  = "caml_unix_recvfrom"
external unsafe_send :
  file_descr -> bytes -> int -> int -> msg_flag list -> int
                                  = "caml_unix_send"
external unsafe_sendto :
  file_descr -> bytes -> int -> int -> msg_flag list -> sockaddr -> int
                                  = "caml_unix_sendto" "caml_unix_sendto_native"

let recv fd buf ofs len flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.recv"
  else unsafe_recv fd buf ofs len flags
let recvfrom fd buf ofs len flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.recvfrom"
  else unsafe_recvfrom fd buf ofs len flags
let send fd buf ofs len flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.send"
  else unsafe_send fd buf ofs len flags
let sendto fd buf ofs len flags addr =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.sendto"
  else unsafe_sendto fd buf ofs len flags addr

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
  | SO_REUSEPORT

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
              = "caml_unix_getsockopt"
  external set: ('opt, 'v) t -> file_descr -> 'opt -> 'v -> unit
              = "caml_unix_setsockopt"
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

external gethostname : unit -> string = "caml_unix_gethostname"
external gethostbyname : string -> host_entry = "caml_unix_gethostbyname"
external gethostbyaddr : inet_addr -> host_entry = "caml_unix_gethostbyaddr"
external getprotobyname : string -> protocol_entry
                                         = "caml_unix_getprotobyname"
external getprotobynumber : int -> protocol_entry
                                         = "caml_unix_getprotobynumber"
external getservbyname : string -> string -> service_entry
                                         = "caml_unix_getservbyname"
external getservbyport : int -> string -> service_entry
                                         = "caml_unix_getservbyport"

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
  = "caml_unix_getaddrinfo"

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
  = "caml_unix_getnameinfo"

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
          Int.to_string p in
      { ni_hostname = hostname; ni_service = service }

let getnameinfo addr opts =
  try
    getnameinfo_system addr opts
  with Invalid_argument _ ->
    getnameinfo_emulation addr opts

(* High-level process management (system, popen) *)

let rec waitpid_non_intr pid =
  try waitpid [] pid
  with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

external spawn : string -> string array -> string array option ->
                 bool -> int array -> int
               = "caml_unix_spawn"

let create_process_gen cmd args optenv
                       new_stdin new_stdout new_stderr =
  let toclose = ref [] in
  let close_after () =
    List.iter
      (fun fd -> try close fd with Unix_error(_,_,_) -> ())
      !toclose in
  (* Duplicate [fd] if needed to make sure it isn't one of the
     standard descriptors (stdin, stdout, stderr).
     The temporary file descriptors created here will be closed
     after the spawn, both in the parent (call to [close_after] below)
     and in the child (they are close-on-exec). *)
  let rec file_descr_not_standard fd =
    if fd >= 3 then fd else begin
      let fd' = dup ~cloexec:true fd in
      toclose := fd' :: !toclose;
      file_descr_not_standard fd'
    end in
  (* As an optimization, if a standard descriptor is not redirected,
     i.e. "redirected to itself", don't duplicate it: the [unix_spawn]
     C stub will perform no redirection either. *)
  let redirections = [|
    (if new_stdin = 0 then 0 else file_descr_not_standard new_stdin);
    (if new_stdout = 1 then 1 else file_descr_not_standard new_stdout);
    (if new_stderr = 2 then 2 else file_descr_not_standard new_stderr)
  |] in
  Fun.protect ~finally:close_after
    (fun () -> spawn cmd args optenv true (* usepath *) redirections)

let create_process cmd args new_stdin new_stdout new_stderr =
  create_process_gen cmd args None new_stdin new_stdout new_stderr

let create_process_env cmd args env new_stdin new_stdout new_stderr =
  create_process_gen cmd args (Some env) new_stdin new_stdout new_stderr

let system cmd =
  let pid = spawn shell [| shell; "-c"; cmd |] None false [| 0; 1; 2 |] in
  snd(waitpid_non_intr pid)

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc prog args envopt proc input output error =
  let pid =
    create_process_gen prog args envopt input output error in
  Hashtbl.add popen_processes proc pid

let open_process_args_in prog args =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let inchan = in_channel_of_descr in_read in
  begin
    try
      open_proc prog args None (Process_in inchan) stdin in_write stderr
    with e ->
      close_in inchan;
      close in_write;
      raise e
  end;
  close in_write;
  inchan

let open_process_args_out prog args =
  let (out_read, out_write) = pipe ~cloexec:true () in
  let outchan = out_channel_of_descr out_write in
  begin
    try
      open_proc prog args None (Process_out outchan) out_read stdout stderr
    with e ->
    close_out outchan;
    close out_read;
    raise e
  end;
  close out_read;
  outchan

let open_process_args prog args =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let (out_read, out_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write; raise e in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  begin
    try
      open_proc prog args None
                (Process(inchan, outchan)) out_read in_write stderr
    with e ->
      close out_read; close out_write;
      close in_read; close in_write;
      raise e
  end;
  close out_read;
  close in_write;
  (inchan, outchan)

let open_process_args_full prog args env =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let (out_read, out_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write; raise e in
  let (err_read, err_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write;
              close out_read; close out_write; raise e in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  begin
    try
      open_proc prog args (Some env) (Process_full(inchan, outchan, errchan))
                out_read in_write err_write
    with e ->
      close out_read; close out_write;
      close in_read; close in_write;
      close err_read; close err_write;
      raise e
  end;
  close out_read;
  close in_write;
  close err_write;
  (inchan, outchan, errchan)

let open_process_shell fn cmd =
  fn shell [|shell; "-c"; cmd|]
let open_process_in cmd =
  open_process_shell open_process_args_in cmd
let open_process_out cmd =
  open_process_shell open_process_args_out cmd
let open_process cmd =
  open_process_shell open_process_args cmd
let open_process_full cmd =
  open_process_shell open_process_args_full cmd

let find_proc_id fun_name proc =
  try
    Hashtbl.find popen_processes proc
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let remove_proc_id proc =
  Hashtbl.remove popen_processes proc

let process_in_pid inchan =
  find_proc_id "process_in_pid" (Process_in inchan)
let process_out_pid outchan =
  find_proc_id "process_out_pid" (Process_out outchan)
let process_pid (inchan, outchan) =
  find_proc_id "process_pid" (Process(inchan, outchan))
let process_full_pid (inchan, outchan, errchan) =
  find_proc_id "process_full_pid"
    (Process_full(inchan, outchan, errchan))

let close_process_in inchan =
  let proc = Process_in inchan in
  let pid = find_proc_id "close_process_in" proc in
  remove_proc_id proc;
  close_in inchan;
  snd(waitpid_non_intr pid)

let close_process_out outchan =
  let proc = Process_out outchan in
  let pid = find_proc_id "close_process_out" proc in
  remove_proc_id proc;
  (* The application may have closed [outchan] already to signal
     end-of-input to the process.  *)
  begin try close_out outchan with Sys_error _ -> () end;
  snd(waitpid_non_intr pid)

let close_process (inchan, outchan) =
  let proc = Process(inchan, outchan) in
  let pid = find_proc_id "close_process" proc in
  remove_proc_id proc;
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  snd(waitpid_non_intr pid)

let close_process_full (inchan, outchan, errchan) =
  let proc = Process_full(inchan, outchan, errchan) in
  let pid = find_proc_id "close_process_full" proc in
  remove_proc_id proc;
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  close_in errchan;
  snd(waitpid_non_intr pid)


(* Polling *)

external select :
  file_descr list -> file_descr list -> file_descr list -> float ->
        file_descr list * file_descr list * file_descr list = "caml_unix_select"

(* High-level network functions *)

let open_connection sockaddr =
  let sock =
    socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  try
    connect sock sockaddr;
    (in_channel_of_descr sock, out_channel_of_descr sock)
  with exn ->
    close sock; raise exn

let shutdown_connection inchan =
  shutdown (descr_of_in_channel inchan) SHUTDOWN_SEND

let rec accept_non_intr s =
  try accept ~cloexec:true s
  with Unix_error (EINTR, _, _) -> accept_non_intr s

let establish_server server_fun sockaddr =
  let sock =
    socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 5;
  while true do
    let (s, _caller) = accept_non_intr sock in
    (* The "double fork" trick, the process which calls server_fun will not
       leave a zombie process *)
    match fork() with
       0 -> if fork() <> 0 then _exit 0;
                                (* The child exits, the grandchild works *)
            close sock;
            let inchan = in_channel_of_descr s in
            let outchan = out_channel_of_descr s in
            server_fun inchan outchan;
            (* Do not close inchan nor outchan, as the server_fun could
               have done it already, and we are about to exit anyway
               (PR#3794) *)
            exit 0
    | id -> close s; ignore(waitpid_non_intr id) (* Reclaim the child *)
  done

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

external tcgetattr: file_descr -> terminal_io = "caml_unix_tcgetattr"

external tcsetattr: file_descr -> setattr_when -> terminal_io -> unit
               = "caml_unix_tcsetattr"
external tcsendbreak: file_descr -> int -> unit = "caml_unix_tcsendbreak"
external tcdrain: file_descr -> unit = "caml_unix_tcdrain"

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH

external tcflush: file_descr -> flush_queue -> unit = "caml_unix_tcflush"

type flow_action = TCOOFF | TCOON | TCIOFF | TCION

external tcflow: file_descr -> flow_action -> unit = "caml_unix_tcflow"

external setsid : unit -> int = "caml_unix_setsid"
