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

(** Interface to the Unix system *)


(** {2 Error report} *)


(** The type of error codes. 
   Errors defined in the POSIX standard
   and additional errors, mostly BSD.
   All other errors are mapped to EUNKNOWNERR.
*)
type error =
    E2BIG               (** Argument list too long *)
  | EACCES              (** Permission denied *)
  | EAGAIN              (** Resource temporarily unavailable; try again *)
  | EBADF               (** Bad file descriptor *)
  | EBUSY               (** Resource unavailable *)
  | ECHILD              (** No child process *)
  | EDEADLK             (** Resource deadlock would occur *)
  | EDOM                (** Domain error for math functions, etc. *)
  | EEXIST              (** File exists *)
  | EFAULT              (** Bad address *)
  | EFBIG               (** File too large *)
  | EINTR               (** Function interrupted by signal *)
  | EINVAL              (** Invalid argument *)
  | EIO                 (** Hardware I/O error *)
  | EISDIR              (** Is a directory *)
  | EMFILE              (** Too many open files by the process *)
  | EMLINK              (** Too many links *)
  | ENAMETOOLONG        (** Filename too long *)
  | ENFILE              (** Too many open files in the system *)
  | ENODEV              (** No such device *)
  | ENOENT              (** No such file or directory *)
  | ENOEXEC             (** Not an executable file *)
  | ENOLCK              (** No locks available *)
  | ENOMEM              (** Not enough memory *)
  | ENOSPC              (** No space left on device *)
  | ENOSYS              (** Function not supported *)
  | ENOTDIR             (** Not a directory *)
  | ENOTEMPTY           (** Directory not empty *)
  | ENOTTY              (** Inappropriate I/O control operation *)
  | ENXIO               (** No such device or address *)
  | EPERM               (** Operation not permitted *)
  | EPIPE               (** Broken pipe *)
  | ERANGE              (** Result too large *)
  | EROFS               (** Read-only file system *)
  | ESPIPE              (** Invalid seek e.g. on a pipe *)
  | ESRCH               (** No such process *)
  | EXDEV               (** Invalid link *)

  | EWOULDBLOCK         (** Operation would block *)
  | EINPROGRESS         (** Operation now in progress *)
  | EALREADY            (** Operation already in progress *)
  | ENOTSOCK            (** Socket operation on non-socket *)
  | EDESTADDRREQ        (** Destination address required *)
  | EMSGSIZE            (** Message too long *)
  | EPROTOTYPE          (** Protocol wrong type for socket *)
  | ENOPROTOOPT         (** Protocol not available *)
  | EPROTONOSUPPORT     (** Protocol not supported *)
  | ESOCKTNOSUPPORT     (** Socket type not supported *)
  | EOPNOTSUPP          (** Operation not supported on socket *)
  | EPFNOSUPPORT        (** Protocol family not supported *)
  | EAFNOSUPPORT        (** Address family not supported by protocol family *)
  | EADDRINUSE          (** Address already in use *)
  | EADDRNOTAVAIL       (** Can't assign requested address *)
  | ENETDOWN            (** Network is down *)
  | ENETUNREACH         (** Network is unreachable *)
  | ENETRESET           (** Network dropped connection on reset *)
  | ECONNABORTED        (** Software caused connection abort *)
  | ECONNRESET          (** Connection reset by peer *)
  | ENOBUFS             (** No buffer space available *)
  | EISCONN             (** Socket is already connected *)
  | ENOTCONN            (** Socket is not connected *)
  | ESHUTDOWN           (** Can't send after socket shutdown *)
  | ETOOMANYREFS        (** Too many references: can't splice *)
  | ETIMEDOUT           (** Connection timed out *)
  | ECONNREFUSED        (** Connection refused *)
  | EHOSTDOWN           (** Host is down *)
  | EHOSTUNREACH        (** No route to host *)
  | ELOOP               (** Too many levels of symbolic links *)

  | EUNKNOWNERR of int  (** Unknown error *)


(** Raised by the system calls below when an error is encountered.
   The first component is the error code; the second component
   is the function name; the third component is the string parameter
   to the function, if it has one, or the empty string otherwise. *)
exception Unix_error of error * string * string

(** Return a string describing the given error code. *)
val error_message : error -> string

(** [handle_unix_error f x] applies [f] to [x] and returns the result.
   If the exception [Unix_error] is raised, it prints a message
   describing the error and exits with code 2. *)
val handle_unix_error : ('a -> 'b) -> 'a -> 'b


(** {2 Access to the process environment} *)


(** Return the process environment, as an array of strings
    with the format ``variable=value''. *)
val environment : unit -> string array
        
(** Return the value associated to a variable in the process
   environment. Raise [Not_found] if the variable is unbound.
   (This function is identical to [Sys.getenv].) *)
val getenv: string -> string

(** [Unix.putenv name value] sets the value associated to a
   variable in the process environment.
   [name] is the name of the environment variable,
   and [value] its new associated value. *)
val putenv: string -> string -> unit


(** {2 Process handling} *)


(** The termination status of a process. *)
type process_status =
    WEXITED of int 
        (** The process terminated normally by [exit]; 
           the argument is the return code. *)
  | WSIGNALED of int
        (** The process was killed by a signal;
           the argument is the signal number. *)
  | WSTOPPED of int
        (** The process was stopped by a signal; the argument is the
           signal number. *)

(** Flags for {!Unix.waitpid}. *)
type wait_flag =
    WNOHANG (** do not block if no child has
               died yet, but immediately return with a pid equal to 0.*)
  | WUNTRACED (** report also the children that receive stop signals. *)

(** [execv prog args] execute the program in file [prog], with
   the arguments [args], and the current process environment. *)
val execv : string -> string array -> unit

(** Same as {!Unix.execv}, except that the third argument provides the
   environment to the program executed. *)
val execve : string -> string array -> string array -> unit

(** Same as {!Unix.execv} respectively, except that
   the program is searched in the path. *)
val execvp : string -> string array -> unit

(** Same as {!Unix.execvp} respectively, except that
   the program is searched in the path. *)
val execvpe : string -> string array -> string array -> unit

(** Fork a new process. The returned integer is 0 for the child
   process, the pid of the child process for the parent process. *)
val fork : unit -> int

(** Wait until one of the children processes die, and return its pid
   and termination status. *)
val wait : unit -> int * process_status

(** Same as {!Unix.wait}, but waits for the process whose pid is given.
   A pid of [-1] means wait for any child.
   A pid of [0] means wait for any child in the same process group
   as the current process.
   Negative pid arguments represent process groups.
   The list of options indicates whether [waitpid] should return
   immediately without waiting, or also report stopped children. *)
val waitpid : wait_flag list -> int -> int * process_status

(** Execute the given command, wait until it terminates, and return
   its termination status. The string is interpreted by the shell
   [/bin/sh] and therefore can contain redirections, quotes, variables,
   etc. The result [WEXITED 127] indicates that the shell couldn't
   be executed. *)
val system : string -> process_status

(** Return the pid of the process. *)
val getpid : unit -> int

(** Return the pid of the parent process. *)
val getppid : unit -> int

(** Change the process priority. The integer argument is added to the
   ``nice'' value. (Higher values of the ``nice'' value mean
   lower priorities.) Return the new nice value. *)
val nice : int -> int


(** {2 Basic file input/output} *)


(** The abstract type of file descriptors. *)
type file_descr

(** File descriptor for standard input.*)
val stdin : file_descr

(** File descriptor for standard output.*)
val stdout : file_descr

(** File descriptor for standard standard error. *)
val stderr : file_descr

(** The flags to {!Unix.openfile}. *)
type open_flag =
    O_RDONLY                    (** Open for reading *)
  | O_WRONLY                    (** Open for writing *)
  | O_RDWR                      (** Open for reading and writing *)
  | O_NONBLOCK                  (** Open in non-blocking mode *)
  | O_APPEND                    (** Open for append *)
  | O_CREAT                     (** Create if nonexistent *)
  | O_TRUNC                     (** Truncate to 0 length if existing *)
  | O_EXCL                      (** Fail if existing *)


(** The type of file access rights. *)
type file_perm = int

(** Open the named file with the given flags. Third argument is
   the permissions to give to the file if it is created. Return
   a file descriptor on the named file. *)
val openfile : string -> open_flag list -> file_perm -> file_descr

(** Close a file descriptor. *)
val close : file_descr -> unit

(** [read fd buff ofs len] reads [len] characters from descriptor
   [fd], storing them in string [buff], starting at position [ofs]
   in string [buff]. Return the number of characters actually read. *)
val read : file_descr -> string -> int -> int -> int

(** [write fd buff ofs len] writes [len] characters to descriptor
   [fd], taking them from string [buff], starting at position [ofs]
   in string [buff]. Return the number of characters actually
   written. *)
val write : file_descr -> string -> int -> int -> int



(** {2 Interfacing with the standard input/output library} *)



(** Create an input channel reading from the given descriptor.
   The channel is initially in binary mode; use
   [set_binary_mode_in ic false] if text mode is desired. *)
val in_channel_of_descr : file_descr -> in_channel

(** Create an output channel writing on the given descriptor.
   The channel is initially in binary mode; use
   [set_binary_mode_out oc false] if text mode is desired. *)
val out_channel_of_descr : file_descr -> out_channel

(** Return the descriptor corresponding to an input channel. *)
val descr_of_in_channel : in_channel -> file_descr

(** Return the descriptor corresponding to an output channel. *)
val descr_of_out_channel : out_channel -> file_descr


(** {2 Seeking and truncating} *)


(** Positioning modes for {!Unix.lseek}. *)
type seek_command =
    SEEK_SET (** indicates positions relative to the beginning of the file *)
  | SEEK_CUR (** indicates positions relative to the current position *)
  | SEEK_END (** indicates positions relative to the end of the file *)


(** Set the current position for a file descriptor *)
val lseek : file_descr -> int -> seek_command -> int

(** Truncates the named file to the given size. *)
val truncate : string -> int -> unit

(** Truncates the file corresponding to the given descriptor
   to the given size. *)
val ftruncate : file_descr -> int -> unit


(** {2 File statistics} *)


type file_kind =
    S_REG                       (** Regular file *)
  | S_DIR                       (** Directory *)
  | S_CHR                       (** Character device *)
  | S_BLK                       (** Block device *)
  | S_LNK                       (** Symbolic link *)
  | S_FIFO                      (** Named pipe *)
  | S_SOCK                      (** Socket *)

(** The informations returned by the {!Unix.stat} calls. *)
type stats =
  { st_dev : int;               (** Device number *)
    st_ino : int;               (** Inode number *)
    st_kind : file_kind;        (** Kind of the file *)
    st_perm : file_perm;        (** Access rights *)
    st_nlink : int;             (** Number of links *)
    st_uid : int;               (** User id of the owner *)
    st_gid : int;               (** Group ID of the file's group *)
    st_rdev : int;              (** Device minor number *)
    st_size : int;              (** Size in bytes *)
    st_atime : float;           (** Last access time *)
    st_mtime : float;           (** Last modification time *)
    st_ctime : float;           (** Last status change time *)
  } 

(** Return the informations for the named file. *)
val stat : string -> stats

(** Same as {!Unix.stat}, but in case the file is a symbolic link,
   return the informations for the link itself. *)
val lstat : string -> stats

(** Return the informations for the file associated with the given
   descriptor. *)
val fstat : file_descr -> stats


(** {2 Operations on file names} *)


(** Removes the named file *)
val unlink : string -> unit

(** [rename old new] changes the name of a file from [old] to [new]. *)
val rename : string -> string -> unit

(** [link source dest] creates a hard link named [dest] to the file
   named [source]. *)
val link : string -> string -> unit


(** {2 File permissions and ownership} *)


(** Flags for the {!Unix.access} call. *)
type access_permission =
    R_OK                        (** Read permission *)
  | W_OK                        (** Write permission *)
  | X_OK                        (** Execution permission *)
  | F_OK                        (** File exists *)


(** Change the permissions of the named file. *)
val chmod : string -> file_perm -> unit

(** Change the permissions of an opened file. *)
val fchmod : file_descr -> file_perm -> unit

(** Change the owner uid and owner gid of the named file. *)
val chown : string -> int -> int -> unit

(** Change the owner uid and owner gid of an opened file. *)
val fchown : file_descr -> int -> int -> unit

(** Set the process creation mask, and return the previous mask. *)
val umask : int -> int

(** Check that the process has the given permissions over the named
   file. Raise [Unix_error] otherwise. *)
val access : string -> access_permission list -> unit


(** {2 Operations on file descriptors} *)


(** Return a new file descriptor referencing the same file as
   the given descriptor. *)
val dup : file_descr -> file_descr

(** [dup2 fd1 fd2] duplicates [fd1] to [fd2], closing [fd2] if already
   opened. *)
val dup2 : file_descr -> file_descr -> unit

(** Set the ``non-blocking'' flag on the given descriptor.
   When the non-blocking flag is set, reading on a descriptor
   on which there is temporarily no data available raises the
   [EAGAIN] or [EWOULDBLOCK] error instead of blocking;
   writing on a descriptor on which there is temporarily no room
   for writing also raises [EAGAIN] or [EWOULDBLOCK]. *)
val set_nonblock : file_descr -> unit

(** Clear the ``non-blocking'' flag on the given descriptor.
   See {!Unix.set_nonblock}.*)
val clear_nonblock : file_descr -> unit

(** Set the ``close-on-exec'' flag on the given descriptor.
   A descriptor with the close-on-exec flag is automatically
   closed when the current process starts another program with
   one of the [exec] functions. *)
val set_close_on_exec : file_descr -> unit

(** Clear the ``close-on-exec'' flag on the given descriptor.
   See {!Unix.set_close_on_exec}.*)
val clear_close_on_exec : file_descr -> unit


(** {2 Directories} *)


(** Create a directory with the given permissions. *)
val mkdir : string -> file_perm -> unit

(** Remove an empty directory. *)
val rmdir : string -> unit

(** Change the process working directory. *)
val chdir : string -> unit

(** Return the name of the current working directory. *)
val getcwd : unit -> string

(** Change the process root directory. *)
val chroot : string -> unit

(** The type of descriptors over opened directories. *)
type dir_handle

(** Open a descriptor on a directory *)
val opendir : string -> dir_handle

(** Return the next entry in a directory.
   @raise End_of_file when the end of the directory has been reached. *)
val readdir : dir_handle -> string

(** Reposition the descriptor to the beginning of the directory *)
val rewinddir : dir_handle -> unit

(** Close a directory descriptor. *)
val closedir : dir_handle -> unit



(** {2 Pipes and redirections} *)


(** Create a pipe. The first component of the result is opened
   for reading, that's the exit to the pipe. The second component is
   opened for writing, that's the entrance to the pipe. *)
val pipe : unit -> file_descr * file_descr

(** Create a named pipe with the given permissions. *)
val mkfifo : string -> file_perm -> unit


(** {2 High-level process and redirection management} *)


(** [create_process prog args new_stdin new_stdout new_stderr]
   forks a new process that executes the program
   in file [prog], with arguments [args]. The pid of the new
   process is returned immediately; the new process executes
   concurrently with the current process.
   The standard input and outputs of the new process are connected
   to the descriptors [new_stdin], [new_stdout] and [new_stderr].
   Passing e.g. [stdout] for [new_stdout] prevents the redirection
   and causes the new process to have the same standard output
   as the current process.
   The executable file [prog] is searched in the path.
   The new process has the same environment as the current process.
   All file descriptors of the current process are closed in the
   new process, except those redirected to standard input and
   outputs. *)
val create_process :
  string -> string array ->
  file_descr -> file_descr -> file_descr -> int

(** [create_process_env prog args env new_stdin new_stdout new_stderr]
   works as {!Unix.create_process}, except that the extra argument
   [env] specifies the environment passed to the program. *)
val create_process_env :
  string -> string array -> string array ->
  file_descr -> file_descr -> file_descr -> int


(** High-level pipe and process management. These functions
   (with {!Unix.open_process_out} and {!Unix.open_process})
   run the given command in parallel with the program,
   and return channels connected to the standard input and/or
   the standard output of the command. The command is interpreted
   by the shell [/bin/sh] (cf. [system]). Warning: writes on channels
   are buffered, hence be careful to call {!Pervasives.flush} at the right times
   to ensure correct synchronization. *)
val open_process_in: string -> in_channel

(** See {!Unix.open_process_in}. *)
val open_process_out: string -> out_channel

(** See {!Unix.open_process_in}. *)
val open_process: string -> in_channel * out_channel

(** Similar to {!Unix.open_process}, but the second argument specifies
   the environment passed to the command.  The result is a triple
   of channels connected to the standard output, standard input,
   and standard error of the command. *)
val open_process_full:
      string -> string array -> in_channel * out_channel * in_channel

(** Close channels opened by {!Unix.open_process_in}, 
   wait for the associated command to terminate,
   and return its termination status. *)
val close_process_in: in_channel -> process_status

(** Close channels opened by {!Unix.open_process_out}, 
   wait for the associated command to terminate,
   and return its termination status. *)
val close_process_out: out_channel -> process_status

(** Close channels opened by {!Unix.open_process}, 
   wait for the associated command to terminate,
   and return its termination status. *)
val close_process: in_channel * out_channel -> process_status

(** Close channels opened by {!Unix.open_process_full}, 
   wait for the associated command to terminate,
   and return its termination status. *)
val close_process_full: in_channel * out_channel * in_channel -> process_status


(** {2 Symbolic links} *)


(** [symlink source dest] creates the file [dest] as a symbolic link
   to the file [source]. *)
val symlink : string -> string -> unit

(** Read the contents of a link. *)
val readlink : string -> string


(** {2 Polling} *)


(** Wait until some input/output operations become possible on
   some channels. The three list arguments are, respectively, a set
   of descriptors to check for reading (first argument), for writing
   (second argument), or for exceptional conditions (third argument).
   The fourth argument is the maximal timeout, in seconds; a
   negative fourth argument means no timeout (unbounded wait).
   The result is composed of three sets of descriptors: those ready
   for reading (first component), ready for writing (second component),
   and over which an exceptional condition is pending (third
   component). *)
val select :
  file_descr list -> file_descr list -> file_descr list ->
  float ->
        file_descr list * file_descr list * file_descr list

(** {2 Locking} *)


(** Commands for {!Unix.lockf}. *)
type lock_command =
    F_ULOCK       (** Unlock a region *)
  | F_LOCK        (** Lock a region for writing, and block if already locked *)
  | F_TLOCK       (** Lock a region for writing, or fail if already locked *)
  | F_TEST        (** Test a region for other process locks *)
  | F_RLOCK       (** Lock a region for reading, and block if already locked *)
  | F_TRLOCK      (** Lock a region for reading, or fail if already locked *)

(** [lockf fd cmd size] puts a lock on a region of the file opened
   as [fd]. The region starts at the current read/write position for
   [fd] (as set by {!Unix.lseek}), and extends [size] bytes forward if
   [size] is positive, [size] bytes backwards if [size] is negative,
   or to the end of the file if [size] is zero.
   A write lock (set with [F_LOCK] or [F_TLOCK]) prevents any other
   process from acquiring a read or write lock on the region.
   A read lock (set with [F_RLOCK] or [F_TRLOCK]) prevents any other
   process from acquiring a write lock on the region, but lets
   other processes acquire read locks on it. *)
val lockf : file_descr -> lock_command -> int -> unit


(** {2 Signals}
   Note: installation of signal handlers is performed via
   the functions {!Sys.signal} and {!Sys.set_signal}. 
*)

(** [kill pid sig] sends signal number [sig] to the process
   with id [pid]. *)
val kill : int -> int -> unit

type sigprocmask_command = SIG_SETMASK | SIG_BLOCK | SIG_UNBLOCK

(** [sigprocmask cmd sigs] changes the set of blocked signals.
   If [cmd] is [SIG_SETMASK], blocked signals are set to those in
   the list [sigs].
   If [cmd] is [SIG_BLOCK], the signals in [sigs] are added to
   the set of blocked signals.
   If [cmd] is [SIG_UNBLOCK], the signals in [sigs] are removed
   from the set of blocked signals.
   [sigprocmask] returns the set of previously blocked signals. *)
val sigprocmask: sigprocmask_command -> int list -> int list

(** Return the set of blocked signals that are currently pending. *)
val sigpending: unit -> int list

(** [sigsuspend sigs] atomically sets the blocked signals to [sig]
   and waits for a non-ignored, non-blocked signal to be delivered.
   On return, the blocked signals are reset to their initial value. *)
val sigsuspend: int list -> unit

(** Wait until a non-ignored, non-blocked signal is delivered. *)
val pause : unit -> unit


(** {2 Time functions} *)


(** The execution times (CPU times) of a process. *)
type process_times =
  { tms_utime : float;  (** User time for the process *)
    tms_stime : float;  (** System time for the process *)
    tms_cutime : float; (** User time for the children processes *)
    tms_cstime : float; (** System time for the children processes *)
  }

(** The type representing wallclock time and calendar date. *)
type tm =
  { tm_sec : int;               (** Seconds 0..59 *)
    tm_min : int;               (** Minutes 0..59 *)
    tm_hour : int;              (** Hours 0..23 *)
    tm_mday : int;              (** Day of month 1..31 *)
    tm_mon : int;               (** Month of year 0..11 *)
    tm_year : int;              (** Year - 1900 *)
    tm_wday : int;              (** Day of week (Sunday is 0) *)
    tm_yday : int;              (** Day of year 0..365 *)
    tm_isdst : bool;            (** Daylight time savings in effect *)
  } 


(** Return the current time since 00:00:00 GMT, Jan. 1, 1970,
   in seconds. *)
val time : unit -> float

(** Same as {!Unix.time}, but with resolution better than 1 second. *)
val gettimeofday : unit -> float

(** Convert a time in seconds, as returned by {!Unix.time}, into a date and
   a time. Assumes Greenwich meridian time zone, also known as UTC. *)
val gmtime : float -> tm

(** Convert a time in seconds, as returned by {!Unix.time}, into a date and
   a time. Assumes the local time zone. *)
val localtime : float -> tm

(** Convert a date and time, specified by the [tm] argument, into
   a time in seconds, as returned by {!Unix.time}. Also return a normalized
   copy of the given [tm] record, with the [tm_wday], [tm_yday],
   and [tm_isdst] fields recomputed from the other fields.
   The [tm] argument is interpreted in the local time zone. *)
val mktime : tm -> float * tm

(** Schedule a [SIGALRM] signal after the given number of seconds. *)
val alarm : int -> int

(** Stop execution for the given number of seconds. *)
val sleep : int -> unit

(** Return the execution times of the process. *)
val times : unit -> process_times

(** Set the last access time (second arg) and last modification time
   (third arg) for a file. Times are expressed in seconds from
   00:00:00 GMT, Jan. 1, 1970. *)
val utimes : string -> float -> float -> unit

(** The three kinds of interval timers. *)
type interval_timer =
    ITIMER_REAL 
      (** decrements in real time, and sends the signal [SIGALRM] when expired.*)
  | ITIMER_VIRTUAL
      (**  decrements in process virtual time, and sends [SIGVTALRM] when expired. *)
  | ITIMER_PROF
      (** (for profiling) decrements both when the process
         is running and when the system is running on behalf of the
         process; it sends [SIGPROF] when expired. *)

(** The type describing the status of an interval timer *)
type interval_timer_status =
  { it_interval: float;         (** Period *)
    it_value: float;            (** Current value of the timer *)
  } 

(** Return the current status of the given interval timer. *)
val getitimer: interval_timer -> interval_timer_status

(** [setitimer t s] sets the interval timer [t] and returns
   its previous status. The [s] argument is interpreted as follows:
   [s.it_value], if nonzero, is the time to the next timer expiration;
   [s.it_interval], if nonzero, specifies a value to
   be used in reloading it_value when the timer expires.
   Setting [s.it_value] to zero disable the timer.
   Setting [s.it_interval] to zero causes the timer to be disabled
   after its next expiration. *)
val setitimer:
  interval_timer -> interval_timer_status -> interval_timer_status


(** {2 User id, group id} *)


(** Return the user id of the user executing the process. *)
val getuid : unit -> int

(** Return the effective user id under which the process runs. *)
val geteuid : unit -> int

(** Set the real user id and effective user id for the process. *)
val setuid : int -> unit

(** Return the group id of the user executing the process. *)
val getgid : unit -> int

(** Return the effective group id under which the process runs. *)
val getegid : unit -> int

(** Set the real group id and effective group id for the process. *)
val setgid : int -> unit

(** Return the list of groups to which the user executing the process
   belongs. *)
val getgroups : unit -> int array

(** Structure of entries in the [passwd] database. *)
type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string;
  }

(** Structure of entries in the [groups] database. *)
type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array;
  }

(** Return the login name of the user executing the process. *)
val getlogin : unit -> string

(** Find an entry in [passwd] with the given name, or raise
   [Not_found]. *)
val getpwnam : string -> passwd_entry

(** Find an entry in [group] with the given name, or raise
   [Not_found]. *)
val getgrnam : string -> group_entry

(** Find an entry in [passwd] with the given user id, or raise
   [Not_found]. *)
val getpwuid : int -> passwd_entry

(** Find an entry in [group] with the given group id, or raise
   [Not_found]. *)
val getgrgid : int -> group_entry


(** {2 Internet addresses} *)


(** The abstract type of Internet addresses. *)
type inet_addr

(** Conversions between string with the format [XXX.YYY.ZZZ.TTT]
   and Internet addresses. [inet_addr_of_string] raises [Failure]
   when given a string that does not match this format. *)
val inet_addr_of_string : string -> inet_addr

(** See {!Unix.inet_addr_of_string}. *)
val string_of_inet_addr : inet_addr -> string

(** A special Internet address, for use only with [bind], representing
   all the Internet addresses that the host machine possesses. *)
val inet_addr_any : inet_addr


(** {2 Sockets} *)


(** The type of socket domains. *)
type socket_domain =
    PF_UNIX                     (** Unix domain *)
  | PF_INET                     (** Internet domain *)

(** The type of socket kinds, specifying the semantics of
   communications. *)
type socket_type =
    SOCK_STREAM                 (** Stream socket *)
  | SOCK_DGRAM                  (** Datagram socket *)
  | SOCK_RAW                    (** Raw socket *)
  | SOCK_SEQPACKET              (** Sequenced packets socket *)

(** The type of socket addresses. [ADDR_UNIX name] is a socket
   address in the Unix domain; [name] is a file name in the file
   system. [ADDR_INET(addr,port)] is a socket address in the Internet
   domain; [addr] is the Internet address of the machine, and
   [port] is the port number. *)
type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int

(** Create a new socket in the given domain, and with the
   given kind. The third argument is the protocol type; 0 selects
   the default protocol for that kind of sockets. *)
val socket :
  socket_domain -> socket_type -> int -> file_descr

(** Create a pair of unnamed sockets, connected together. *)
val socketpair :
  socket_domain -> socket_type -> int ->
    file_descr * file_descr

(** Accept connections on the given socket. The returned descriptor
   is a socket connected to the client; the returned address is
   the address of the connecting client. *)
val accept : file_descr -> file_descr * sockaddr

(** Bind a socket to an address. *)
val bind : file_descr -> sockaddr -> unit

(** Connect a socket to an address. *)
val connect : file_descr -> sockaddr -> unit

(** Set up a socket for receiving connection requests. The integer
   argument is the maximal number of pending requests. *)
val listen : file_descr -> int -> unit

(** The type of commands for [shutdown]. *)
type shutdown_command =
    SHUTDOWN_RECEIVE            (** Close for receiving *)
  | SHUTDOWN_SEND               (** Close for sending *)
  | SHUTDOWN_ALL                (** Close both *)


(** Shutdown a socket connection. [SHUTDOWN_SEND] as second argument
   causes reads on the other end of the connection to return
   an end-of-file condition.
   [SHUTDOWN_RECEIVE] causes writes on the other end of the connection
   to return a closed pipe condition ([SIGPIPE] signal). *)
val shutdown : file_descr -> shutdown_command -> unit

(** Return the address of the given socket. *)
val getsockname : file_descr -> sockaddr

(** Return the address of the host connected to the given socket. *)
val getpeername : file_descr -> sockaddr

(** The flags for {!Unix.recv},  {!Unix.recvfrom}, 
   {!Unix.send} and {!Unix.sendto}. *)
type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

(** Receive data from an unconnected socket. *)
val recv :
        file_descr -> string -> int -> int
          -> msg_flag list -> int

(** Receive data from an unconnected socket. *)
val recvfrom :
        file_descr -> string -> int -> int
          -> msg_flag list -> int * sockaddr

(** Send data over an unconnected socket. *)
val send : file_descr -> string -> int -> int
          -> msg_flag list -> int

(** Send data over an unconnected socket. *)
val sendto :
        file_descr -> string -> int -> int
          -> msg_flag list -> sockaddr -> int



(** {2 Socket options} *)


(** The socket options that can be consulted with {!Unix.getsockopt}
   and modified with {!Unix.setsockopt}.  These options have a boolean
   ([true]/[false]) value. *)
type socket_bool_option =
    SO_DEBUG       (** Record debugging information *)
  | SO_BROADCAST   (** Permit sending of broadcast messages *)
  | SO_REUSEADDR   (** Allow reuse of local addresses for bind *)
  | SO_KEEPALIVE   (** Keep connection active *)
  | SO_DONTROUTE   (** Bypass the standard routing algorithms *)
  | SO_OOBINLINE   (** Leave out-of-band data in line *)
  | SO_ACCEPTCONN  (** Report whether socket listening is enabled *)

(** The socket options that can be consulted with {!Unix.getsockopt_int}
   and modified with {!Unix.setsockopt_int}.  These options have an
   integer value. *)
type socket_int_option =
    SO_SNDBUF      (** Size of send buffer *)
  | SO_RCVBUF      (** Size of received buffer *)
  | SO_ERROR       (** Report the error status and clear it *)
  | SO_TYPE        (** Report the socket type *)
  | SO_RCVLOWAT    (** Minimum number of bytes to process for input operations *)
  | SO_SNDLOWAT    (** Minimum number of bytes to process for output operations *)

(** The socket options that can be consulted with {!Unix.getsockopt_optint}
   and modified with {!Unix.setsockopt_optint}.  These options have a
   value of type [int option], with [None] meaning ``disabled''. *)
type socket_optint_option = 
    SO_LINGER      (** Whether to linger on closed connections
                      that have data present, and for how long
                      (in seconds) *)
                  
(** The socket options that can be consulted with {!Unix.getsockopt_float}
   and modified with {!Unix.setsockopt_float}.  These options have a
   floating-point value representing a time in seconds.
   The value 0 means infinite timeout. *)
type socket_float_option =
    SO_RCVTIMEO    (** Timeout for input operations *)
  | SO_SNDTIMEO    (** Timeout for output operations *)

(** Return the current status of a boolean-valued option
   in the given socket. *)
val getsockopt : file_descr -> socket_bool_option -> bool

(** Set or clear a boolean-valued option in the given socket. *)
val setsockopt : file_descr -> socket_bool_option -> bool -> unit

(** Same as {!Unix.getsockopt} for an integer-valued socket option. *)
external getsockopt_int : file_descr -> socket_int_option -> int
                                          = "unix_getsockopt_int"

(** Same as {!Unix.setsockopt} for an integer-valued socket option. *)
external setsockopt_int : file_descr -> socket_int_option -> int -> unit
                                          = "unix_setsockopt_int"

(** Same as {!Unix.getsockopt} for a socket option whose value is an [int option]. *)
external getsockopt_optint : file_descr -> socket_optint_option -> int option
                                          = "unix_getsockopt_optint"

(** Same as {!Unix.setsockopt} for a socket option whose value is an [int option]. *)
external setsockopt_optint : file_descr -> socket_optint_option -> int option -> unit
                                          = "unix_setsockopt_optint"

(** Same as {!Unix.getsockopt} for a socket option whose value is a floating-point number. *)
external getsockopt_float : file_descr -> socket_float_option -> float
                                          = "unix_getsockopt_float"

(** Same as {!Unix.setsockopt} for a socket option whose value is a floating-point number. *)
external setsockopt_float : file_descr -> socket_float_option -> float -> unit
                                          = "unix_setsockopt_float"

(** {2 High-level network connection functions} *)


(** Connect to a server at the given address.
   Return a pair of buffered channels connected to the server.
   Remember to call {!Pervasives.flush} on the output channel at the right times
   to ensure correct synchronization. *)
val open_connection : sockaddr -> in_channel * out_channel

(** ``Shut down'' a connection established with {!Unix.open_connection};
   that is, transmit an end-of-file condition to the server reading
   on the other side of the connection. *)
val shutdown_connection : in_channel -> unit

(** Establish a server on the given address.
   The function given as first argument is called for each connection
   with two buffered channels connected to the client. A new process
   is created for each connection. The function {!Unix.establish_server}
   never returns normally. *)
val establish_server : (in_channel -> out_channel -> unit) ->
                       sockaddr -> unit


(** {2 Host and protocol databases} *)


(** Structure of entries in the [hosts] database. *)
type host_entry =
  { h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array;
  }

(** Structure of entries in the [protocols] database. *)
type protocol_entry =
  { p_name : string;
    p_aliases : string array;
    p_proto : int;
  }

(** Structure of entries in the [services] database. *)
type service_entry =
  { s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string;
  }

(** Return the name of the local host. *)
val gethostname : unit -> string

(** Find an entry in [hosts] with the given name, or raise
   [Not_found]. *)
val gethostbyname : string -> host_entry

(** Find an entry in [hosts] with the given address, or raise
   [Not_found]. *)
val gethostbyaddr : inet_addr -> host_entry

(** Find an entry in [protocols] with the given name, or raise
   [Not_found]. *)
val getprotobyname : string -> protocol_entry

(** Find an entry in [protocols] with the given protocol number,
   or raise [Not_found]. *)
val getprotobynumber : int -> protocol_entry

(** Find an entry in [services] with the given name, or raise
   [Not_found]. *)
val getservbyname : string -> string -> service_entry

(** Find an entry in [services] with the given service number,
   or raise [Not_found]. *)
val getservbyport : int -> string -> service_entry



(** {2 Terminal interface} *)


(** The following functions implement the POSIX standard terminal
   interface. They provide control over asynchronous communication ports
   and pseudo-terminals. Refer to the [termios] man page for a
   complete description. *)

type terminal_io = {
  (** Input modes: *)
    mutable c_ignbrk: bool;  (** Ignore the break condition. *)
    mutable c_brkint: bool;  (** Signal interrupt on break condition. *)
    mutable c_ignpar: bool;  (** Ignore characters with parity errors. *)
    mutable c_parmrk: bool;  (** Mark parity errors. *)
    mutable c_inpck: bool;   (** Enable parity check on input. *)
    mutable c_istrip: bool;  (** Strip 8th bit on input characters. *)
    mutable c_inlcr: bool;   (** Map NL to CR on input. *)
    mutable c_igncr: bool;   (** Ignore CR on input. *)
    mutable c_icrnl: bool;   (** Map CR to NL on input. *)
    mutable c_ixon: bool;    (** Recognize XON/XOFF characters on input. *)
    mutable c_ixoff: bool;   (** Emit XON/XOFF chars to control input flow. *)
  (** Output modes: *)
    mutable c_opost: bool;   (** Enable output processing. *)
  (** Control modes: *)
    mutable c_obaud: int;    (** Output baud rate (0 means close connection).*)
    mutable c_ibaud: int;    (** Input baud rate. *)
    mutable c_csize: int;    (** Number of bits per character (5-8). *)
    mutable c_cstopb: int;   (** Number of stop bits (1-2). *)
    mutable c_cread: bool;   (** Reception is enabled. *)
    mutable c_parenb: bool;  (** Enable parity generation and detection. *)
    mutable c_parodd: bool;  (** Specify odd parity instead of even. *)
    mutable c_hupcl: bool;   (** Hang up on last close. *)
    mutable c_clocal: bool;  (** Ignore modem status lines. *)
  (** Local modes: *)
    mutable c_isig: bool;    (** Generate signal on INTR, QUIT, SUSP. *)
    mutable c_icanon: bool;  (** Enable canonical processing
                                (line buffering and editing) *)
    mutable c_noflsh: bool;  (** Disable flush after INTR, QUIT, SUSP. *)
    mutable c_echo: bool;    (** Echo input characters. *)
    mutable c_echoe: bool;   (** Echo ERASE (to erase previous character). *)
    mutable c_echok: bool;   (** Echo KILL (to erase the current line). *)
    mutable c_echonl: bool;  (** Echo NL even if c_echo is not set. *)
  (** Control characters: *)
    mutable c_vintr: char;   (** Interrupt character (usually ctrl-C). *)
    mutable c_vquit: char;   (** Quit character (usually ctrl-\). *)
    mutable c_verase: char;  (** Erase character (usually DEL or ctrl-H). *)
    mutable c_vkill: char;   (** Kill line character (usually ctrl-U). *)
    mutable c_veof: char;    (** End-of-file character (usually ctrl-D). *)
    mutable c_veol: char;    (** Alternate end-of-line char. (usually none). *)
    mutable c_vmin: int;     (** Minimum number of characters to read
                                before the read request is satisfied. *)
    mutable c_vtime: int;    (** Maximum read wait (in 0.1s units). *)
    mutable c_vstart: char;  (** Start character (usually ctrl-Q). *)
    mutable c_vstop: char;   (** Stop character (usually ctrl-S). *)
  }

(** Return the status of the terminal referred to by the given
   file descriptor. *)
val tcgetattr: file_descr -> terminal_io

type setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

(** Set the status of the terminal referred to by the given
   file descriptor. The second argument indicates when the
   status change takes place: immediately ([TCSANOW]),
   when all pending output has been transmitted ([TCSADRAIN]),
   or after flushing all input that has been received but not
   read ([TCSAFLUSH]). [TCSADRAIN] is recommended when changing
   the output parameters; [TCSAFLUSH], when changing the input
   parameters. *)
val tcsetattr: file_descr -> setattr_when -> terminal_io -> unit

(** Send a break condition on the given file descriptor.
   The second argument is the duration of the break, in 0.1s units;
   0 means standard duration (0.25s). *)
val tcsendbreak: file_descr -> int -> unit

(** Waits until all output written on the given file descriptor
   has been transmitted. *)
val tcdrain: file_descr -> unit

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH

(** Discard data written on the given file descriptor but not yet
   transmitted, or data received but not yet read, depending on the
   second argument: [TCIFLUSH] flushes data received but not read,
   [TCOFLUSH] flushes data written but not transmitted, and
   [TCIOFLUSH] flushes both. *)
val tcflush: file_descr -> flush_queue -> unit

type flow_action = TCOOFF | TCOON | TCIOFF | TCION

(** Suspend or restart reception or transmission of data on
   the given file descriptor, depending on the second argument:
   [TCOOFF] suspends output, [TCOON] restarts output,
   [TCIOFF] transmits a STOP character to suspend input,
   and [TCION] transmits a START character to restart input. *)
val tcflow: file_descr -> flow_action -> unit

(** Put the calling process in a new session and detach it from
   its controlling terminal. *)
val setsid : unit -> int
