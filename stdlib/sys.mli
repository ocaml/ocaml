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

(* Module [Sys]: system interface *)

val argv: string array
        (* The command line arguments given to the process.
           The first element is the command name used to invoke the program.
           The following elements are the command-line arguments
           given to the program. *)
external file_exists: string -> bool = "sys_file_exists"
        (* Test if a file with the given name exists. *)
external remove: string -> unit = "sys_remove"
        (* Remove the given file name from the file system. *)
external rename : string -> string -> unit = "sys_rename"
        (* Rename a file. The first argument is the old name and the
           second is the new name. *)
external getenv: string -> string = "sys_getenv"
        (* Return the value associated to a variable in the process
           environment. Raise [Not_found] if the variable is unbound. *)
external command: string -> int = "sys_system_command"
        (* Execute the given shell command and return its exit code. *)
external time: unit -> float = "sys_time"
        (* Return the processor time, in seconds, used by the program
           since the beginning of execution. *)
external chdir: string -> unit = "sys_chdir"
        (* Change the current working directory of the process. *)
external getcwd: unit -> string = "sys_getcwd"
        (* Return the current working directory of the process. *)
val interactive: bool ref
        (* This reference is initially set to [false] in standalone
           programs and to [true] if the code is being executed under
           the interactive toplevel system [ocaml]. *)
val os_type: string
        (* Operating system currently executing the Caml program.
           One of ["Unix"], ["Win32"], or ["MacOS"]. *)
val word_size: int
        (* Size of one word on the machine currently executing the Caml
           program, in bits: 32 or 64. *)
val max_string_length: int
        (* Maximum length of a string. *)
val max_array_length: int
        (* Maximum length of an array. *)

(*** Signal handling *)

type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)
        (* What to do when receiving a signal:
-          [Signal_default]: take the default behavior
              (usually: abort the program)
-          [Signal_ignore]: ignore the signal
-          [Signal_handle f]: call function [f], giving it the signal
             number as argument. *)

external signal: int -> signal_behavior -> unit = "install_signal_handler"
        (* Set the behavior of the system on receipt of a given signal.
           The first argument is the signal number. *)

val sigabrt: int   (* Abnormal termination *)
val sigalrm: int   (* Timeout *)
val sigfpe: int    (* Arithmetic exception *)
val sighup: int    (* Hangup on controlling terminal *)
val sigill: int    (* Invalid hardware instruction *)
val sigint: int    (* Interactive interrupt (ctrl-C) *)
val sigkill: int   (* Termination (cannot be ignored) *)
val sigpipe: int   (* Broken pipe *)
val sigquit: int   (* Interactive termination *)
val sigsegv: int   (* Invalid memory reference *)
val sigterm: int   (* Termination *)
val sigusr1: int   (* Application-defined signal 1 *)
val sigusr2: int   (* Application-defined signal 2 *)
val sigchld: int   (* Child process terminated *)
val sigcont: int   (* Continue *)
val sigstop: int   (* Stop *)
val sigtstp: int   (* Interactive stop *)
val sigttin: int   (* Terminal read from background process *)
val sigttou: int   (* Terminal write from background process *)
val sigvtalrm: int (* Timeout in virtual time *)
val sigprof: int   (* Profiling interrupt *)
        (* Signal numbers for the standard POSIX signals. *)

exception Break
        (* Exception raised on interactive interrupt if [catch_break]
           is on. *)

val catch_break: bool -> unit
        (* [catch_break] governs whether interactive interrupt (ctrl-C)
           terminates the program or raises the [Break] exception. 
           Call [catch_break true] to enable raising [Break],
           and [catch_break false] to let the system
           terminate the program on user interrupt. *)
