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

(** System interface. *)

(** The command line arguments given to the process.
   The first element is the command name used to invoke the program.
   The following elements are the command-line arguments
   given to the program. *)
val argv: string array

(** Test if a file with the given name exists. *)
external file_exists: string -> bool = "sys_file_exists"

(** Remove the given file name from the file system. *)
external remove: string -> unit = "sys_remove"

(** Rename a file. The first argument is the old name and the
   second is the new name. *)
external rename: string -> string -> unit = "sys_rename"

(** Return the value associated to a variable in the process
   environment. Raise [Not_found] if the variable is unbound. *)
external getenv: string -> string = "sys_getenv"

(** Execute the given shell command and return its exit code. *)
external command: string -> int = "sys_system_command"

(** Return the processor time, in seconds, used by the program
   since the beginning of execution. *)
external time: unit -> float = "sys_time"

(** Change the current working directory of the process. *)
external chdir: string -> unit = "sys_chdir"

(** Return the current working directory of the process. *)
external getcwd: unit -> string = "sys_getcwd"

(** This reference is initially set to [false] in standalone
   programs and to [true] if the code is being executed under
   the interactive toplevel system [ocaml]. *)
val interactive: bool ref

(** Operating system currently executing the Caml program.
   One of ["Unix"], ["Win32"], ["Cygwin"] or ["MacOS"]. *)
val os_type: string

(** Size of one word on the machine currently executing the Caml
   program, in bits: 32 or 64. *)
val word_size: int

(** Maximum length of a string. *)
val max_string_length: int

(** Maximum length of an array. *)
val max_array_length: int


(** {2 Signal handling} *)


(** What to do when receiving a signal:
   - [Signal_default]: take the default behavior
     (usually: abort the program)
   - [Signal_ignore]: ignore the signal
   - [Signal_handle f]: call function [f], giving it the signal
   number as argument. *)
type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)


(** Set the behavior of the system on receipt of a given signal.
   The first argument is the signal number.  Return the behavior
   previously associated with the signal. *)
external signal: int -> signal_behavior -> signal_behavior
      = "install_signal_handler"

(** Same as {!Sys.signal} but return value is ignored. *)
val set_signal: int -> signal_behavior -> unit


(** {3 Signal numbers for the standard POSIX signals.} *) 

(** Abnormal termination *)
val sigabrt: int   
(** Timeout *)
val sigalrm: int
(** Arithmetic exception *)
val sigfpe: int    
(** Hangup on controlling terminal *)
val sighup: int    
(** Invalid hardware instruction *)
val sigill: int    
(** Interactive interrupt (ctrl-C) *)
val sigint: int    
(** Termination (cannot be ignored) *)
val sigkill: int   
(** Broken pipe *)
val sigpipe: int   
(** Interactive termination *)
val sigquit: int   
(** Invalid memory reference *)
val sigsegv: int   
(** Termination *)
val sigterm: int   
(** Application-defined signal 1 *)
val sigusr1: int   
(** Application-defined signal 2 *)
val sigusr2: int   
(** Child process terminated *)
val sigchld: int   
(** Continue *)
val sigcont: int   
(** Stop *)
val sigstop: int   
(** Interactive stop *)
val sigtstp: int   
(** Terminal read from background process *)
val sigttin: int   
(** Terminal write from background process *)
val sigttou: int   
(** Timeout in virtual time *)
val sigvtalrm: int 
(** Profiling interrupt *)
val sigprof: int   


(** Exception raised on interactive interrupt if {!Sys.catch_break}
   is on. *)
exception Break


(** [catch_break] governs whether interactive interrupt (ctrl-C)
   terminates the program or raises the [Break] exception. 
   Call [catch_break true] to enable raising [Break],
   and [catch_break false] to let the system
   terminate the program on user interrupt. *)
val catch_break: bool -> unit
