(* System interface *)

val argv: string array
external file_exists: string -> bool = "sys_file_exists"
external remove: string -> unit = "sys_remove"
external getenv: string -> string = "sys_getenv"
external command: string -> int = "sys_system_command"
external chdir: string -> unit = "sys_chdir"

type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)

external signal: int -> signal_behavior -> unit = "install_signal_handler"

val sigabrt: int
val sigalrm: int
val sigfpe: int
val sighup: int
val sigill: int
val sigint: int
val sigkill: int
val sigpipe: int
val sigquit: int
val sigsegv: int
val sigterm: int
val sigusr1: int
val sigusr2: int
val sigchld: int
val sigcont: int
val sigstop: int
val sigtstp: int
val sigttin: int
val sigttou: int

exception Break

val catch_break: bool -> unit
