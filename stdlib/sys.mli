(* System interface *)

type open_flag =
    Open_rdonly | Open_wronly | Open_rdwr
  | Open_append | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text

val argv: string array
val remove: string -> unit = "sys_remove"
val getenv: string -> string = "sys_getenv"
val open_desc: string -> open_flag list -> int -> int = "sys_open"
val close_desc: int -> unit = "sys_close"
val command: string -> int = "sys_system_command"
val chdir: string -> unit = "sys_chdir"

type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)

val signal: int -> signal_behavior -> unit = "install_signal_handler"

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
