type t

external dec_initialize : unit -> unit = "caml_dec_initialize"

external dec_new : (unit -> unit) -> t = "caml_dec_new"

external dec_join : t -> unit = "caml_dec_join"

external self : unit -> t = "caml_dec_self" [@@noalloc]
external id : t -> int = "caml_dec_id" [@@noalloc]

external exit : unit -> unit = "caml_dec_exit"

external dec_yield : unit -> unit = "caml_dec_yield"

external dec_uncaught_exception : exn -> unit =
            "caml_dec_uncaught_exception"

let create fn arg =
  dec_new
    (fun () ->
      try
        fn arg; ()
      with exn ->
             flush stdout; flush stderr;
             dec_uncaught_exception exn)

let join = dec_join
let yield = dec_yield
let preempt signal = yield ()
let delay = Unix.sleepf

let () = dec_initialize ()

let kill th = invalid_arg "Thread.kill: not implemented"

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

external sigmask : Unix.sigprocmask_command -> int list -> int list
   = "caml_dec_sigmask"
external wait_signal : int list -> int = "caml_wait_signal"
