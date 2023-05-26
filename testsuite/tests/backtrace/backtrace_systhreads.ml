(* TEST
flags = "-g"
ocamlrunparam += ",b=1"
* hassysthreads
include systhreads
** bytecode
** native
*)

let throw_exn msg = failwith msg [@@inline never]

let thread_func delay =
  Thread.yield ();
  try throw_exn (string_of_int delay) with
  | exn ->
     Thread.delay (float_of_int delay);
     Gc.minor ();
     raise exn

let thread_backtrace (cond, mut) =
  Thread.yield ();
  try throw_exn "backtrace" with
  | exn ->
     Mutex.lock mut;
     Condition.wait cond mut; Mutex.unlock mut;
     raise exn

let () =
  Random.self_init ();
  let mut = Mutex.create () in
  let cond = Condition.create () in
  let backtrace_thread = Thread.create thread_backtrace (cond, mut) in
  let threads =
    List.init 4 begin fun i ->
      Thread.create thread_func i
      end
  in
  List.iter Thread.join threads;
  Condition.signal cond;
  Thread.join backtrace_thread
