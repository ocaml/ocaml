(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

let log s =
  Printf.printf "%s\n%!" s

let mutex_lock_must_fail m =
  try
    Mutex.lock m; log "Should have failed!"
  with Sys_error _ ->
    log "Error reported"

let mutex_unlock_must_fail m =
  try
    Mutex.unlock m; log "Should have failed!"
  with Sys_error _ ->
    log "Error reported"

let mutex_deadlock () =
  let m = Mutex.create() in
  log "Acquiring mutex";
  Mutex.lock m;
  log "Acquiring mutex again";
  mutex_lock_must_fail m;
  log "Releasing mutex";
  Mutex.unlock m;
  let f () =
    log "Acquiring mutex from another thread";
    Mutex.lock m;
    log "Success";
    Mutex.unlock m in
  Thread.join (Thread.create f ())

let mutex_unlock_twice () =
  let m = Mutex.create() in
  log "Acquiring mutex";
  Mutex.lock m;
  log "Releasing mutex";
  Mutex.unlock m;
  log "Releasing mutex again";
  mutex_unlock_must_fail m;
  log "Releasing mutex one more time";
  mutex_unlock_must_fail m

let mutex_unlock_other_thread () =
  let m = Mutex.create() in
  log "Acquiring mutex";
  Mutex.lock m;
  let f () =
    log "Releasing mutex from another thread";
    mutex_unlock_must_fail m;
    log "Releasing mutex from another thread (again)";
    mutex_unlock_must_fail m in
  Thread.join (Thread.create f ())

let _ =
  log "---- Self deadlock";
  mutex_deadlock();
  log "---- Unlock twice";
  mutex_unlock_twice();
  log "---- Unlock in other thread";
  mutex_unlock_other_thread()
