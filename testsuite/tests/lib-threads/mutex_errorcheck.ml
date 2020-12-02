(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

let m = Mutex.create ()

let on = ref true

let create_finalised () =
  let r = ref 0 in
  Gc.finalise (fun _ -> if !on then begin
    Mutex.lock m ;
    print_endline "finalised!" ;
    Mutex.unlock m
  end) r;
  r

let () =
  try
    while true do
      Mutex.lock m ;
      let x = create_finalised () in
      Mutex.unlock m ;
      ignore (Sys.opaque_identity x)
    done
  with
    (* BEGIN ATOMIC *)
    Sys_error _ (* e.g. "Mutex.lock: Resource deadlock avoided" *) -> (
      on := false
      (* END ATOMIC *)
      (* success *)
    )
