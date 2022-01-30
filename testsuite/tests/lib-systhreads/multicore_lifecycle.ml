(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)

let _ =
  let t = ref (Thread.self ()) in
  let d = Domain.spawn begin fun () ->
     let thread_func () = Unix.sleep 5 in
     let tt = Thread.create thread_func () in
     t := tt;
    ()
   end
  in
  Domain.join d;
  Thread.join (!t);
  Domain.join @@ Domain.spawn (fun () -> print_endline "ok")
