(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

let t =
  let t = Thread.create (fun _ -> ())() in
  Thread.join t

let () =
  Thread.exit ()
