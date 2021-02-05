(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)


let n = 42
let num_domains = try int_of_string Sys.argv.(1) with _ -> 4

let rec fib n =
  if (n < 2) then n
  else fib (n - 1) + fib (n - 2)

let th_create () =
  let t = Thread.create fib in
  Thread.join (t n)

let _ =
  let domains = Array.init num_domains (fun _ -> Domain.spawn(th_create)) in
  Array.iter Domain.join domains;
  print_endline "done"
