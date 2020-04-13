(* TEST
* hasunix
include unix
** bytecode
** native
*)

(*
  Test handling of continuations created by a domain that has since terminated.
  Bug report and testcase by Žiga Lukšič, see:
    https://github.com/ocamllabs/ocaml-multicore/issues/175
 *)

effect Poke : unit

type result = Done | Poking of (unit -> result)

(* Debug help. *)
let print s = print_string s; Format.pp_print_flush Format.std_formatter ()

(* Just poke the handler n times. *)
let rec poke = function
  | 0 -> ()
  | n -> perform Poke; poke (n-1)

(* The handler inside the domain, that captures the continuation whenever
    it gets poked. *)
let domain_handler f =
  match f () with
  | effect Poke k -> Poking (fun () -> print "..."; continue k () |> ignore; print "success\n"; Done)
  | () -> Done

(* Re-runs the poker that happened inside a domain. *)
let rerunner = function
  | Poking f ->  f () (*re-runs the function*)
  | Done -> Done

(* Test. *)
let test n =
  (* Messy handler wrapping. *)
  let inner () = domain_handler (fun () -> poke n) in
  rerunner (Domain.join (Domain.spawn inner))

let _ = test 100 |> ignore; print_endline "done"
