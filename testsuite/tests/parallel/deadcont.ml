(* TEST
*)

(*
  Test handling of continuations created by a domain that has since terminated.
  Bug report and testcase by Ziga Luksic, see:
    https://github.com/ocamllabs/ocaml-multicore/issues/175
 *)

open Effect
open Effect.Deep

type _ t += Poke : unit t

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
  match_with f ()
  { retc = (fun () -> Done);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
          match e with
          | Poke -> Some (fun (k : (a, _) continuation) ->
              Poking (fun () ->
                print "...";
                ignore (continue k ());
                print "success\n";
                Done))
          | _ -> None }

(* Re-runs the poker that happened inside a domain. *)
let rerunner = function
  | Poking f -> f () (*re-runs the function*)
  | Done -> Done

(* Test. *)
let test n =
  (* Messy handler wrapping. *)
  let inner () = domain_handler (fun () -> poke n) in
  rerunner (Domain.join (Domain.spawn inner))

let _ = test 100 |> ignore; print_endline "done"
