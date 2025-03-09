(* TEST *)

open Printf
open Effect
open Effect.Deep

(* Some examples from Matija Pretnar's MFPS 2015 tutorial,
   "An introduction to algebraic effects and handlers". *)

type _ eff += Print : string -> unit eff

let print s = perform (Print s)

let abc () = print "a"; print "b"; print "c"

let output f =
  match f () with
  | () -> print_newline()
  | effect Print s, k ->  print_string s; continue k ()

let reverse f =
  match f () with
  | () -> ()
  | effect Print s, k ->  continue k (); print s

let collect f =
  match f () with
  | () -> ""
  | effect Print s, k -> s ^ continue k ()

let _ =
  output abc;
  output (fun () -> reverse abc);
  printf "%s\n" (collect abc);
  printf "%s\n" (collect (fun () -> reverse abc))

type _ eff += Get : int eff
            | Set : int -> unit eff

let get () = perform Get
let set n  = perform (Set n)
let incr () = set (get () + 1)

let run_state (f : unit -> 'a) : int -> 'a * int =
  match f () with
  | v -> (fun s -> (v, s))
  | effect Get, k -> (fun s -> continue k s s)
  | effect Set n, k -> (fun _ -> continue k () n)

let _ =
  run_state
    (fun () ->
      printf "%d " (get()); incr();
      printf "%d " (get()); incr();
      printf "%d\n" (get()))
    10

exception Abort

let transaction (f : unit -> unit) : unit =
  begin match f () with
  | () -> (fun s -> set s)
  | effect Get, k -> (fun s -> continue k s s)
  | effect Set n, k -> (fun _ -> continue k () n)
  | exception Abort -> (fun _ -> ())
  end (get ())

let _ =
  run_state
    (fun () ->
      printf "%d " (get());
      transaction (fun () -> incr(); incr());
      printf "%d " (get());
      transaction (fun () -> incr(); raise Abort);
      printf "%d " (get());
      transaction (fun () -> incr(); incr());
      printf "%d\n" (get()))
    10
