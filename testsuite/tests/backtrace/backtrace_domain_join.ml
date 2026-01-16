(* TEST_BELOW
(* Blank lines added here to preserve locations. *)

*)

(* A test for backtraces on [Domain.join] *)

let rec bar n =
  if n = 0 then failwith "bar raised"
  else foo (n-1) + 1

and foo n =
  if n = 0 then failwith "foo raised"
  else bar (n-1) + 1

let f () = foo 10

let _ =
  match Domain.spawn f |> Domain.join with
  | v -> ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.print_raw_backtrace stdout bt

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
*)
