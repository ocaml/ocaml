(* TEST
 ocamlopt_flags += " -O3 ";
*)

(* In this test we force a lazy from two concurrent domains without
   synchronization. This leads to unspecified behavior but still
   should not crash. Currently, the implementation raises Undefined,
   and that's what we test here.
*)


let f count =
  let _n = (Domain.self ():> int) in
  let r = ref 0 in
  for i = 1 to count do
    incr r;
  done;
  !r

let main () =
  let l = lazy (f 1_000_000_000) in
  let d1 =
    Domain.spawn (fun () ->
        let _n = (Domain.self ():> int) in
        Lazy.force l)
  in
  let n2 = try Some (Lazy.force l) with Lazy.Undefined -> None in
  let n1 = Domain.join d1 in
  (n1, n2)

let _ =
  match main () with
  | (n1, Some n2) -> Printf.printf "n1=%d n2=%d\n" n1 n2
  | (_, None) -> print_endline "Undefined"
  | exception Lazy.Undefined -> print_endline "Undefined"
