(* TEST
*)

(* when run with the bytecode debug runtime, this test
   used to trigger a bug where the constant [13]
   remained unpromoted *)

let rec burn l =
  if List.hd l > 14 then ()
  else burn (l @ l |> List.map (fun x -> x + 1))

let () =
  ignore (Domain.spawn (fun () -> burn [13]));
  burn [0];
  Printf.printf "all done\n%!"
