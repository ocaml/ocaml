(* TEST
 set OCAMLRUNPARAM = "s32";
 native;
*)

(* This test verifies that stack frames are correct when raising unhandled
   effect exceptions. This used not to be the case on some platforms,
   causing assertions when the garbage collector would fire.

   By using a very small initial heap (s32), this test guarantees the GC
   will get triggered.

   Refer to https://github.com/ocaml/ocaml/issues/12486 for more
   information.
*)

open Effect

type _ t += Yield : unit t

let rec burn l =
  if List.hd l > 12 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let foo l =
  burn l;
  perform Yield

let bar i = foo [i]

let () =
  for _ = 1 to 10_000 do
    try bar 8
    with Unhandled _ -> ()
  done
