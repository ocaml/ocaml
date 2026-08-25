(* TEST *)

(* This test reliably exercises the bugs from Jan Midtgaard's repro case in #14349.

   On a developer machine, out of 10 on OCaml 5.4, it was observed to:
   - terminate in about 0.3s when it works correctly
   - segfault 3 times
   - loop indefinitely 2 times
*)
let go () =
  for _ = 1 to 256 do
    let stack = ref [] in
    for _ = 1 to 256 do
      stack := Weak.create 64 :: !stack;
    done;
    ignore (Sys.opaque_identity !stack)
  done

let () =
  for _ = 1 to 4 do
    Array.init 4 (fun _ -> Domain.spawn go)
    |> Array.iter Domain.join
  done
