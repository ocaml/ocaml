(* TEST *)

let go () =
  for _ = 1 to 256 do
    let stack = ref [] in
    for _ = 1 to 256 do
      stack := Ephemeron.K1.make (1, 2) (3, 4) :: !stack;
    done;
    ignore (Sys.opaque_identity !stack)
  done

let () =
  for _ = 1 to 10 do
    Array.init 4 (fun _ -> Domain.spawn go)
    |> Array.iter Domain.join
  done
