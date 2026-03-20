(* TEST
*)

let num_domains = 20

let go () =
  let n = 50_000 in
  let c = Array.make n None in
  for i = 0 to n-1 do
    c.(i) <- Some (i, i)
  done;
  Gc.compact ()

let () =
  Array.init num_domains (fun _ -> Domain.spawn go)
  |> Array.iter Domain.join
