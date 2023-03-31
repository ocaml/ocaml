
let rec bidule acc n =
  if n = 0 then
    Gc.finalise
      (fun str ->
        Printf.printf "Finalizer: str of size %d\n" (String.length str))
      acc
  else bidule (acc ^ ":" ^ string_of_int n) (n - 1)

let () =
  for _ = 1 to 1000000 do
    bidule "" 500 (* Gc.print_stat stdout *)
  done;
  print_endline "Somehow caml terminated"
