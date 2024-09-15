(* TEST *)

let comp = String.init 200 (fun i -> Char.chr (Char.code '0' + i mod 10))

let () =
  for n = 1 to 10 do
    Sys.mkdir (String.concat "/" (List.init n (Fun.const comp))) 0o777
  done;
  print_endline "OK"
