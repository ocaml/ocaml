let () =
  let rec loop failed i =
    if i > 5 then begin
      print_endline "Too many failures.";
      exit 1
    end else
    match Dynlink.loadfile "toto.cmxs" with
    | _ ->
      print_endline "OK";
      exit 0
    | exception exn ->
      if not failed then print_endline "Dynlink.loadfile failed. Retrying.";
      Unix.sleep 1;
      loop true (i + 1)
  in
  loop false 0
