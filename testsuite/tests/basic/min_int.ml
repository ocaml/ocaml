let min_int = -4611686018427387904
let () = match min_int with
  | -4611686018427387904 as i ->
    assert (string_of_int i = "-4611686018427387904");
    print_endline "OK"
  | _ -> assert false
