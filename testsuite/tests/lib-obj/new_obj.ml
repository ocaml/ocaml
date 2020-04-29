(* TEST
*)

let _ =

  begin match Obj.new_block 255 1 with
  | v -> failwith "Expected failure for custom block"
  | exception (Invalid_argument _) -> ()
  end;

  begin match Obj.new_block 252 0 with
  | v -> failwith "Expected failure for zero length string block"
  | exception (Invalid_argument _) -> ()
  end;

  print_endline "OK"
