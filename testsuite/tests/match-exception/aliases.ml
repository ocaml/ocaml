(*
  Test that aliases are accepted.
*)

let test_multiple_handlers =
  let nat_pred = function
    | 0 -> failwith "zero"
    | n -> pred n in
  let print_nat_pred n =
    match nat_pred n with
    | m -> print_int m
    | exception Failure _ as f -> raise f
    | exception Not_found -> () in
  let safe_print_nat_pred n =
    try
      print_nat_pred n
    with e ->
      print_string (Printexc.to_string e) in
  for i = 3 downto 0 do
    safe_print_nat_pred i;
    print_string " "
  done
;;
