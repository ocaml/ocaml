(* TEST
include config
include testing
binary_modules = "config build_path_prefix_map misc identifiable numbers"
* bytecode
*)

let print_int i =
  if i = max_int then
    "max_int"
  else if i = min_int then
    "min_int"
  else
    Int.to_string i

let test_no_overflow_add a b =
  Printf.printf "Misc.no_overflow_add %s %s = %b\n"
    (print_int a)
    (print_int b)
    (Misc.no_overflow_add a b)

let test_no_overflow_sub a b =
  Printf.printf "Misc.no_overflow_sub %s %s = %b\n"
    (print_int a)
    (print_int b)
    (Misc.no_overflow_sub a b)

let test_no_overflow_mul a b =
  Printf.printf "Misc.no_overflow_mul %s %s = %b\n"
    (print_int a)
    (print_int b)
    (Misc.no_overflow_mul a b)

let cartesian_product l1 l2 =
  List.concat
    (l1 |> List.map (fun v1 ->
     l2 |> List.map (fun v2 ->
     (v1, v2))))

let () =
  let ints = [ 0; 1; 2; max_int; -1; -2; min_int ] in
  let int_pairs = cartesian_product ints ints in
  int_pairs |> List.iter (fun (a, b) -> test_no_overflow_add a b);
  int_pairs |> List.iter (fun (a, b) -> test_no_overflow_sub a b);
  int_pairs |> List.iter (fun (a, b) -> test_no_overflow_mul a b)
