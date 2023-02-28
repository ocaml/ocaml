(* TEST
include config
include testing
binary_modules = "config build_path_prefix_map misc"
* bytecode
*)

let check_and_count_calls n =
  let calls = ref 0 in
  let res = Misc.find_first_mono (fun i -> incr calls; i >= n) in
  assert (res = n);
  !calls

let report i =
  let calls = check_and_count_calls i in
  Printf.printf "%d: %d calls\n%!" i calls

let () =
  print_endline "Testing small values";
  let small_tests = List.init 20 (fun i -> i) in
  List.iter report small_tests;
  print_newline ()

let () =
  print_endline "Testing around max_int/2";
  (* we do not use [report] to show the test output, as the output
     depends on the value of [max_int], so it is not portable across
     integer sizes. *)
  let median_tests =
    List.map (fun i -> max_int / 2 + i) [-2; -1; 0; 1; 2] in
  List.map check_and_count_calls median_tests |> ignore;
  print_newline ()

let () =
  print_endline "Testing around max_int";
  (* see above for why we do not use [report] *)
  let max_int_tests =
    List.map (fun i -> max_int + i) [-4; -3; -2; -1; 0] in
  List.map check_and_count_calls max_int_tests |> ignore;
  print_newline ()

let () =
  let n = 100 in
  Printf.printf
    "Showing predicate calls for find_first_mono to find n=%d\n%!" n;
  let res =
    Misc.find_first_mono (fun i -> Printf.printf "call on %d\n%!" i; i >= n) in
  Printf.printf "result: %d\n%!" res;
  print_newline ()

let () =
  print_endline "Test constantly-false predicates";
  print_endline "Constantly-false predicates are outside the spec,\n\
    but we ask them to return [max_int] rather than loop or crash.";
  let res = Misc.find_first_mono (fun _ -> false) in
  assert (res = max_int);
  print_newline ()

let () =
  print_endline "Test non-monotonous predicates";
  print_endline "Non-monotonous predicates are outside the spec,\n\
    but we ask them to find a satisfying value rather than loop or crash\n\
    as long as they are asymptotically true.";
  (* We generate predicates that are true above 1000 but
     are sometimes-true before that. *)
  for n = 1 to 499 do
    let pred i =
      i >= 1000 || (i > n && i mod n = 0)
    in
    let res = Misc.find_first_mono pred in
    assert (pred res)
  done;
  print_newline ()
