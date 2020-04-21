(* TEST *)

let try_float_of_string str =
  try
    print_float (float_of_string str);
    print_newline ()
  with exn ->
    print_endline (Printexc.to_string exn)
;;

let () =
  try_float_of_string "0x1A";
  try_float_of_string "0x1Ap3";
  try_float_of_string "0x";
  try_float_of_string "0x.";
  try_float_of_string "0xp0";
  try_float_of_string "0x.p0";

  (* MPR#7690 *)
  try_float_of_string "0x1.0p-2147483648";
  try_float_of_string "0x123456789ABCDEF0p2147483647";
  try_float_of_string "0x1p2147483648";

  (* Allow underscore almost everywhere *)
  try_float_of_string "_0x1.1";
  try_float_of_string "0_x1.1";
  try_float_of_string "0x_1.1";
  try_float_of_string "0x1_.1";
  try_float_of_string "0x1._";
  try_float_of_string "0x1.1_";
  try_float_of_string "0x1_p1";
  try_float_of_string "0x1p_1";
  try_float_of_string "0x1p1_";
  try_float_of_string "0x1p-1_1";
  try_float_of_string "0x1p-1_";
  try_float_of_string "0x1p+1_1";
  try_float_of_string "0x1p+1_";

  try_float_of_string "0x1p1\000suffix"

let () =
  (* check that the compiler can also parse tokens *)
  let _ = 0x1A in
  let _ = 0x1Ap3 in

  let _ = 0x1.0p-2147483648 in
  let _ = 0x123456789ABCDEF0p2147483647 in
  let _ = 0x1p2147483648 in

  let _ = 0x1_._1p1_1 in
  let _ = 0x1_._1p1_ in
  let _ = 0x1_._1p-1_1 in
  let _ = 0x1_._1p-1_ in
  let _ = 0x1_._1p+1_1 in
  let _ = 0x1_._1p+1_ in
  ()
