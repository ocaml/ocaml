(* TEST
 arch64;
 include testing;
*)


let test msg f arg r =
  if f arg <> r then begin
      Format.eprintf "Error:%s@." msg
    end

(* Overflow in constant tests  *)
let f = function
  | -4611686018427387904 -> 0
  | -4611686018427387903 -> 1
  | -4611686018427387902 -> 2

  | -4611686018427387609 -> 11
  | -4611686018427387608 -> 12

  | -4611686018427387509 -> 21
  | -4611686018427387508 -> 22


  | -4611686018427387009 -> 31
  | -4611686018427387008 -> 32


  | 65529 -> 101
  | 65530 -> 102
  | _ -> -1;;

test "min_int_overflow" f (-4611686018427387904) 0;
test "min_int_overflow" f (-4611686018427387903) 1;
test "min_int_overflow" f (-4611686018427387609) 11;
test "min_int_overflow" f (-4611686018427387508) 22;
test "min_int_overflow" f (-4611686018427387008) 32;
test "min_int_overflow" f 0 (-1);
test "min_int_overflow" f 65529 101;;
