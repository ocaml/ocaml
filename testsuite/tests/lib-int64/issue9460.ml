(* TEST
*)

(* See https://github.com/ocaml/ocaml/issues/9460
   This test comes from Richard Jones
   at
     https://github.com/libguestfs/libnbd/blob/0475bfe04a527051c0a37af59a733c4c8554e427/ocaml/tests/test_400_pread.ml#L21-L36
*)
let test_result =
  let b = Bytes.create 16 in
  for i = 0 to 16/8-1 do
    let i64 = ref (Int64.of_int (i*8)) in
    for j = 0 to 7 do
      let c = Int64.shift_right_logical !i64 56 in
      let c = Int64.to_int c in
      let c = Char.chr c in
      Bytes.unsafe_set b (i*8+j) c;
      i64 := Int64.shift_left !i64 8
    done
  done;
  (Bytes.to_string b) ;;

let expected =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008"

let () =
  assert (test_result = expected)

(* Reproduction case by Jeremy Yallop in
   https://github.com/ocaml/ocaml/pull/9463#issuecomment-615831765
*)
let () =
  let x = ref Int64.max_int in
  assert (!x = Int64.max_int)

let () =
  print_endline "OK"
