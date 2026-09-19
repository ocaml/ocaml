(* TEST *)

(* Shift pairs that native code turns into a rotate instruction. *)

let opaque = Sys.opaque_identity

let check name got want =
  if got <> want then
    failwith (Printf.sprintf "rotate %s: got %Lx, expected %Lx" name got want)

let () =
  let open Int64 in
  let x = opaque 0x0123456789ABCDEFL in
  let y = opaque 0x8000000000000001L in
  check "x 1"  (logor (shift_left x 1) (shift_right_logical x 63))
    0x02468ACF13579BDEL;
  check "x 13" (logor (shift_left x 13) (shift_right_logical x 51))
    0x68ACF13579BDE024L;
  check "x 32" (logor (shift_left x 32) (shift_right_logical x 32))
    0x89ABCDEF01234567L;
  check "x 63" (logor (shift_left x 63) (shift_right_logical x 1))
    0x8091A2B3C4D5E6F7L;
  check "y 1"  (logor (shift_left y 1) (shift_right_logical y 63))
    0x0000000000000003L;
  check "y 13" (logor (shift_left y 13) (shift_right_logical y 51))
    0x0000000000003000L;
  check "y 32" (logor (shift_left y 32) (shift_right_logical y 32))
    0x0000000180000000L;
  check "y 63" (logor (shift_left y 63) (shift_right_logical y 1))
    0xC000000000000000L;

  (* The halves never overlap, so xor and either operand order rotate too. *)
  check "xor"     (logxor (shift_left x 13) (shift_right_logical x 51))
    0x68ACF13579BDE024L;
  check "swapped" (logor (shift_right_logical x 51) (shift_left x 13))
    0x68ACF13579BDE024L;

  let n = opaque (to_nativeint 0x0123456789ABCDEFL) in
  if Sys.word_size = 64 then
    check "nativeint"
      (of_nativeint
         Nativeint.(logor (shift_left n 13) (shift_right_logical n 51)))
      0x68ACF13579BDE024L;

  (* These are not rotations and must keep their plain meaning. *)
  check "amounts add to 63"
    (logor (shift_left x 13) (shift_right_logical x 50))
    0x68ACF13579BDE048L;
  check "two values"
    (logor (shift_left x 13) (shift_right_logical y 51))
    0x68ACF13579BDF000L;
  check "masked low half"
    (logor (shift_left x 13)
       (shift_right_logical (logand x 0xFFFFFFFFL) 19))
    0x68ACF13579BDF135L
