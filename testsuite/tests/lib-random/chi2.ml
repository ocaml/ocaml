(* TEST
*)

(* A basic chi-square test to detect simple errors in the Random module. *)

(* Accumulate [n] samples from function [f] and check the chi-square.
   Only the low 8 bits of the result of [f] are sampled. *)

let chisquare n f =
  let r = 256 in
  let freq = Array.make r 0 in
  for i = 0 to n - 1 do
    let t = f () land 0xFF in
    freq.(t) <- freq.(t) + 1
  done;
  let expected = float n /. float r in
  let t =
    Array.fold_left
      (fun s x -> let d = float x -. expected in d *. d +. s)
      0.0 freq in
  let chi2 = t /. expected in
  let degfree = float r -. 1.0 in
  (* The degree of freedom is high, so we approximate as a normal
     distribution with mean equal to degfree and variance 2 * degfree.
     Four sigmas correspond to a 99.9968% confidence interval.
     (Without the approximation, the confidence interval seems to be 99.986%.)
  *)
  chi2 <= degfree +. 4.0 *. sqrt (2.0 *. degfree)

let test name f =
  if not (chisquare 100_000 f)
  then Printf.printf "%s: suspicious result\n%!" name

let _ =
  test "Random.bits (bits 0-7)"
       Random.bits;
  test "Random.bits (bits 12-19)"
       (fun () -> Random.bits() lsr 12);
  test "Random.bits (bits 22-29)"
       (fun () -> Random.bits() lsr 22);
  test "Random.int 2^26 (bits 0-7)"
       (fun () -> Random.int (1 lsl 26));
  test "Random.int 2^26 (bits 18-25)"
       (fun () -> Random.int (1 lsl 26) lsr 18);
  test "Random.int (256 * p) / p"
       (fun () -> Random.int (256 * 853187) / 853187);
  test "Random.float 1.0 (first 8 bits)"
       (fun () -> int_of_float (Random.float 1.0 *. 256.0));
  test "Random.float 1.0 (next 8 bits)"
       (fun () -> int_of_float (Random.float 1.0 *. 65536.0));
  test "Random.bits32 (bits 0-7)"
       (fun () -> Int32.to_int (Random.bits32()));
  test "Random.bits32 (bits 20-27)"
       (fun () -> Int32.(to_int (shift_right (Random.bits32()) 20)));
  test "Random.int32 2^30 (bits 0-7)"
       (fun () -> Int32.to_int (Random.int32 0x40000000l));
  test "Random.int32 2^30 (bits 20-27)"
       (fun () -> Int32.(to_int (shift_right (Random.int32 0x40000000l) 20)));
  test "Random.int32 (256 * p) / p"
       (let p = 7048673l in
        fun () -> Int32.(to_int (div (Random.int32 (mul 256l p)) p)));
  test "Random.bits64 (bits 0-7)"
       (fun () -> Int64.to_int (Random.bits64()));
  test "Random.bits64 (bits 30-37)"
       (fun () -> Int64.(to_int (shift_right (Random.bits64()) 30)));
  test "Random.bits64 (bits 52-59)"
       (fun () -> Int64.(to_int (shift_right (Random.bits64()) 52)));
  test "Random.int64 2^60 (bits 0-7)"
       (fun () -> Int64.to_int (Random.int64 0x1000000000000000L));
  test "Random.int64 2^60 (bits 30-37)"
       (fun () -> Int64.(to_int (shift_right (Random.int64 0x1000000000000000L)
                                             30)));
  test "Random.int64 2^60 (bits 52-59)"
       (fun () -> Int64.(to_int (shift_right (Random.int64 0x1000000000000000L)
                                             52)));
  test "Random.int64 (256 * p) / p"
       (let p = 16430454264262693L in
        fun () -> Int64.(to_int (div (Random.int64 (mul 256L p)) p)));
  if Sys.int_size >= 32 then begin
    test "Random.full_int 2^30 (bits 0-7)"
         (fun () -> Random.full_int (1 lsl 30));
    test "Random.full_int 2^30 (bits 22-29)"
         (fun () -> Random.full_int (1 lsl 30) lsr 22);
    test "Random.full_int (256 * p) / p"
         (let p = 7992689 in
          fun () -> Random.full_int (256 * p) / p)
  end;
  if Sys.int_size >= 63 then begin
    test "Random.full_int 2^60 (bits 0-7)"
         (fun () -> Random.full_int (1 lsl 60));
    test "Random.full_int 2^60 (bits 30-37)"
         (fun () -> Random.full_int (1 lsl 60) lsr 30);
    test "Random.full_int 2^60 (bits 52-59)"
         (fun () -> Random.full_int (1 lsl 60) lsr 52);
    test "Random.full_int (256 * P) / P"
         (let p = Int64.to_int 17766642568158577L in
          fun () -> Random.full_int (256 * p) / p)
  end
