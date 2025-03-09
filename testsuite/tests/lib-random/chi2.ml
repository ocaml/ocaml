(* TEST *)

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

(* Division of [x] by [y] where [x] is interpreted as an unsigned integer.
 * This code assumes [y >= 0]. *)
let udiv x y =
  if x >= 0 then x / y
  else
    let x' = x + min_int in
    let q = (x' / y) - (min_int / y)
    and r = (x' mod y) - (min_int mod y) in
    if r < y then q else q + 1

let _ =

  (* [bits] *)
  test "Random.bits (bits 0-7)"
       Random.bits;
  test "Random.bits (bits 12-19)"
       (fun () -> Random.bits() lsr 12);
  test "Random.bits (bits 22-29)"
       (fun () -> Random.bits() lsr 22);

  (* [int] *)
  test "Random.int 2^26 (bits 0-7)"
       (fun () -> Random.int (1 lsl 26));
  test "Random.int 2^26 (bits 18-25)"
       (fun () -> Random.int (1 lsl 26) lsr 18);
  test "Random.int (256 * p) / p"
       (fun () -> Random.int (256 * 853187) / 853187);

  (* [float] *)
  test "Random.float 1.0 (first 8 bits)"
       (fun () -> int_of_float (Random.float 1.0 *. 256.0));
  test "Random.float 1.0 (next 8 bits)"
       (fun () -> int_of_float (Random.float 1.0 *. 65536.0));

  (* [bits32] *)
  test "Random.bits32 (bits 0-7)"
       (fun () -> Int32.to_int (Random.bits32()));
  test "Random.bits32 (bits 20-27)"
       (fun () -> Int32.(to_int (shift_right (Random.bits32()) 20)));

  (* [int32] *)
  test "Random.int32 2^30 (bits 0-7)"
       (fun () -> Int32.to_int (Random.int32 0x40000000l));
  test "Random.int32 2^30 (bits 20-27)"
       (fun () -> Int32.(to_int (shift_right (Random.int32 0x40000000l) 20)));
  test "Random.int32 (256 * p) / p"
       (let p = 7048673l in
        fun () -> Int32.(to_int (div (Random.int32 (mul 256l p)) p)));

  (* [bits64] *)
  test "Random.bits64 (bits 0-7)"
       (fun () -> Int64.to_int (Random.bits64()));
  test "Random.bits64 (bits 30-37)"
       (fun () -> Int64.(to_int (shift_right (Random.bits64()) 30)));
  test "Random.bits64 (bits 52-59)"
       (fun () -> Int64.(to_int (shift_right (Random.bits64()) 52)));

  (* [int64] *)
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

  (* [full_int] *)
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
  end;

  (* [int_in_range] *)

  let min_ = -214748364 in
  let max_ = min_ + 0x1FFF_FFFF in
  test "Random.int_in_range, range of length 2^29 (bits 0-7)"
       (fun () -> Random.int_in_range ~min:min_ ~max:max_ - min_);
  test "Random.int_in_range, range of length 2^29 (bits 21-28)"
       (fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) lsr 21);
  let min_ = -214748364 in
  let max_ = min_ + 0x3FFF_FFFF in
  test "Random.int_in_range, range of length 2^30 (bits 0-7)"
       (fun () -> Random.int_in_range ~min:min_ ~max:max_ - min_);
  test "Random.int_in_range, range of length 2^30 (bits 22-29)"
       (fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) lsr 22);
  let min_int31 = -0x4000_0000 in
  let max_int31 = 0x3FFF_FFFF in
  test "Random.int_in_range, full int31 range (bits 0-7)"
      (fun () -> Random.int_in_range ~min:min_int31 ~max:max_int31);
  test "Random.int_in_range, full int31 range (bits 23-30)"
      (fun () -> (Random.int_in_range ~min:min_int31 ~max:max_int31) lsr 23);
  test "Random.int_in_range, range of length 256*p < 2^30 (bits 0-7)"
      (let p = 2_097_169 in (* prime < 2^22 *)
       let min_ = -214748364 in
       let max_ = min_ + (256 * p) - 1 in
       fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) / p);
  test "Random.int_in_range, range of length 2^30 < 256*p < 2^31 (bits 0-7)"
      (let p = 6_291_469 in (* prime > 2^22 and < 2^23 *)
       let min_ = min_int in
       let max_ = min_ + (256 * p) - 1 in
       fun () -> udiv (Random.int_in_range ~min:min_ ~max:max_ - min_) p);
  if Sys.int_size >= 32 then begin
    let min_int32 = Int64.to_int (-0x8000_0000L) in
    let max_int32 = Int64.to_int 0x7FFF_FFFFL in
    test "Random.int_in_range, full int32 range (bits 0-7)"
        (fun () -> Random.int_in_range ~min:min_int32 ~max:max_int32);
    test "Random.int_in_range, full int32 range (bits 24-31)"
        (fun () -> (Random.int_in_range ~min:min_int32 ~max:max_int32) lsr 24);
    test "Random.int_in_range, range of length 2^31 < 256*p < 2^32 (bits 0-7)"
        (let p = 12_582_917 in (* prime > 2^23 and < 2^24 *)
         let min_ = min_int in
         let max_ = min_ + (256 * p) - 1 in
         fun () -> udiv (Random.int_in_range ~min:min_ ~max:max_ - min_) p);
  end;
  if Sys.int_size >= 63 then begin
    let min_ = Int64.to_int (-1844674407370955197L) in
    let max_ = min_ + Int64.to_int 0x1FFF_FFFF_FFFF_FFFFL in
    test "Random.int_in_range, range of length 2^61 (bits 0-7)"
         (fun () -> Random.int_in_range ~min:min_ ~max:max_ - min_);
    test "Random.int_in_range, range of length 2^61 (bits 30-37)"
         (fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) lsr 30);
    test "Random.int_in_range, range of length 2^61 (bits 53-60)"
         (fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) lsr 53);
    let min_ = Int64.to_int (-1844674407370955197L) in
    let max_ = min_ + Int64.to_int 0x3FFF_FFFF_FFFF_FFFFL in
    test "Random.int_in_range, range of length 2^62 (bits 0-7)"
         (fun () -> Random.int_in_range ~min:min_ ~max:max_ - min_);
    test "Random.int_in_range, range of length 2^62 (bits 30-37)"
         (fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) lsr 30);
    test "Random.int_in_range, range of length 2^62 (bits 54-61)"
         (fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) lsr 54);
    test "Random.int_in_range, full int range (bits 0-7)"
        (fun () -> Random.int_in_range ~min:min_int ~max:max_int);
    test "Random.int_in_range, full int range (bits 30-37)"
        (fun () -> (Random.int_in_range ~min:min_int ~max:max_int) lsr 30);
    test "Random.int_in_range, full int range (bits 55-62)"
        (fun () -> (Random.int_in_range ~min:min_int ~max:max_int) lsr 55);
    test "Random.int_in_range, range of length 2^61 < 256*p < 2^62 (bits 0-7)"
        (let p = Int64.to_int 13510798882111519L in (*prime > 2^53 and < 2^54 *)
         let min_ = min_int in
         let max_ = min_ + (256 * p) - 1 in
         fun () -> (Random.int_in_range ~min:min_ ~max:max_ - min_) / p);
    test "Random.int_in_range, range of length 256*p > 2^62 (bits 0-7)"
        (let p = Int64.to_int 27021597764223071L in (*prime > 2^54 and < 2^55 *)
         let min_ = min_int in
         let max_ = min_ + (256 * p) - 1 in
         fun () -> udiv (Random.int_in_range ~min:min_ ~max:max_ - min_) p);
  end;

  (* [int32_in_range] *)
  let min_ = -429496751l in
  let max_ = Int32.add min_ 0x3FFF_FFFFl in
  test "Random.int32_in_range, range of length 2^30 (bits 0-7)"
       (fun () -> Int32.(to_int
            (sub (Random.int32_in_range ~min:min_ ~max:max_) min_)));
  test "Random.int32_in_range, range of length 2^30 (bits 22-29)"
       (fun () -> Int32.(to_int (shift_right
            (sub (Random.int32_in_range ~min:min_ ~max:max_) min_)
            22)));
  let min_ = -429496751l in
  let max_ = Int32.add min_ 0x7FFF_FFFFl in
  test "Random.int32_in_range, range of length 2^31 (bits 0-7)"
       (fun () -> Int32.(to_int
            (sub (Random.int32_in_range ~min:min_ ~max:max_) min_)));
  test "Random.int32_in_range, range of length 2^31 (bits 23-30)"
       (fun () -> Int32.(to_int (shift_right
            (sub (Random.int32_in_range ~min:min_ ~max:max_) min_)
            23)));
  test "Random.int32_in_range, full int32 range (bits 0-7)"
       (fun () -> Int32.(to_int
            (Random.int32_in_range ~min:min_int ~max:max_int)));
  test "Random.int32_in_range, full int32 range (bits 24-31)"
       (fun () -> Int32.(to_int (shift_right
            (Random.int32_in_range ~min:min_int ~max:max_int)
            24)));
  test "Random.int32_in_range, range of length 256*p < 2^31 (bits 0-7)"
       (let p = 6_291_469l in (* prime < 2^23 *)
        let min_ = -429496751l in
        let max_ = Int32.(pred (add min_ (mul 256l p))) in
        fun () -> Int32.(to_int
            (div (sub (Random.int32_in_range ~min:min_ ~max:max_) min_) p)));
  test "Random.int32_in_range, range of length 256*p > 2^31 (bits 0-7)"
       (let p = 12_582_917l in (* prime > 2^23 and < 2^24 *)
        let min_ = Int32.min_int in
        let max_ = Int32.(pred (add min_ (mul 256l p))) in
        fun () -> Int32.(to_int
            (unsigned_div (sub (Random.int32_in_range ~min:min_ ~max:max_) min_)
               p)));

  (* [int64_in_range] *)
  let min_ = -1844674407370955197L in
  let max_ = Int64.add min_ 0x3FFF_FFFF_FFFF_FFFFL in
  test "Random.int64_in_range, range of length 2^62 (bits 0-7)"
       (fun () -> Int64.(to_int
            (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)));
  test "Random.int64_in_range, range of length 2^62 (bits 30-37)"
       (fun () -> Int64.(to_int (shift_right
            (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)
            30)));
  test "Random.int64_in_range, range of length 2^62 (bits 54-61)"
       (fun () -> Int64.(to_int (shift_right
            (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)
            54)));
  let min_ = -1844674407370955197L in
  let max_ = Int64.add min_ 0x7FFF_FFFF_FFFF_FFFFL in
  test "Random.int64_in_range, range of length 2^63 (bits 0-7)"
       (fun () -> Int64.(to_int
            (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)));
  test "Random.int64_in_range, range of length 2^63 (bits 30-37)"
       (fun () -> Int64.(to_int (shift_right
            (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)
            30)));
  test "Random.int64_in_range, range of length 2^63 (bits 55-62)"
       (fun () -> Int64.(to_int (shift_right
            (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)
            55)));
  test "Random.int64_in_range, full int64 range (bits 0-7)"
       (fun () -> Int64.(to_int
            (Random.int64_in_range ~min:min_int ~max:max_int)));
  test "Random.int64_in_range, full int64 range (bits 30-37)"
       (fun () -> Int64.(to_int (shift_right
            (Random.int64_in_range ~min:min_int ~max:max_int)
            30)));
  test "Random.int64_in_range, full int64 range (bits 56-63)"
       (fun () -> Int64.(to_int (shift_right
            (Random.int64_in_range ~min:min_int ~max:max_int)
            56)));
  test "Random.int64_in_range, range of length 256*p < 2^63 (bits 0-7)"
       (let p = 27021597764223071L in (* prime < 2^55 *)
        let min_ = -1844674407370955197L in
        let max_ = Int64.(pred (add min_ (mul 256L p))) in
        fun () -> Int64.(to_int
            (div (sub (Random.int64_in_range ~min:min_ ~max:max_) min_) p)));
  test "Random.int64_in_range, range of length 256*p > 2^63 (bits 0-7)"
       (let p = 54043195528445957L in (* prime > 2^55 and < 2^56 *)
        let min_ = Int64.min_int in
        let max_ = Int64.(pred (add min_ (mul 256L p))) in
        fun () -> Int64.(to_int
            (unsigned_div (sub (Random.int64_in_range ~min:min_ ~max:max_) min_)
               p)));

  (* [split] *)
  let r1 = Random.split() in
  let r2 = Random.split() in
  let r3 = Random.State.split r1 in
  test "Random.split (r0-r1)"
       (fun () -> Random.bits() - Random.State.bits r1);
  test "Random.split (r1-r2)"
       (fun () -> Random.State.bits r1 - Random.State.bits r2);
  test "Random.split (r2-r3)"
       (fun () -> Random.State.bits r2 - Random.State.bits r3)
