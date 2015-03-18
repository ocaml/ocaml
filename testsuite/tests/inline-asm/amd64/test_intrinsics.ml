open Intrinsics

let () =
  let open Intrinsics.Sse2 in
  let x = !(ref (_mm_set_pd 1. 2.)) in
  let y = _mm_set_pd 3. 4. in
  let z = _mm_add_sd x y in
  assert (z = _mm_set_pd 4. 2.);
  let z = _mm_add_sd y x in
  assert (z = _mm_set_pd 4. 4.);
  let z = _mm_add_pd x y in
  assert (z = _mm_set_pd 4. 6.);
  let z = _mm_add_pd y x in
  assert (z = _mm_set_pd 4. 6.);
  let z = _mm_sub_sd x y in
  assert (z = _mm_set_pd (-2.) 2.);
  let z = _mm_sub_sd y x in
  assert (z = _mm_set_pd 2. 4.);
  let z = _mm_sub_pd x y in
  assert (z = _mm_set_pd (-2.) (-2.));
  let z = _mm_sub_pd y x in
  assert (z = _mm_set_pd 2. 2.);
  let z = _mm_mul_sd x y in
  assert (z = _mm_set_pd 3. 2.);
  let z = _mm_mul_sd y x in
  assert (z = _mm_set_pd 3. 4.);
  let z = _mm_mul_pd x y in
  assert (z = _mm_set_pd 3. 8.);
  let z = _mm_mul_pd y x in
  assert (z = _mm_set_pd 3. 8.);
  let z = _mm_div_sd x y in
  assert (z = _mm_set_pd (1. /. 3.) 2.);
  let z = _mm_div_sd y x in
  assert (z = _mm_set_pd 3. 4.);
  let z = _mm_div_pd x y in
  assert (z = _mm_set_pd (1. /. 3.) 0.5);
  let z = _mm_div_pd y x in
  assert (z = _mm_set_pd 3. 2.);
  let z = _mm_sqrt_sd x in
  assert (z = _mm_set_pd (sqrt 1.) 0.);
  let z = _mm_sqrt_pd x in
  assert (z = _mm_set_pd 1. (sqrt 2.));
  let z = _mm_min_sd x y in
  assert (z = _mm_set_pd 1. 2.);
  let z = _mm_min_sd y x in
  assert (z = _mm_set_pd 1. 4.);
  let z = _mm_min_pd x y in
  assert (z = _mm_set_pd 1. 2.);
  let z = _mm_min_pd y x in
  assert (z = _mm_set_pd 1. 2.);
  let z = _mm_max_sd x y in
  assert (z = _mm_set_pd 3. 2.);
  let z = _mm_max_sd y x in
  assert (z = _mm_set_pd 3. 4.);
  let z = _mm_max_pd x y in
  assert (z = _mm_set_pd 3. 4.);
  let z = _mm_max_pd y x in
  assert (z = _mm_set_pd 3. 4.)

let () =
  let _, _, c, _ = __cpuid 1 in
  if c land bit_AVX <> 0 then begin
    let open Intrinsics.Avx in
    let x = !(ref (_mm256_set_pd 1. 2. 3. 4.)) in
    let y = _mm256_set_pd 5. 6. 7. 8. in
    let z = _mm256_add_pd x y in
    assert (z = _mm256_set_pd 6. 8. 10. 12.);
    let z = _mm256_add_pd y x in
    assert (z = _mm256_set_pd 6. 8. 10. 12.);
  end

