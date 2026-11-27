(* TEST *)

open Complex

let signed_zeros =
  [zero; { re = 0.; im = -0. }; { re = -0.; im = 0. }; { re = -0.; im = -0. }]

let test_zero_base x =
  List.iter (fun y -> assert (pow x y = one)) signed_zeros;
  List.iter (fun re ->
    List.iter (fun im -> assert (pow x { re; im } = zero))
      [0.; -0.; 1.; -1.; max_float; -.max_float]
  ) [0x1p-1074; min_float; 0.5; 1.; 2.; 3.5; max_float; infinity];
  List.iter (fun y ->
    let z = pow x y in
    assert (Float.is_nan z.re && Float.is_nan z.im)
  ) [neg one; i; neg i; { re = -1.; im = 1. };
     { re = neg_infinity; im = 0. }; { re = nan; im = 0. };
     { re = 1.; im = infinity }; { re = 1.; im = nan };
     { re = infinity; im = infinity }; { re = infinity; im = nan }]

let () =
  List.iter test_zero_base signed_zeros;
  assert (pow one one = one)
