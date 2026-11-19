(* TEST *)

open Complex

let () =
  List.iter (fun re ->
    List.iter (fun im ->
      let x = { re; im } in
      List.iter (fun re ->
        List.iter (fun im ->
          assert (pow x { re; im } = zero)
        ) [0.; -0.]
      ) [0x1p-1074; min_float; 0.5; 1.; 2.; 3.5; max_float]
    ) [0.; -0.]
  ) [0.; -0.];
  assert (pow one one = one)
