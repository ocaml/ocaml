(* TEST
   * flambda
   ** native
   ocamlopt_flags = "-O2 -inline-call-cost 1=20 -unbox-closures"
*)

let hide_until_round_2 init_in_hide f_in_hide =
  let x1_in_hide =
    match init_in_hide with
    | 0 -> true
    | _ -> false
  in
  ignore (Sys.opaque_identity x1_in_hide);
  let x2_in_hide =
    match init_in_hide with
    | 0 -> true
    | _ -> false
  in
  ignore (Sys.opaque_identity x2_in_hide);
  f_in_hide

let foo bar init a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
  let f_outer =
    let baz = bar + 1 in
    let rec f_inner x_in_f y_in_f b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 =
      let dec =
        b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + b11 + b12 + b13
      in
      match x_in_f with
      | Some _ -> g_inner x_in_f (y_in_f - dec)
      | None   -> g_inner x_in_f (y_in_f - 2)
    and g_inner x_in_g y_in_g =
      let a1  = baz + 1 in
      let a2  = a1  + 1 in
      let a3  = a2  + 1 in
      let a4  = a3  + 1 in
      let a5  = a4  + 1 in
      let a6  = a5  + 1 in
      let a7  = a6  + 1 in
      let a8  = a7  + 1 in
      let a9  = a8  + 1 in
      let a10 = a9  + 1 in
      let a11 = a10 + 1 in
      let a12 = a11 + 1 in
      let a13 = a12 + 1 in
      match x_in_g with
      | Some _ ->
          f_inner x_in_g (y_in_g - baz)
                  a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
      | None   ->
          f_inner x_in_g (y_in_g - baz)
                  a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
    in
    f_inner
  in
  let s = Some init in
  let f_through_hide = hide_until_round_2 init f_outer in
  (f_through_hide [@specialised])
    s 10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
