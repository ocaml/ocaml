(* TEST
   modules = "subarraycstub.c";
*)

external sub_right_copy:
   Bigarray.(('a, 'b, c_layout) Array2.t)
   -> int
   -> int
   -> Bigarray.(('a, 'b, c_layout) Array2.t) = "stub_sub_right_copy"

let sub_2d ba x0 xn y0 yn=
  let rows = Bigarray.Array2.sub_left ba x0 xn in
  sub_right_copy rows y0 yn

let sub_2d_safe ba x0 xn y0 yn =
  let ba = Bigarray.Array2.sub_left ba x0 xn in
  let copy = Bigarray.Array2.(create (kind ba) (layout ba) xn yn) in
  for row = 0 to xn - 1 do
    let src = Bigarray.Array2.slice_left ba row in
    let dst = Bigarray.Array2.slice_left copy row in
    let src_sub = Bigarray.Array1.sub src y0 yn in
    let dst_sub = Bigarray.Array1.sub dst 0 yn in
    Bigarray.Array1.blit src_sub dst_sub
  done;
  copy

let sub_2d_test ba x0 xn y0 yn =
  let c = sub_2d ba x0 xn y0 yn in
  assert (Bigarray.Array2.dim1 c = xn);
  assert (Bigarray.Array2.dim2 c = yn);
  assert Bigarray.Array2.(layout c = layout ba);
  assert Bigarray.Array2.(kind c = kind ba);
  let expected = sub_2d_safe ba x0 xn y0 yn in
  for i = 0 to Bigarray.Array2.dim1 expected - 1 do
    for j = 0 to Bigarray.Array2.dim2 expected - 1 do
      let got = c.{i, j}
      and want = expected.{i, j} in
      if want <> got then
        Printf.printf "{%d,%d}: Want: %d, got: %d\n" i j want got
    done
  done

let () =
  let a = Array.init_matrix 2048 64  (fun x y -> x *64 + y) in
  let orig = Bigarray.(Array2.of_array int16_signed c_layout a) in

  sub_2d_test orig 1 2047 3 47;
  let rec loop () =
    let (ba : (_, _, _) Bigarray.Array2.t) =
      sub_2d orig 1 2047 3 47 |> Sys.opaque_identity
    in
    Gc.minor ();
    ignore ba
  in
  Gc.full_major ();
  let collections n =
    Gc.full_major ();
    let gc0 = Gc.quick_stat () in
    for _ = 1 to n do
      loop ()
    done;
    let gc1 = Gc.quick_stat () in
    gc1.Gc.major_collections - gc0.Gc.major_collections
  in
  let n1 = collections 1000
  and n2 = collections 10000
  and n3 = collections 30000 in
  if n2 <= n1 || n3 <= n2 then
    Printf.printf "Not enough GC cycles: %d, %d, %d\n" n1 n2 n3
  else
    print_endline "OK"
