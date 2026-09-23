(* TEST *)

(* The bulk operations, against the plain loops they replace. *)

module A = Float.Array

let opaque = Sys.opaque_identity

let check name got want =
  if got <> want then
    failwith (Printf.sprintf "%s: got %h, expected %h" name got want)

let close name got want =
  if Float.abs (got -. want) > 1e-9 *. (1. +. Float.abs want) then
    failwith (Printf.sprintf "%s: got %h, expected %h" name got want)

let a = A.init 37 (fun i -> float_of_int (i mod 13) -. 6.)
let b = A.init 37 (fun i -> float_of_int (i mod 7) *. 0.5)

let () =
  (* Small integer values, so the order of the additions cannot matter. *)
  let dot = ref 0. and sum = ref 0. in
  for i = 0 to 36 do
    dot := !dot +. A.get a i *. A.get b i;
    sum := !sum +. A.get a i
  done;
  check "dot" (A.dot (opaque a) (opaque b)) !dot;
  check "sum" (A.sum (opaque a)) !sum;
  check "dot empty" (A.dot (A.create 0) (A.create 0)) 0.;
  check "sum empty" (A.sum (A.create 0)) 0.;

  (* Every length from 0 to 40 exercises the four-wide loop and its tail. *)
  for n = 0 to 40 do
    let x = A.init n (fun i -> float_of_int (i * i mod 11)) in
    let y = A.init n (fun i -> float_of_int (i mod 5)) in
    let dot = ref 0. and sum = ref 0. in
    for i = 0 to n - 1 do
      dot := !dot +. A.get x i *. A.get y i;
      sum := !sum +. A.get x i
    done;
    check "dot" (A.dot x y) !dot;
    check "sum" (A.sum x) !sum;
    let s = A.copy x in
    A.scale 3. s;
    for i = 0 to n - 1 do check "scale" (A.get s i) (3. *. A.get x i) done;
    let z = A.copy y in
    A.axpy 2. x z;
    for i = 0 to n - 1 do
      close "axpy" (A.get z i) (2. *. A.get x i +. A.get y i)
    done;
    let d = A.create n in
    A.add x y d;
    for i = 0 to n - 1 do check "add" (A.get d i) (A.get x i +. A.get y i) done;
    A.mul x y d;
    for i = 0 to n - 1 do check "mul" (A.get d i) (A.get x i *. A.get y i) done;
    A.add x y d;
    A.add x y x;
    for i = 0 to n - 1 do check "add in place" (A.get x i) (A.get d i) done
  done;

  (* Length mismatches raise. *)
  let three = A.make 3 1. and four = A.make 4 1. in
  List.iter
    (fun (name, f) ->
      match f () with
      | () -> failwith (name ^ ": no exception")
      | exception Invalid_argument _ -> ())
    [ "dot", (fun () -> ignore (A.dot three four));
      "axpy", (fun () -> A.axpy 1. three four);
      "add", (fun () -> A.add three four three);
      "mul", (fun () -> A.mul three three four) ]
