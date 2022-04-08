(* TEST *)

module A = Bigarray.Genarray
module B = Bigarray

let shape = [|2; 2; 2; 2|]
let make () = A.init B.Float64 B.C_layout shape  (fun _ -> 0.)

let increase_refcount a () = B.reshape a shape

let round a =
  let d1 = Domain.spawn (increase_refcount a) in
  let d2 = Domain.spawn (increase_refcount a) in
  let d3 = Domain.spawn (increase_refcount a) in
  let d4 = Domain.spawn (increase_refcount a) in
  let all = [| Domain.join d1; Domain.join d2; Domain.join d3; Domain.join d4; make () |] in
  all.(Random.int (Array.length all))

let rec test n a = if n = 0 then () else test (n-1) (round a)

let () = test 10_000 (make ())
