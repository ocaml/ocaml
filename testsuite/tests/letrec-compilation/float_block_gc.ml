(* TEST *)

(* A [let rec]-bound float record is pre-allocated as a dummy block, which
   [caml_update_dummy] later fills with unboxed floats. If the major GC has
   marked the dummy as a scannable block in the meantime, it must not go on
   to scan those floats. The large live array keeps marking work outstanding,
   and the per-iteration major allocation keeps major cycles turning over. *)

type r = { a : float; b : float; c : float; d : float; e : float; f : float;
           g : float; h : float; i : float; j : float; k : float; l : float }

let churn x =
  let a = Array.make 64 x in
  float_of_int (Array.length a) +. 0.5

let mk () =
  let rec r = { a = churn 1; b = churn 2; c = churn 3; d = churn 4;
                e = churn 5; f = churn 6; g = churn 7; h = churn 8;
                i = churn 9; j = churn 10; k = churn 11; l = churn 12 }
  and get () = r.a in
  get

let big = Array.init 1_000_000 (fun i -> ref i)

let () =
  for _ = 1 to 200_000 do
    ignore (Sys.opaque_identity (Array.make 1000 0));
    let get = mk () in
    assert (get () = 64.5)
  done;
  ignore (Sys.opaque_identity big)
