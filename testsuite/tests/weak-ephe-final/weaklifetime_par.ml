(* This test is currently disabled,
   until the random failures have been investigated.
*)

let () = Random.self_init ()

let size, num_domains, num_gcs, num_rounds =
  let test_size =
    try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
    with Not_found | Failure _ -> 0
  in
  if test_size >= 3
  then (1000, 4, 5, 4)
  else ( 400, 2, 5, 3)

type block = int array

type objdata =
  | Present of block
  | Absent of int  (* GC count at time of erase *)

type bunch = {
  objs : objdata array;
  wp : block Weak.t;
}

let data =
  Array.init size (fun i ->
    let n = 1 + Random.int size in
    {
      objs = Array.make n (Absent 0);
      wp = Weak.create n;
    }
  )

let gccount () =
  let res = (Gc.quick_stat ()).Gc.major_collections in
  res

type change = No_change | Fill | Erase

(* Check the correctness condition on the data at (i,j):
   1. if the block is present, the weak pointer must be full
   2. if the block was removed at GC n, and the weak pointer is still
      full, then the current GC must be at most n+2.
      (could have promotion from minor during n+1 which keeps alive in n+1,
      so will die at n+2)
   Then modify the data in one of the following ways:
   1. if the block and weak pointer are absent, fill them
   2. if the block and weak pointer are present, randomly erase the block
*)
let check_and_change data i j =
  let gc1 = gccount () in
  let change =
    (* we only read data.(i).objs.(j) in this local binding to ensure
        that it does not remain reachable on the bytecode stack
        in the rest of the function below, when we overwrite the value
        and try to observe its collection.  *)
    match data.(i).objs.(j), Weak.check data.(i).wp j with
    | Present x, false -> assert false
    | Absent n, true -> assert (gc1 <= n+2); No_change
    | Absent _, false -> Fill
    | Present _, true ->
      if Random.int 10 = 0 then Erase else No_change
  in
  begin match change with
  | No_change -> ()
  | Fill ->
    let x = Array.make (1 + Random.int 10) 42 in
    data.(i).objs.(j) <- Present x;
    Weak.set data.(i).wp j (Some x);
  | Erase ->
    data.(i).objs.(j) <- Absent gc1;
    let gc2 = gccount () in
    if gc1 <> gc2 then data.(i).objs.(j) <- Absent gc2;
  end


let dummy = ref [||]

let run index () =
  let domain_data = Array.init 100 (fun i ->
    let n = 1 + Random.int 100 in
    {
      objs = Array.make n (Absent 0);
      wp = Weak.create n;
    }
  ) in
  let gc_start = gccount () in
  while gccount () - gc_start < num_gcs do
    dummy := Array.make (Random.int 300) 0;
    let per_domain_size = size / num_domains in
    assert (index < num_domains);
    let i = index * per_domain_size + Random.int per_domain_size in
    let j = Random.int (Array.length data.(i).objs) in
    check_and_change data i j;
    let ix = Random.int 100 in
    let jx = Random.int (Array.length domain_data.(ix).objs) in
    check_and_change domain_data ix jx
  done

let _ =
  for round = 1 to num_rounds do
    (* Each domain owns a region of the data array, starting at
         [index i * (size / num_domains)]
       and of size [size / num_domains].

       Note that the regions are rotated on each round, to help surface
       potential bugs coming from a new domain reusing the memory of
       a previous domain. *)
    let index i = (i + round) mod num_domains in
    let domains = Array.init (num_domains - 1) (fun i -> Domain.spawn(run (index i))) in
    run (index (num_domains) mod num_domains) ();
    Array.iter Domain.join domains
  done;
  print_endline "ok"
