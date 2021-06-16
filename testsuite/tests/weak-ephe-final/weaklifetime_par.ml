(* TEST
*)

let size = 1000;;
let num_domains = 4;;
let random_state = Domain.DLS.new_key Random.State.make_self_init

let random_int = Random.State.int (Domain.DLS.get random_state)

type block = int array;;

type objdata =
  | Present of block
  | Absent of int  (* GC count at time of erase *)
;;

type bunch = {
  objs : objdata array;
  wp : block Weak.t;
};;

let data =
  Array.init size (fun i ->
    let n = 1 + random_int size in
    {
      objs = Array.make n (Absent 0);
      wp = Weak.create n;
    }
  )
;;

let gccount () =
  let res = (Gc.quick_stat ()).Gc.major_collections in
  res

type change = No_change | Fill | Erase;;

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
      if random_int 10 = 0 then Erase else No_change
  in
  match change with
  | No_change -> ()
  | Fill ->
    let x = Array.make (1 + random_int 10) 42 in
    data.(i).objs.(j) <- Present x;
    Weak.set data.(i).wp j (Some x);
  | Erase ->
    data.(i).objs.(j) <- Absent gc1;
    let gc2 = gccount () in
    if gc1 <> gc2 then data.(i).objs.(j) <- Absent gc2;
;;

let dummy = ref [||];;

let run index () =
  let domain_data = Array.init 100 (fun i ->
    let n = 1 + random_int 100 in
    {
      objs = Array.make n (Absent 0);
      wp = Weak.create n;
    }
  ) in
  while gccount () < 5 do
    dummy := Array.make (random_int 300) 0;
    let i = (random_int (size/num_domains)) + index * size/num_domains in
    let j = random_int (Array.length data.(i).objs) in
    check_and_change data i j;
    let ix = random_int 100 in
    let jx = random_int (Array.length domain_data.(ix).objs) in
    check_and_change domain_data ix jx
  done

let _ =
  for index = 0 to 4 do
    let domains = Array.init (num_domains - 1) (fun i -> Domain.spawn(run ((i + index) mod 5))) in
    run ((num_domains - 1 + index) mod 5) ();
    Array.iter Domain.join domains
  done;
  print_endline "ok"
