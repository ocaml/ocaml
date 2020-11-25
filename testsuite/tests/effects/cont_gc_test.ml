(* TEST *)

(* This test intends to exercise the minor and major collectors
   while also dropping, cloning and storing continuations
 *)

effect Yield : unit

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
  match d with
  | 0 -> Node(Empty, Empty)
  | _ -> let d = d - 1 in Node(make d, make d)

let rec check = function Empty -> 0 | Node(l, r) -> 1 + check l + check r

let work () =
  let n = 18 in
  let arr = Array.init n make in
  let x = ref 0 in

  while true do
    let j = Random.int n in
    let next_tree = make j in
    perform Yield;
    x := !x + (check (Array.get arr j));
    Array.set arr j next_tree
  done;
  ()

type stored = None | Cont of (unit,unit) continuation * int tree

let run n =
  let m = 10 in
  let arr = Array.make m None in
  let n_left = ref n in

  let scheduleNext () =
    let rec getCont () =
      let j = Random.int m in
      match (Array.get arr j) with
      | Cont (k, t) -> begin
        ignore (check t);
        Array.set arr j (Cont (Obj.clone_continuation k, make 2));
        k
      end
      | None -> getCont ()
    in

    (* every now and then do some garbage collection *)
    if !n_left mod 10 == 0 then begin
      if Random.bool ()
      then Gc.minor ()
      else Gc.full_major ()
    end;

    (* stop scheduling and return *)
    if !n_left < 0 then ()
    else begin
     n_left := !n_left - 1;
     continue (getCont ()) ()
    end
  in

  (match work () with
  | effect Yield k -> begin
      (* random insert of the continuations *)
      let rec insertClone c =
        let j = Random.int m in
        match Array.get arr j with
        | None -> (Array.set arr j (Cont (c, make 4)); insertClone (Obj.clone_continuation c))
        | Cont (c2, _) -> (Array.set arr j (Cont (c, make 4)); c2)
      in

      (* we drop the last one we stop at *)
      ignore (insertClone k);
      scheduleNext ()
    end
  | () -> scheduleNext ());

  print_endline "ok"

let _ = Random.init 2493

let _ = run 500
