(* TEST *)

let[@inline never] small_match n x =
  let (left, right) = match x with
    | 0 -> n, 42
    | 1 -> 42, n
    | _ -> assert false in
  left - right

let[@inline never] big_match n x =
  let (left, right) = match x with
    | 0 -> n, 42
    | 1 -> 42, n
    | 2 -> 42-n, 0
    | 3 -> 0, 42-n
    | 4 -> n/2, n/2
    | 5 -> n, n
    | _ -> assert false in
  left - right

let[@inline never] string_match n x =
  let (left, right) = match x with
    | "0" -> n, 42
    | "1" -> 42, n
    | "2" -> 42-n, 0
    | "3" -> 0, 42-n
    | "4" -> n/2, n/2
    | "5" -> n, n
    | _ -> assert false in
  left - right




let printf = Printf.printf

let test f n i =
  let mw_overhead =
    let a = Gc.minor_words () in
    let b = Gc.minor_words () in
    b -. a in
  let mw = Gc.minor_words () in
  let k = f n i in
  assert (k = 0);
  let mw' = Gc.minor_words () in
  let delta = int_of_float (mw' -. mw -. mw_overhead) in
  printf "allocated %d words\n" delta

let () =
  let n = 42 in
  printf "small_match:\n";
  for i = 0 to 1 do test small_match n i done;
  printf "big_match:\n";
  for i = 0 to 5 do test big_match n i done;
  printf "string_match:\n";
  for i = 0 to 5 do test string_match n (string_of_int i) done
