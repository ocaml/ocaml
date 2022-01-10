(* TEST
 *)

let f x =
  let a0 = ref 1 in
  let a1 = ref 1 in
  let a2 = ref 1 in
  let a3 = ref 1 in
  let a4 = ref 1 in
  let a5 = ref 1 in
  let a6 = ref 1 in
  let a7 = ref 1 in
  let a8 = ref 1 in
  let a9 = ref 1 in
  let a10 = ref 1 in
  let a11 = ref 1 in
  let a12 = ref 1 in
  if x then raise Not_found;
  [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12 |]

let () =
  for i = 1 to 50000 do
    let rs = Sys.opaque_identity f false in
    assert (Array.for_all (fun x -> !x = 1) rs);
    let _ = Array.make (Random.int 30) 'a' in ()
  done;
  print_string "ok\n"
