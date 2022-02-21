(* TEST
* hasunix
include unix
** bytecode
** native
*)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0

let num_domains =
  if test_size >= 2 then 100 else 14

let main_join n =
  let a = Array.init n (fun _ -> false) in
  Array.init n (fun i -> Domain.spawn (fun () ->
    a.(i) <- true;
    Some i
  )) |>
  Array.iteri (fun i d -> let v = Domain.join d in assert (v = Some i));
  let ok = Array.to_list a |> List.for_all (fun x -> x) in
  assert ok

let rec other_join flags n domain =
  if n > 0 then
    Domain.spawn (fun () -> flags.(n-1) <- true; Domain.join domain)
    |> other_join flags (n-1)
  else
    Domain.join domain

exception Ex of string
let join_exn () =
  match Domain.(join (spawn (fun () -> raise (Ex (String.make 5 '!'))))) with
  | _ -> assert false
  | exception (Ex "!!!!!") -> ()

let join_slow () =
  let rec burn l =
    if List.hd l > 14 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1)) in
  assert (Domain.(join (spawn (fun () -> burn [0]; 42))) = 42)


let join2 () =
  let r = ref false in
  let t = Domain.spawn (fun () -> r := true) in
  Domain.join t;
  assert !r;
  try
    Domain.join t;
    assert false
  with Invalid_argument _ ->
    assert !r

let () =
  main_join num_domains;
  let flags = Array.make num_domains false in
  other_join flags (Array.length flags) (Domain.spawn ignore);
  assert (Array.for_all (fun x -> x) flags);
  join2 ();
  join_exn ();
  join_slow ();
  Gc.full_major ();
  Gc.full_major ()
