(* TEST
* hasunix
include unix
** bytecode
** native
*)

open Domain.Sync

let n = 25
let iter = 20

let counts = Array.make n 0
let running = Array.make n false

let main = Domain.self ()

let rec burn l =
  if List.hd l > 14 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let go id =
  counts.(id) <- counts.(id) + 1;
  burn [0];
  running.(id) <- false;
  notify main



let rec wait_all () =
  if critical_section (fun () ->
    let still_running =
      running |> Array.to_list |> List.mapi (fun i r -> i, r) |> List.filter (fun (i, r) -> r) in
    match still_running with
    | [] -> Printf.printf "done\n%!"; true
    | xs ->
       (* Printf.printf "%d... %!" (List.length xs); *)
       wait ();
       false)
  then
    ()
  else
    wait_all ()

let () =
  for i = 1 to iter do
    for j = 0 to n - 1 do
      running.(j) <- true;
      ignore (Domain.spawn (fun () -> go j));
    done;
    wait_all ();
  done;
  for i = 1 to n - 1 do
    assert (counts.(i) = iter)
  done;
  Gc.full_major ()
