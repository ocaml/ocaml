let n = 25
let iter = 20

let counts = Array.make n 0
let running = Array.make n false

let main = Domain.self ()

external critical_section : int -> unit = "caml_ml_domain_critical_section"
external interrupt : Domain.id -> unit = "caml_ml_domain_interrupt"
external yield : unit -> unit = "caml_ml_domain_yield"

let rec burn l =
  if List.hd l > 14 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let go id =
  counts.(id) <- counts.(id) + 1;
  burn [0];
  running.(id) <- false;
  interrupt main


let crit f =
  critical_section 1;
  let x = f () in
  critical_section (-1);
  x

let rec wait_all () =
  if crit (fun () ->
    let still_running =
      running |> Array.to_list |> List.mapi (fun i r -> i, r) |> List.filter (fun (i, r) -> r) in
    match still_running with
    | [] -> Printf.printf "done\n%!"; true
    | xs ->
       (* Printf.printf "%d... %!" (List.length xs); *)
       yield ();
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
