(* TEST *)
open Gc.Memprof

let stuff = ref [4;3;2;1;9;8;7;6;5]
let[@inline never] work () =
  let r =
    !stuff
    |> List.map (fun x -> x, Printf.sprintf "{%04d}" x)
    |> List.sort compare in
  assert (List.assoc 5 (Sys.opaque_identity r) = "{0005}")


let record cb =
  let r = ref [] in
  start ~callstack_size:10 ~sampling_rate:0.3
    { null_tracker with
      alloc_minor = (fun info -> cb (); r := (info.size, info.n_samples) :: !r; None)
    };
  work ();
  stop ();
  List.rev !r

let glob_small = ref []
let glob_big = ref [| |]
let () =
  let r1 = record (fun () -> ()) in
  let r2 = record (fun () ->
     (* do both large and small allocations *)
     glob_big := Array.make 10000 (-1);
     let r = ref [] in
     for i = 1 to 10 do
       r := ref i :: !r
     done;
     glob_small := !r) in
  assert (r1 = r2);
  print_endline "ok"

