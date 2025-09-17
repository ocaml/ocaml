(* TEST
  no-tsan; (* TSan detects the intentional data race *)
  { bytecode; }
  { native; }
*)

let () = Random.self_init ()

let num_domains = 4
let iters = 1_000_000
let len = 10_000
let table = Weak.create len

let go () =
  for i = 1 to 1_000_000 do
    let s = string_of_int i in
    let h = String.hash s mod len in
    begin match Weak.get table h with
    | None -> ()
    | Some s' -> assert (String.hash s' mod len = h)
    end;
    Weak.set table h (Some s)
  done

let () =
  let domains = Array.init (num_domains - 1) (fun _ -> Domain.spawn go) in
  go ();
  Array.iter Domain.join domains;
  print_endline "ok"
