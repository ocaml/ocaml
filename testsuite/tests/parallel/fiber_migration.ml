(* TEST
 multicore;
 {
   bytecode;
 }{
   native;
 }
*)

type _ Effect.t += Migrate : unit Effect.t

let rec push stack item =
  let before = Atomic.get stack in
  let after = item :: before in
  if not (Atomic.compare_and_set stack before after) then
    push stack item

let main () =
  let fibers_in = Atomic.make [] in
  let receiving_domain = Domain.spawn begin fun () ->
    try
      let rec loop = function
        | fiber :: fibers ->
          fiber ();
          loop fibers
        | [] ->
          while Atomic.get fibers_in = [] do
            Domain.cpu_relax ()
          done;
          loop (List.rev (Atomic.exchange fibers_in []))
      in
      loop []
    with Exit -> ()
  end in
  let finally () =
    push fibers_in (fun () -> raise Exit);
    Domain.join receiving_domain
  in
  let rec busy_wait () =
    if Atomic.get fibers_in <> [] then
      (Domain.cpu_relax (); busy_wait ())
  in
  Fun.protect ~finally begin fun () ->
    for _ = 1 to 50 do
      for _ = 1 to 10 do
        match Effect.perform Migrate with
        | () -> ()
        | effect Migrate, k -> push fibers_in (Effect.Deep.continue k)
      done;
      busy_wait ();
    done;
    let cache_size = (Gc.quick_stat()).live_stacks_words in
    if (cache_size < 25000) then
      Printf.printf "OK\n%!"
    else Printf.printf "Bad %d\n%!" cache_size
  end

let () = main ()
