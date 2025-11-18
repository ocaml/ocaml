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
          while Atomic.get fibers_in == [] do
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
  Fun.protect ~finally begin fun () ->
    let effc (type a) (e : a Effect.t) =
      match e with
      | Migrate ->
        Some (fun (k : (a, unit) Effect.Deep.continuation) ->
          push fibers_in (Effect.Deep.continue k))
      | _ ->
        None
    in
    let handler = { Effect.Deep.retc = Fun.id; exnc = raise; effc } in
    for _ = 1 to 1_000_000 do
      Effect.Deep.match_with (fun () -> Effect.perform Migrate) () handler
    done;
    Printf.printf "OK\n%!"
  end

let () = main ()
