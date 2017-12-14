
module Segfault = struct
  module Nondet = struct
    effect Fork : bool
    let fork () = perform Fork

    let rec forkEach f = function
      | [] -> ()
      | x::xs ->
         (* if branches are swapped, then no segfault occurs *)
         if (fork ()) then (f x) else (forkEach f xs)

    (** Explores alternatives in BFS *)
    let run worlds action =
      let scheduleNext () =
        begin match !worlds with
        | [] -> ()
        | w::ws -> worlds := ws; w ()
        end in
      begin match action () with
      | x -> scheduleNext ()
      | effect Fork k ->
         let k2 = Obj.clone_continuation k in
         let choices = [(fun () -> continue k true); (fun () -> continue k2 false)] in
         worlds := !worlds @ choices;
         scheduleNext ()
      end

    let handle action = run (ref []) action
  end

  let randArray n =
    Array.init n (fun i -> Random.int 1073741823)

  effect Yield: int -> unit
  let yield v = perform (Yield v)

  let boom () =
    let unyield action () =
      try action () with
        effect (Yield _) _ -> ()
    in
    let state = ref [] in
    let stateful action () =
      try action () with
      | effect (Yield i) k ->
         state := i :: !state;
         (* Important: each yield invocation does not return. *)
         Nondet.forkEach yield !state;
         (* Exactly one of the forked computations will return here. *)
         continue k ()
    in
    let count = 10000 in (* for count < 10000, no segfault *)
    let a1 = randArray count in
    let iter () = Array.iter yield a1 in
    Nondet.handle (unyield (stateful iter))

  let _ = boom ()
  let _ = print_string "ok"
end
