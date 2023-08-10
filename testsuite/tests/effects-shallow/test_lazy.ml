(* TEST *)

type stop = effect Stop : unit

let stop = Effect.create ()

let f count =
  let r = ref 0 in
  for i = 1 to count do
    incr r;
    if i = count / 2 then Effect.perform stop Stop
  done;
  !r

let _ =
  let l = lazy (f 1_000) in
  let v1 =
    let rec handle = function
      | Effect.Result x -> x
      | Effect.Exn e -> raise e
      | Effect.Operation(Stop, k) -> handle (Effect.continue k ())
    in
    handle (Effect.run stop Lazy.force l)
  in
  Printf.printf "%d\n" v1;
  let l2 = lazy (f 2_000) in
  let v2 =
    let rec handle = function
      | Effect.Result x -> x
      | Effect.Exn e -> raise e
      | Effect.Operation(Stop, k) -> 
          let d = Domain.spawn (fun () -> handle (Effect.continue k ())) in
          Domain.join d
    in
    handle (Effect.run stop Lazy.force l2)
  in
  Printf.printf "%d\n" v2;
  let l3 = lazy (f 3_000) in
  let _ =
    match Effect.run stop Lazy.force l3 with
    | Result x -> x
    | Exn e -> raise e
    | Operation(Stop, _) ->
        try
          let d = Domain.spawn(fun () -> Lazy.force l3) in
          Domain.join d
        with CamlinternalLazy.Undefined ->
          Printf.printf "Undefined\n"; 0 
  in
  ()
