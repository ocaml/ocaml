(* TEST *)

open Effect

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
    Effect.run_with stop Lazy.force l
      { result = Fun.id;
        exn = raise;
        operation =
          (fun (type a) (Stop : (a, _) operation) (k : (a, _) continuation) ->
            Effect.continue k ()) }
  in
  Printf.printf "%d\n" v1;
  let l2 = lazy (f 2_000) in
  let v2 =
    Effect.run_with stop Lazy.force l2
      { result = Fun.id;
        exn = raise;
        operation =
          (fun (type a) (Stop : (a, _) operation) (k : (a, _) continuation) ->
            let d = Domain.spawn(fun () -> Effect.continue k ()) in
            Domain.join d) }
  in
  Printf.printf "%d\n" v2;
  let l3 = lazy (f 3_000) in
  let _ =
    Effect.run_with stop Lazy.force l3
      { result = Fun.id;
        exn = raise;
        operation =
          (fun (type a) (Stop : (a, _) operation) _ ->
            try
              let d = Domain.spawn(fun () -> Lazy.force l3) in
              Domain.join d
            with CamlinternalLazy.Undefined ->
              Printf.printf "Undefined\n"; 0) }
  in
  ()
