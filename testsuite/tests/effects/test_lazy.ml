(* TEST *)

open Effect
open Effect.Deep

type _ t += Stop : unit t

let f count =
  let r = ref 0 in
  for i = 1 to count do
    incr r;
    if i = count / 2 then perform Stop
  done;
  !r

let _ =
  let l = lazy (f 1_000) in
  let v1 =
    try_with Lazy.force l
    { effc = fun (type a) (e : a t) ->
        match e with
        | Stop -> Some (fun (k : (a, _) continuation) -> continue k ())
        | _ -> None }
  in
  Printf.printf "%d\n" v1;
  let l2 = lazy (f 2_000) in
  let v2 =
    try_with Lazy.force l2
    { effc = fun (type a) (e : a t) ->
        match e with
        | Stop -> Some (fun (k : (a, _) continuation) ->
            let d = Domain.spawn(fun () -> continue k ()) in
            Domain.join d)
        | _ -> None }
  in
  Printf.printf "%d\n" v2;
  let l3 = lazy (f 3_000) in
  let _ =
    try_with Lazy.force l3
    { effc = fun (type a) (e : a t) ->
        match e with
        | Stop -> Some (fun _ ->
            try
              let d = Domain.spawn(fun () -> Lazy.force l3) in
              Domain.join d
            with CamlinternalLazy.Undefined -> Printf.printf "Undefined\n"; 0)
        | _ -> None }
  in
  ()
