module E = Ephemeron.Kn

let test1 =
  let e1 = E.create 16 in
  for i = 0 to 15 do
    E.set_key e1 i i
  done;
  let d = Domain.spawn (fun () ->
    let e2 = E.create 16 in
    E.blit_key e1 0 e2 0 16;
    match E.get_key e2 1 with
    | Some i -> assert (i = 1)
    | None -> assert false)
  in
  Domain.join d;
  print_endline "test1: ok"

let _ = Gc.full_major ()

let test2 =
  let e1 = E.create 16 in
  let d = Domain.spawn (fun () ->
    let e2 = E.create 16 in
    for i = 0 to 15 do
      E.set_key e2 i i
    done;
    E.blit_key e2 0 e1 0 16;
    match E.get_key e1 1 with
    | Some i -> assert (i = 1)
    | None -> assert false)
  in
  Domain.join d;
  print_endline "test2: ok"

let test3 =
  let e1 = E.create 16 in
  let d1 = Domain.spawn (fun () ->
    let e2 = E.create 16 in
    let d2 = Domain.spawn (fun () ->
      match E.blit_key e1 0 e2 0 16 with
      | v -> assert false
      | exception Invalid_argument _ -> ())
    in
    Domain.join d2)
  in
  Domain.join d1;
  print_endline "test3: ok"

let test4 =
  let e1 = E.create 16 in
  for i = 0 to 15 do
    E.set_key e1 i i
  done;
  let e2 = E.create 16 in
  let d = Domain.spawn (fun () ->
    E.blit_key e1 0 e2 0 16;
    match E.get_key e2 1 with
    | Some i -> assert (i = 1)
    | None -> assert false)
  in
  Domain.join d;
  print_endline "test4: ok"
