(* TEST *)

(* Testing unsynchronized, parallel Weak usage *)

(* Issue 11749 *)
let test () =
  let weak_size = 8 in
  let t = Weak.create weak_size in
  let () = Weak.set t 3 (Some 2L) in
  let wait = Atomic.make true in
  let d1 =
    Domain.spawn
      (fun () ->
         while Atomic.get wait do Domain.cpu_relax() done; Weak.set t 3 None
      ) in
  let d2 =
    Domain.spawn
      (fun () ->
         Atomic.set wait false;
         let res = Weak.get t 3 in
         match res with
         | None
         | Some 2L -> ()
         | Some i -> failwith ("received: " ^ (Int64.to_string i))
      ) in
  let () = Domain.join d1 in
  let () = Domain.join d2 in
  ()

let () =
  for i = 0 to 100 do
    test ()
  done
