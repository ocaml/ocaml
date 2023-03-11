(* TEST

include systhreads
* hassysthreads
** bytecode
** native

*)


module Atomic = struct
  let make = ref
  let set = (:=)
  let get = (!)
end

let arr = [| 1; 2; 3 |]

let[@inline never] bounds r =
  (* r is live across a bounds check failure *)
  try arr.(42) with
  | _ -> !r

let glob = ref (ref 0)
let () =
  let go = Atomic.make true in
  let gcthread =
    Thread.create (fun () ->
      while Atomic.get go do Thread.yield (); Gc.minor (); done) ()
  in
  while (Gc.quick_stat ()).minor_collections < 1000 do
    let r = ref 42 in
    glob := r; (* force promotion *)
    let n = bounds r in
    if n <> 42 then Printf.printf "%x <> 42!\n%!" n;
  done;
  Atomic.set go false;
  Thread.join gcthread
