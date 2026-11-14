(* TEST *)

class foo (name : string) =
  object (self)
    initializer
      Gc.finalise_last (fun () -> Printf.printf "%s is collected\n%!" name) self

    method name = name
  end

let () =
  let[@inline never][@local never] first () =
    let f = new foo "first" in
    Printf.printf "Done with %s\n%!" f#name
  in
  first ();
  let f = new foo "second" in
  Gc.compact ();
  Printf.printf "Still holding %s\n%!" f#name
