(* Test the GC. *)

let () = Printf.printf "Hello, world!\n"
let () = Gc.full_major ()
let () = Printf.printf "Hello, world!\n"
let () = Gc.full_major ()
