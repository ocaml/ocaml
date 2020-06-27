(* TEST
   * bytecode
*)

let x = Array.make 100 "abcd"
let _ = Gc.field_path x
let _ = Gc.print_reachable 100
