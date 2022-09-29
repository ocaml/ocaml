(* TEST *)

let () =
  assert(Obj.tag Obj.null = 42);
  assert(Obj.size Obj.null = 0)

let atom42 = Obj.new_block 42 0
let atom42' = Obj.new_block 42 0

let () =
  assert(Obj.tag atom42 = 42);
  assert(Obj.size atom42 = 0)

let () =
  assert(atom42 == atom42');
  assert(atom42 != Obj.null)

let str = Marshal.to_string Obj.null []
let unmarshalled = Marshal.from_string str 0

let () =
  assert(unmarshalled == Obj.null)

