type color = White | Gray | Blue | Black

external make_block: nativeint -> color -> nativeint -> Obj.t
         = "make_block"

external make_raw_pointer: nativeint -> Obj.t
         = "make_raw_pointer"

let do_gc root =
  Gc.compact();   (* full major + compaction *)
  root
