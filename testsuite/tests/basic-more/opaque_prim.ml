
let f x = opaque x

let () =
  assert(f f == f);
  assert(opaque 1 = 1);
  assert(opaque 1. = 1.)

