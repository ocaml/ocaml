external floor : float -> float
  = "%asm" "floor_stub" "roundsd	$1, %0, %1	# floor" "mx" "=x"
external ceil : float -> float
  = "%asm" "ceil_stub" "roundsd	$2, %0, %1	# ceil" "mx" "=x"

type t = {
  x : float;
  y : float;
  z : float }
let () =
  let t = { x = 1.5; y = 1.; z = -0.5 } in
  assert (
       floor t.x = 1.
    && floor t.y = 1.
    && floor t.z = (-1.)
    && ceil t.x = 2.
    && ceil t.y = 1.
    && ceil t.z = 0.)
