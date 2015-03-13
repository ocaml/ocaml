external floor : float -> float
  = "%asm" "floor_stub" "roundsd	$1, %0, %1" "x" "=x"
external ceil : float -> float
  = "%asm" "ceil_stub" "roundsd	$2, %0, %1" "x" "=x"

let () =
  assert (floor 1.5 = 1.);
  assert (floor 1.  = 1.);
  assert (floor (-0.5) = (-1.))
