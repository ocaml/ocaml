external floor : float -> float
  = "%asm" "floor_stub" "roundsd	$1, %0, %1" "x" "=x"
external ceil : float -> float
  = "%asm" "ceil_stub" "roundsd	$2, %0, %1" "x" "=x"

let () =
  assert (
       floor 1.5 = 1.
    && floor 1. = 1.
    && floor (-0.5) = (-1.)
    && ceil 1.5 = 2.
    && ceil 1. = 1.
    && ceil (-0.5) = 0.)
