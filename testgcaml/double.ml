let double = 
  let plus = generic (+) | (+.) in
  fun x -> plus x x

let _ = Printf.printf "%d %f\n" (double 1) (double 1.2)
