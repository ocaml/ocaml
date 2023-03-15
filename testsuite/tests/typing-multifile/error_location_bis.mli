#1 "A_far_away_module"
type t = A
module M: sig
#1 "A_small_isolate"
  val zero : int
end
#3 "A_far_away_module"
type s = A
