module M = struct
  type t = A of int * int * int * int * int * int * int * int * int * int
    | B | B | B | B | B | B | B | B
  and t2 =
    | B | B | B | B | B | B | B | B
  and t3 =
    | B | B | B of a * a * a * a * a * a * a * a * a * a * a | B | B | B | B | B
  and t4 =
    | B | B | B | B | B | B | B | B
  and t5 =
    | B | B | B | B | B | B | B | B
  and t6 =
    | B | B | B |  A of int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int | B | B | B | B | B
  and t7 =
    | B | B | B | B | B | B | B | B
  and t8 =
    | B | B | B | B | B | B | B | B
  and t9 =
    | B | B | B | B | B | B | B | B
  and t10 =
    | A of (a * a)
end
