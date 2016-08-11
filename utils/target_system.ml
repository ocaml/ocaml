module Address = struct
  type t =
    | Int32 of Int32.t
    | Int64 of Int64.t

  type word_size = Four | Eight

  let word_size () =
    match Sys.word_size with
    | 32 -> Four
    | 64 -> Eight
    | bits -> Misc.fatal_errorf "Unknown word size %d" bits

  let zero () =
    match word_size () with
    | Four -> Int32 Int32.zero
    | Eight -> Int64 Int64.zero

  let all_ones () =
    match word_size () with
    | Four -> Int32 Int32.minus_one
    | Eight -> Int64 Int64.minus_one

  let thirty_two_bit_bounds =
    Int64.of_int32 Int32.min_int,
      Int64.of_int32 Int32.max_int

  let of_int_exn i =
    let min, max =
      match word_size () with
      | Four -> thirty_two_bit_bounds
      | Eight -> Int64.min_int, Int64.max_int
    in
    let i' = Int64.of_int i in
    if Int64.compare i' min < 0 || Int64.compare i' max > 0 then
      Misc.fatal_errorf "Target_system.Address.of_int_exn: 0x%Ld out of range"
        i'
    else
      match word_size () with
      | Four -> Int32 (Int32.of_int i)
      | Eight -> Int64 i'

  let to_int64 = function
    | Int32 i -> Int64.of_int32 i
    | Int64 i -> i
end
