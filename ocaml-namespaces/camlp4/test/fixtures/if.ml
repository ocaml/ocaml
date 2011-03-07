let x = if x then a else b in x

let x = if StringSet.mem "*" sections then a else b in x

let x =
if StringSet.mem "*" sections then fun _ -> true else
  fun x -> StringSet.mem x sections
in x
