module MySet = Set.Make(String);;
let set = MySet.empty;;
assert (MySet.is_empty set);;
