let id x = x in
let fst (x,y) = y in
let p = ((fun x -> x+1), 4) in
id fst p 2.0