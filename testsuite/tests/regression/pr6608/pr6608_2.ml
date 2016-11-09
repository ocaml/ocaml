type ('a, 'b) t = { fst : 'a; snd : 'b };;
let with_fst r fst = { r with fst };;
with_fst { fst=""; snd="" } 2;;
