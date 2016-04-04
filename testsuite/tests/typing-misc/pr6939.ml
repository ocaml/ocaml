
let rec x = [| x |]; 1.;;

let rec x = let u = [|y|] in 10. and y = 1.;;
