let f ?(x=true) y ?z ~t u =
  if x && t
     then ((match z with None -> [y] | Some x -> x)) @ u
     else [y]
let _ = 
  f ~x:false 0 ~t:false [3.0]