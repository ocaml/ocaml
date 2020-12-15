let y f = 
  (fun x -> f x x) (fun x -> f x x)

let a = y y

