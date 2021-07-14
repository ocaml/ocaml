



let rec map f = function 
|[] ->[]
| l::ls -> (f l)::(map f ls)


let map_from_to f from t =
  let rec map_tail_rec t res =
    if t < from then res else map_tail_rec (t - 1) ((f t)::res)
  in
  map_tail_rec t []
;;

let map_from_downto f from t =
  let rec map_tail_rec t res =
    if t > from then res else map_tail_rec (t + 1) ((f t)::res)
  in
  map_tail_rec t [] 
;;