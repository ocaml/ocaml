(* List operations *)

let rec length = function
    [] -> 0
  | a::l -> 1 + length l

let hd = function
    [] -> failwith "hd"
  | a::l -> a

let tl = function
    [] -> failwith "tl"
  | a::l -> l

let rec rev_append accu = function
    [] -> accu
  | a::l -> rev_append (a :: accu) l

let rev l = rev_append [] l

let rec flatten = function
    [] -> []
  | l::r -> l @ flatten r

let rec map f = function
    [] -> []
  | a::l -> let r = f a in r :: map f l

(* let rec map f = function
    [] -> []
  | a::l -> f a :: map f l *)

let rec iter f = function
    [] -> ()
  | a::l -> f a; iter f l


let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu)

let rec map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> f a1 a2 :: map2 f l1 l2
  | (_, _) -> invalid_arg "List.map2"

let rec iter2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
  | (_, _) -> invalid_arg "List.iter2"

let rec fold_left2 f accu l1 l2 =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> fold_left2 f (f accu a1 a2) l1 l2
  | (_, _) -> invalid_arg "List.fold_left2"

let rec fold_right2 f l1 l2 accu =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> f a1 a2 (fold_right2 f l1 l2 accu)
  | (_, _) -> invalid_arg "List.fold_right2"

let rec for_all p = function
    [] -> true
  | a::l -> p a & for_all p l

let rec exists p = function
    [] -> false
  | a::l -> p a or exists p l

let rec mem x = function
    [] -> false
  | a::l -> a = x or mem x l

let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if a = x then b else assoc x l

let rec mem_assoc x = function
    [] -> false
  | (a,b)::l -> a = x or mem_assoc x l

let rec assq x = function
    [] -> raise Not_found
  | (a,b)::l -> if a == x then b else assq x l

let rec split = function
    [] -> ([], [])
  | (x,y)::l ->
      let (rx, ry) = split l in (x::rx, y::ry)

let rec combine = function
    ([], []) -> []
  | (a1::l1, a2::l2) -> (a1, a2) :: combine(l1, l2)
  | (_, _) -> invalid_arg "List.combine"
