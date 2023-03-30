
type 'a t = (::) of 'a * 'a list

let rec length_aux len (l : _ list) =
  match l with
    [] -> len
  | _::l -> length_aux (len + 1) l

let length (_::l) = length_aux 1 l

let compare_lengths (_::l1) (_::l2) = List.compare_lengths l1 l2

let cons a (x :: xs) = a :: x :: xs

let hd (x :: _) = x
let tl (_ :: xs) = xs

let to_list (x :: xs) = List.(x :: xs)

let of_list_exn = function
  | [] -> invalid_arg "Nonempty_list.of_list_exn"
  | ((x :: xs) : _ list) -> x :: xs

let of_array_exn = function
  | [||] -> invalid_arg "Nonempty_list.of_array_exn"
  | ar ->
      let list = Array.to_list ar in
      of_list_exn list

let iter f = function
  | a::l -> f a; List.iter f l

let map f = function
  | a::l -> let r1 = f a in r1:: List.map f l

let map_to_list f = function
  | a::l -> let r1 = f a in List.cons r1 (List.map f l)

let[@tail_mod_cons] rec mapi_list i f l =
  match (l : _ list) with
  | [] -> []
  | [a1] ->
      let r1 = f i a1 in
      List.([r1])
  | a1::a2::l ->
      let r1 = f i a1 in
      let r2 = f (i+1) a2 in
      let open List in
      r1::r2::(mapi_list (i+2) f l)
and mapi f (x :: xs) =
  f 0 x :: mapi_list 1 f xs

let fold_left f accu l =
  match l with
  | a::l -> List.fold_left f (f accu a) l

let fold_right f l accu =
  match l with
  | a::l -> f a (List.fold_right f l accu)

let for_all p = function
  | a::l -> p a && List.for_all p l

let exists p = function
  | a::l -> p a || List.exists p l

let find p = function
  | x :: l -> if p x then x else List.find p l

let find_all p = function
  | x :: l -> if p x then List.(x :: find_all p l) else List.find_all p l

let filter = find_all

let mem_assoc x = function
  | (a, _) :: l -> compare a x = 0 || List.mem_assoc x l

let assoc x = function
  | (a,b)::l -> if compare a x = 0 then b else List.assoc x l
