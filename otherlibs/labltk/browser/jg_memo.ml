(* $Id$ *)

type ('a, 'b) assoc_list =
    Nil
  | Cons of 'a * 'b * ('a, 'b) assoc_list

let rec assq :key = function
    Nil -> raise Not_found
  | Cons (a, b, l) ->
      if key == a then b else assq :key l

let fast fun:f =
  let memo = ref Nil in
  fun key ->
    try assq :key !memo
    with Not_found ->
      let data = f key in
      memo := Cons(key, data, !memo);
      data
  
  
