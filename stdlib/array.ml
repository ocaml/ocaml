(* Array operations *)

external length : 'a array -> int = "%array_length"
external unsafe_get: 'a array -> int -> 'a = "%array_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_set"
external new: int -> 'a -> 'a array = "make_vect"

let get a n =
  if n < 0 or n >= length a
  then invalid_arg "Array.get"
  else unsafe_get a n

let set a n v =
  if n < 0 or n >= length a
  then invalid_arg "Array.set"
  else unsafe_set a n v

let new_matrix sx sy init =
  let res = new sx [||] in
  for x = 0 to pred sx do
    unsafe_set res x (new sy init)
  done;
  res

let copy a =
  let l = length a in
  if l = 0 then [||] else begin
    let r = new l (unsafe_get a 0) in
    for i = 1 to l-1 do
      unsafe_set r i (unsafe_get a i)
    done;
    r
  end

let concat_aux a1 a2 l1 l2 init =
  let r = new (l1 + l2) init in
  for i = 0 to l1 - 1 do unsafe_set r i (unsafe_get a1 i) done;  
  for i = 0 to l2 - 1 do unsafe_set r (i + l1) (unsafe_get a1 i) done;  
  r

let concat a1 a2 =
  let l1 = length a1 and l2 = length a2 in
  if l1 = 0 & l2 = 0 then [||] else begin
    let r = new (l1 + l2) (unsafe_get (if l1 > 0 then a1 else a2) 0) in
    for i = 0 to l1 - 1 do unsafe_set r i (unsafe_get a1 i) done;  
    for i = 0 to l2 - 1 do unsafe_set r (i + l1) (unsafe_get a1 i) done;  
    r
  end

let sub a ofs len =
  if ofs < 0 or len < 0 or ofs + len > length a then invalid_arg "Array.sub"
  else if len = 0 then [||]
  else begin
    let r = new len (unsafe_get a ofs) in
    for i = 1 to len - 1 do unsafe_set r i (unsafe_get a (ofs + i)) done;
    r
  end

let fill a ofs len v =
  if ofs < 0 or len < 0 or ofs + len > length a
  then invalid_arg "Array.fill"
  else for i = ofs to ofs + len - 1 do unsafe_set a i v done

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 or ofs1 < 0 or ofs1 + len > length a1
             or ofs2 < 0 or ofs2 + len > length a2
  then invalid_arg "Array.blit"
  else
    for i = 0 to len - 1 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let map f a =
  let l = length a in
  if l = 0 then [||] else begin
    let r = new l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f(unsafe_get a i))
    done;
    r
  end

let to_list a =
  let len = length a in
  let rec tolist i =
    if i >= len then [] else unsafe_get a i :: tolist(i+1) in
  tolist 0

let of_list = function
    [] -> [||]
  | hd::tl ->
      let a = new (List.length tl + 1) hd in
      let rec fill i = function
          [] -> a
        | hd::tl -> unsafe_set a i hd; fill (i+1) tl in
      fill 1 tl
