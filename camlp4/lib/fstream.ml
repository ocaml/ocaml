(* camlp4r *)
(* $Id$ *)
(* Copyright 2001 INRIA *)

type t 'a = { count : int; data : Lazy.t (data 'a) }
and data 'a =
  [ Nil
  | Cons of 'a and t 'a
  | App of t 'a and t 'a ]
;

value from f =
  loop 0 where rec loop i =
    {count = 0;
     data =
       lazy
         (match f i with
          [ Some x -> Cons x (loop (i + 1))
          | None -> Nil ])}
;

value rec next s =
  let count = s.count + 1 in
  match Lazy.force s.data with
  [ Nil -> None
  | Cons a s -> Some (a, {count = count; data = s.data})
  | App s1 s2 ->
      match next s1 with
      [ Some (a, s1) -> Some (a, {count = count; data = lazy (App s1 s2)})
      | None ->
          match next s2 with
          [ Some (a, s2) -> Some (a, {count = count; data = s2.data})
          | None -> None ] ] ]
;

value empty s =
  match next s with
  [ Some _ -> None
  | None -> Some ((), s) ]
;

value nil = {count = 0; data = lazy Nil};
value cons a s = Cons a s;
value app s1 s2 = App s1 s2;
value flazy f = {count = 0; data = Lazy.lazy_from_fun f};

value of_list l =
  List.fold_right (fun x s -> flazy (fun () -> cons x s)) l nil
;

value of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)
;

value of_channel ic =
  from (fun _ -> try Some (input_char ic) with [ End_of_file -> None ])
;

value iter f =
  do_rec where rec do_rec strm =
    match next strm with
    [ Some (a, strm) ->
        let _ = f a in
        do_rec strm
    | None -> () ]
;

value count s = s.count;

value count_unfrozen s =
  loop 0 s where rec loop cnt s =
    if Lazy.lazy_is_val s.data then
      match Lazy.force s.data with
      [ Cons _ s -> loop (cnt + 1) s
      | _ -> cnt ]
    else cnt
;
