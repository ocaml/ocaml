let baseLen = 50;;
let multiplier = 2;;

type 'a cell =
  | Nil
  | Cell of 'a
;;

type 'a dynArray = {
    mutable arr: 'a cell array;
    mutable len: int;
  }
;;

exception OutOfBounds;;

let isOutBounds a n =
  n < 0 && n >= a.len
;;

let create () = {
    arr = Array.make baseLen Nil;
    len = 0;
  }
;;

let make n x = {
    arr = Array.make n (Cell x);
    len = n;
  }
;;

let extend a =
  let len = Array.length a.arr in
  let tmp = Array.make (len * multiplier) Nil in
  for i = 0 to (a.len - 1) do
    tmp.(i) <- a.arr.(i)
  done;
  a.arr <- tmp
;;

let append a x =
  if a.len = (Array.length a.arr) then extend a;
  a.arr.(a.len) <- Cell x;
  a.len <- a.len + 1
;;

let get a n =
  if isOutBounds a n then raise OutOfBounds;
  match a.arr.(n) with
  | Nil -> raise OutOfBounds
  | Cell x -> x
;;

let set a n x =
  if isOutBounds a n then raise OutOfBounds;
  match a.arr.(n) with
  | Nil -> raise OutOfBounds
  | Cell _ -> a.arr.(n) <- Cell x
;;

let remove a n =
  if isOutBounds a n then raise OutOfBounds;
  for i = n to a.len - 2 do
    a.arr.(i) <- a.arr.(i + 1)
  done;
  a.arr.(a.len - 1) <- Nil;
  a.len <- a.len - 1
;;

let length a = a.len;;

let copy a = {
    arr = Array.copy a.arr;
    len = a.len;
  }
;;

let clear a =
  a.arr <- Array.make baseLen Nil;
  a.len <- 0
;;

let insert a n x =
  if isOutBounds a n then raise OutOfBounds;
  if a.len = (Array.length a.arr) then extend a;
  for i = a.len downto n + 1 do
    a.arr.(i) <- a.arr.(i - 1)
  done;
  a.arr.(n) <- Cell x;
  a.len <- a.len + 1
;;

let map a f =
  let tmp = {
    arr = Array.make a.len Nil;
    len = a.len;
    } in
  for i = 0 to a.len - 1 do
    match a.arr.(i) with
    | Nil -> tmp.arr.(i) <- Nil
    | Cell x -> tmp.arr.(i) <- Cell (f x)
  done;
  tmp
;;

let iter a f =
  for i = 0 to a.len - 1 do
    match a.arr.(i) with
    | Nil -> ()
    | Cell x -> f x
  done
;;

let reverse a =
  let tmp = Array.copy a.arr in
  for i = 0 to a.len - 1 do
    a.arr.(i) <- tmp.(a.len - (1 + i))
  done
;;

let switch a m n =
  if (isOutBounds a m) || (isOutBounds a n) then raise OutOfBounds;
  let c = a.arr.(m) in (
      a.arr.(m) <- a.arr.(n);
      a.arr.(m) <- c
    )
;;

                         
