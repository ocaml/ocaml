open Rtype;;
open Gcaml;;

let t_dest_ ty v =
  match ty with
  | { desc= Ttuple ts } -> 
      let rec f pos = function
	| [] -> typeof (), ()
	| t::ts -> 
	    let t', v' = f (pos+1) ts in
	    { desc= Ttuple [t; t'] }, Obj.magic (Obj.field v pos, v')
      in
      (Obj.magic (f 0 ts) : dyn)
  | _ -> raise Exit
;;

generic val t_dest : {'a} => 'a -> dyn = t_dest_;;

let t_cnst_ ty v =
  let rec make_tuple_ty = function
    | [: unit :] -> 0, []
    | { desc= Ttuple [t; ts] } -> 
	let size, ts' = make_tuple_ty ts in
	size + 1, t :: ts'
    | _ -> assert false
  in
  let size, ty = make_tuple_ty ty in
  let o = Obj.new_block 0 size in
  let rec f pos v = 
    let v = Obj.magic v in
    Obj.set_field o pos (Obj.repr (fst v));
    f (pos + 1) (Obj.magic (snd v))
  in
  (Obj.magic (ty, f 0 v) : dyn)
;;

generic val t_cnst : {'a} => 'a -> dyn = t_cnst_;;
let t_cnst_ ty v =
  let rec make_tuple_ty = function
    | [: unit :] -> 0, []
    | { desc= Ttuple [t; ts] } -> 
	let size, ts' = make_tuple_ty ts in
	size + 1, t :: ts'
    | _ -> assert false
  in
  let size, tys = make_tuple_ty ty in
  let ty = { desc= Ttuple tys } in
  let o = Obj.new_block 0 size in
  let rec f pos v = 
    if pos = size then () else
    let v = Obj.magic v in
    Obj.set_field o pos (Obj.repr (fst v));
    f (pos + 1) (Obj.magic (snd v))
  in
  f 0 v; 
  (Obj.magic (ty, o) : dyn)
;;

generic val t_cnst : {'a} => 'a -> dyn = t_cnst_;;

let _ = (coerce (t_cnst ((coerce (t_dest (1,2,3))) : int * (int * (int * unit)))) : int * int * int);;

(* 
   <..., 1>      n, (...n..., 1)
   <..., 1,2>    n, (...n..., 1, 2)
   
   let x = <..., 1,2> in   (* <...n..., 1,2>  :  <..., int, int> *)  ---> <..., int, int, int, int, int> 
   let y = 0 <::> x in     (* <...(n-1)..., 0, 1, 2>   :  <..., int, int, int> *)
   let z = <..., 3,4> in   (* <...m..., 3,4>  :  <..., int, int> *)   
   y <@> z                 (* <...n..., 0,1,2,3,4> :  <..., int, int, int, int, int> *)  (* => m + 3 = n *) 

   let f x = x <@> <..., 1, 2>            'a -> <... as 'a, int, int>  ???

   int <::> <'a> => fun (x <::> y) -> 
                      x : int
                      y : <'a>
                      ...
                      x <::> y    :  int <::> <'a>   == <int :: 'a>
*)

