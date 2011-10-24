(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

open Num
open Format
open Options

type borne = Strict of num | Large of num | Pinfty | Minfty

type t = { 
  ints : (borne * borne) list;
  is_int : bool
}

exception EmptyInterval
exception NotConsistent
exception Not_a_float

let print_borne fmt = function
  | Minfty -> fprintf fmt "-inf" 
  | Pinfty -> fprintf fmt "+inf"
  | Strict v | Large v -> fprintf fmt "%s" (string_of_num v)
      
let print_interval fmt (b1,b2) =
  let c1, c2 = match b1, b2 with
    | Large _, Large _ -> '[', ']'
    | Large _, _ -> '[', '['
    | _, Large _ -> ']', ']'
    | _, _ -> ']', '['
  in 	    
  fprintf fmt "%c%a;%a%c" c1 print_borne b1 print_borne b2 c2
    
let print fmt {ints=ints; is_int = b} = 
  List.iter (fun i -> fprintf fmt "%a " print_interval i) ints
  

let undefined ty = {
  ints = [Minfty, Pinfty];
  is_int =  ty  = Ty.Tint; 
}

let point b ty = {
  ints = [Large b, Large b]; 
  is_int = ty  = Ty.Tint 
}

let borne_of k n = if k then Large n else Strict n

let is_point {ints = l} =
  match l with
    | [Large v1, Large v2] when v1 =/ v2 -> Some v1
    | _ -> None

let check_one_interval b1 b2 is_int =
    match b1, b2 with
      | Pinfty, _ | _, Minfty  -> raise EmptyInterval
      | (Strict v1 | Large v1), (Strict v2 | Large v2) ->
	  let c = compare_num v1 v2 in 
	  if c > 0 then raise EmptyInterval;
	  if c = 0 then begin
	    match b1, b2 with
	      | Large _, Large _ when not is_int || is_integer_num v1 ->
		  ()
	      | _ -> raise EmptyInterval
	  end
      | _ -> ()

let min_borne b1 b2 = 
  match b1, b2 with
    | Minfty , _ | _ , Minfty -> Minfty
    | b , Pinfty | Pinfty, b -> b
    | (Strict v1 | Large v1) , (Strict v2 | Large v2) -> 
	let c = compare_num v1 v2 in
	if c < 0 then b1
	else if c > 0 then b2
	else match b1, b2 with 
	  | (Strict _ as b) , _ | _, (Strict _ as b) -> b
	  | _, _ -> b1
 
let max_borne b1 b2 = 
  match b1, b2 with
    | Pinfty , _ | _ , Pinfty -> Pinfty
    | b , Minfty | Minfty, b -> b
    | (Strict v1 | Large v1) , (Strict v2 | Large v2) -> 
	let c = compare_num v1 v2 in
	if c > 0 then b1
	else if c < 0 then b2
	else match b1, b2 with 
	  | (Strict _ as b) , _ | _, (Strict _ as b) -> b
	  | _, _ -> b1

let compare_bornes b1 b2 =
  match b1, b2 with
    | Minfty, Minfty | Pinfty, Pinfty -> 0
    | Minfty, _ | _, Pinfty -> -1
    | Pinfty, _ | _, Minfty -> 1
    | Strict v1, Strict v2 | Large v1, Large v2 
    | Strict v1, Large v2 | Large v1, Strict v2 -> compare_num v1 v2
	
let pos_borne b1 = compare_bornes b1 (borne_of true (Int 0)) >= 0
  
let neg_borne b1 = compare_bornes b1 (borne_of true (Int 0)) <= 0

let rec union_bornes l =
  match l with
    | [] | [_] -> l
    | (l1, u1)::((l2, u2)::r as r2) ->
	if compare_bornes u1 l2 < 0 then
	  (l1, u1)::(union_bornes r2)
	else if compare_bornes u1 u2 > 0 then
	  union_bornes ((l1, u1)::r)
	else
	  union_bornes ((l1, u2)::r)

let union ({ints = l} as uints) =
  let l = List.sort (fun (l1, _) (l2, _) -> compare_bornes l1 l2) l in
  { uints with ints = union_bornes l }

let add_borne b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> assert false
    | Minfty, _ | _, Minfty -> Minfty
    | Pinfty, _ | _, Pinfty -> Pinfty
    | Large v1, Large v2 -> Large (v1 +/ v2)
    | (Large v1 | Strict v1), (Large v2 | Strict v2) -> Strict (v1 +/ v2)

let add_interval l (b1,b2) =
  List.fold_right
    (fun (b1', b2') l ->
       let l1 = ((add_borne b1 b1'),(add_borne b2 b2'))::l in
       union_bornes (l1)
    ) l []

let add {ints = l1; is_int = is_int} {ints = l2} =
  let l = 
    List.fold_left
      (fun l bs -> let i = add_interval l1 bs in i@l) [] l2 
  in
  union { ints = l ; is_int = is_int }

let minus_borne = function
  | Minfty -> Pinfty
  | Pinfty -> Minfty
  | Large v -> Large (minus_num v)
  | Strict v -> Strict (minus_num v)

let scale_borne n b =
  assert (n >=/ Int 0);
  if n =/ Int 0 then Large (Int 0)
  else match b with
    | Pinfty | Minfty -> b
    | Large v -> Large (n */ v)
    | Strict v -> Strict (n */ v)

let scale_interval n (b1,b2) =
  if n </ Int 0 then
    (minus_borne (scale_borne (minus_num n) b2),
     minus_borne (scale_borne (minus_num n) b1))
  else (scale_borne n b1, scale_borne n b2)

let scale n uints =
  let l = List.map (scale_interval n) uints.ints in
  union { uints with ints = l }
	    
let mult_borne b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> assert false
    | Minfty, b | b, Minfty ->
	if compare_bornes b (borne_of true (Int 0)) = 0 then b
	else if pos_borne b then Minfty
	else Pinfty
    | Pinfty, b | b, Pinfty ->
	if compare_bornes b (borne_of true (Int 0)) = 0 then b
	else if pos_borne b then Pinfty
	else Minfty
    | Strict v1, Strict v2 | Strict v1, Large v2
    | Large v1, Strict v2 -> Strict (v1 */ v2)
    | Large v1, Large v2 -> Large (v1 */ v2)

let mult_borne_inf b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> Minfty
    | _, _ -> mult_borne b1 b2

let mult_borne_sup b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> Pinfty
    | _, _ -> mult_borne b1 b2

let mult_bornes (l1,u1) (l2,u2) =
  if pos_borne l1 && pos_borne l2 then
    (mult_borne_inf l1 l2, mult_borne_sup u1 u2)
  else if neg_borne u1 && neg_borne u2 then
    (mult_borne_inf u1 u2, mult_borne_sup l1 l2)
  else if pos_borne l2  && neg_borne l1 && pos_borne u1 then
    (mult_borne_inf l1 u2, mult_borne_sup u1 u2)
  else if pos_borne l1  && neg_borne l2 && pos_borne u2 then
    (mult_borne_inf l2 u1, mult_borne_sup u2 u1)
  else if neg_borne u2  && neg_borne l1 && pos_borne u1 then
    (mult_borne_inf u1 l2, mult_borne_sup l1 l2)
  else if neg_borne u1  && neg_borne l2 && pos_borne u2 then
    (mult_borne_inf u2 l1, mult_borne_sup l2 l1)
  else
    let b1i = mult_borne_inf l1 u2 in
    let b2i = mult_borne_inf u1 l2 in
    let b1s = mult_borne_sup l1 l2 in
    let b2s = mult_borne_sup u1 u2 in
    (min_borne b1i b2i, max_borne b1s b2s)
      
let rec power_borne_inf p b =
  match p with
    | 1 -> b
    | p -> mult_borne_inf b (power_borne_inf (p-1) b)

let rec power_borne_sup p b =
  match p with
    | 1 -> b
    | p -> mult_borne_sup b (power_borne_sup (p-1) b)

let power_bornes p (b1,b2) =
  if neg_borne b1 && pos_borne b2 then
    match p with
      | 0 -> assert false
      | p when p mod 2 = 0 -> 
	  let m = max_borne (power_borne_sup p b1) (power_borne_sup p b2) in
	  (Large (Int 0), m)
      | _ -> (power_borne_inf p b1, power_borne_sup p b2)
  else if pos_borne b1 && pos_borne b2 then
    (power_borne_inf p b1, power_borne_sup p b2)
  else if neg_borne b1 && neg_borne b2 then
    match p with
      | 0 -> assert false
      | p when p mod 2 = 0 -> (power_borne_inf p b2, power_borne_sup p b1)
      | _ -> (power_borne_inf p b1, power_borne_sup p b2)
  else assert false
    
let intersect2 (l1, u1) (l2, u2) = (max_borne l1 l2, min_borne u1 u2)

let int_of_borne_inf b =
  match b with
    | Minfty | Pinfty -> b
    | Large v -> Large (ceiling_num v)
    | Strict v ->
	let v' = ceiling_num v in
	if v' >/ v then Large v' else Large (v +/ (Int 1)) 

let int_of_borne_sup b =
  match b with
    | Minfty | Pinfty -> b
    | Large v -> Large (floor_num v)
    | Strict v ->
	let v' = floor_num v in
	if v' </ v then Large v' else Large (v -/ (Int 1)) 

let int_bornes l u = 
  int_of_borne_inf l, int_of_borne_sup u

let intersect_bornes (b1, b2) { ints = l; is_int = is_int } =
  let l = List.map (intersect2 (b1, b2)) l in
  let l = 
    List.fold_right
      (fun (l, u) l' -> try
	 let l,u = if is_int then int_bornes l u else l,u in
	 check_one_interval l u is_int;
	 (l, u)::l'
       with EmptyInterval -> l'
      ) l [] 
  in
  let l = union_bornes l in
  if l = [] then raise NotConsistent else { ints = l; is_int = is_int }

let new_borne_sup b ~is_le uints =
  intersect_bornes (Minfty, (borne_of is_le b)) uints

let new_borne_inf b ~is_le uints =
  intersect_bornes ((borne_of is_le b), Pinfty) uints

let intersect ({ints=l1} as uints1) ({ints=l2} as uints2) =
  let u =
    List.fold_left
      (fun u' bs ->
	 let ui = try intersect_bornes bs uints2
	 with NotConsistent -> {ints = []; is_int = uints2.is_int} in
	 { u' with ints = (u'.ints)@(ui.ints) }
      ) {ints = []; is_int = uints1.is_int} uints1.ints in
  let u = union u in
  if u.ints = [] then (raise NotConsistent) else u

let exclude_bornes ui (b1,b2) =
  let bu = match b1 with
    | Strict v -> Large v
    | Large v -> Strict v
    | _ -> b1 in
  let bl = match b2 with
    | Strict v -> Large v
    | Large v -> Strict v
    | _ -> b2 in
  let u1 = 
    try intersect_bornes (Minfty, bu) ui
    with NotConsistent -> { ui with ints = [] } 
  in
  let u2 = 
    try intersect_bornes (bl, Pinfty) ui
    with NotConsistent -> { ui with ints = [] } 
  in
  let u = {ui with ints = u1.ints@u2.ints} in
  if u.ints = [] then raise NotConsistent else u
    
let exclude uints1 uints2 =
  union (List.fold_left exclude_bornes uints2 uints1.ints)

let mult u1 u2 =
  let resl = 
    List.fold_left
      (fun l' (u,l) -> (List.map (mult_bornes (u,l)) u2.ints)@l') 
      [] u1.ints 
  in
  union { ints=resl; is_int = u1.is_int }

let power n u =
  let l = List.map (power_bornes n) u.ints in
  union { u with ints = l }


let num_of_float x =
  if x = infinity or x = neg_infinity then raise Not_a_float;
  let (f, n) = frexp x in
  let z =
    Big_int.big_int_of_string
      (Int64.to_string (Int64.of_float (f *. 2. ** 52.))) in
  (*
    Si on a ocaml 3.11 on peut mettre (mieux) :
    let z =
      Big_int.big_int_of_int64
        (Int64.of_float (f *. 2. ** 52.)) in
  *)
  let factor = (Int 2) **/ (Int (n - 52)) in
  (Big_int z) */ factor

let root_num a n = 
  if a </ (Int 0) then assert false
  else if a =/ (Int 0) then (Int 0)
  else if n = 2 then num_of_float (sqrt (float_of_num a))
  else num_of_float ((float_of_num a) ** (1./. (float n)))

let root_default_num a n =
  let s = root_num a n in
  let d = a -/ (s **/ (Int n)) in
  if d >=/ (Int 0) then s else a // (s **/ ((Int n) -/ (Int 1)))

let root_exces_num a n =
  let s = root_num a n in
  let d = a -/ (s **/ (Int n)) in
  if d <=/ (Int 0) then s else a // (s **/ ((Int n) -/ (Int 1)))

let root_default_borne is_int x n =
  match x with
    | Pinfty -> Pinfty
    | Minfty -> Minfty
    | Large v | Strict v ->
	let s = if v >=/ (Int 0) then root_default_num v n
	else (minus_num (root_exces_num (minus_num v) n)) in
	if is_int then
	  let cs = ceiling_num s in
	  let cs2 = cs **/ (Int n) in
	  if v <=/ cs2 then Large cs
	  else Large (cs +/ (Int 1))
	else Large s

let root_exces_borne is_int x n =
  match x with
    | Pinfty -> Pinfty
    | Minfty -> Minfty
    | Large v | Strict v ->
	let s = if v >=/ (Int 0) then root_exces_num v n
	else (minus_num (root_default_num (minus_num v) n)) in
	if is_int then
	  let cs = floor_num s in
	  let cs2 = cs **/ (Int n) in
	  if v >=/ cs2 then Large cs
	  else Large (cs -/ (Int 1))
	else Large s

let sqrt_interval is_int (b1,b2) =
  let l1, u1 = (minus_borne (root_exces_borne is_int b2 2),
		minus_borne (root_default_borne is_int b1 2)) in
  let l2, u2 = (root_default_borne is_int b1 2,
		root_exces_borne is_int b2 2) in
  if compare_bornes l1 u1 > 0 then
    if compare_bornes l2 u2 > 0 then []
    else [l2,u2]
  else if compare_bornes l2 u2 > 0 then [l1, u1]
  else  union_bornes [(l1,u1); (l2, u2)]

let root_interval is_int (b1,b2) n =
  let u,l = (root_default_borne is_int b1 n, root_exces_borne is_int b2 n) in
  if compare_bornes u l > 0 then [] else [u,l]

let sqrt {ints = l; is_int = is_int } =
  let l =
    List.fold_left
      (fun l' bs ->
	 (sqrt_interval is_int bs)@l'
      ) [] l in
  union { ints = l; is_int = is_int }

let root n {ints = l; is_int = is_int} =
  let l =
    List.fold_left
      (fun l' bs ->
	 (root_interval is_int bs n)@l'
      ) [] l in
  union { ints = l; is_int = is_int }

let finite_size {ints = l; is_int = is_int} =
  if (not is_int) then None
  else
    try
      let n =
	List.fold_left
	  (fun n (b1,b2) ->
	     match b1, b2 with
	       | Minfty, _ | _, Pinfty -> raise Exit
	       | Large v1, Large v2 -> n +/ (v2 -/ v1 +/ (Int 1))
	       | _, _ -> assert false
	  ) (Int 0) l in
      Some n
    with Exit -> None
		 
let borne_inf = function
  | {ints = (Large v, _)::_} -> v
  | _ -> invalid_arg "Intervals.borne_inf : No finite borne inf"
