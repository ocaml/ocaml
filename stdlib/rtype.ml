let debug = true
;;

(* Run time types *)

type label = string
;;

type rpath =
  | RPident of string * int
  | RPdot of rpath * string * int
  | RPapply of rpath * rpath
;;

type rtype =
  | RTvar of int
  | RTarrow of label * rtype * rtype
  | RTtuple of rtype list
  | RTconstr of rpath * rtype list
;;

(* Debugging purpose printers *)

let catenate_sep sep =
  function 
      [] -> ""
    | x::l -> List.fold_left (fun s s' -> s^sep^s') x l
;;

let string_of_rident (s,i) = 
  if i = 0 then s else s ^ "/" ^ string_of_int i
;;

let rec string_of_rpath = function
  | RPident (s,i) -> string_of_rident (s,i)
  | RPdot (p,s,i) -> string_of_rpath p ^ "." ^ string_of_rident (s,i)
  | RPapply (p1,p2) -> string_of_rpath p1 ^"("^ string_of_rpath p2 ^")"
;;

let string_of_rtype = 
  let rec aux = function
    | RTvar x -> 
	(* it is not ok if x becomes enough bigger, but at this moment
	   we do not mind *)
	"'" ^ (String.make 1 (Char.chr (Char.code 'a' + x)))
    | RTarrow (l,f,t) -> "(" ^ (if l <> "" then l ^ ":" else "") ^ aux f ^ " -> " ^ aux t ^ ")"
    | RTtuple typs -> "(" ^ catenate_sep " * " (List.map aux typs) ^ ")" 
    | RTconstr (rp, typs) ->
	let pstr = string_of_rpath rp in
	if typs <> [] then 
	  "(" ^ catenate_sep ", " (List.map aux typs) ^ ") " ^ pstr
	else pstr
  in
  aux
;;

(* Predicates *)

(* [is_instance rt1 rt2] checks whether the run time type [rt2] is 
   an valid instance of the rn time type [rt1]. *)
let is_instance g t =

  if debug then begin
    prerr_endline "IS INSTANCE CALLED";
    prerr_endline (string_of_rtype g);
    prerr_endline (string_of_rtype t);
  end;

  let rec aux subst g t =
    match g, t with
    | _, RTvar i when i < 0 -> [] (* _ i.e. "any" type pattern *)
    | RTvar i, _ ->
  	begin 
  	  try
  	    if List.assoc i subst = t then subst else raise Exit
  	  with
  	    Not_found -> (i, t) :: subst
  	end
    | RTarrow (l1, f1, t1), RTarrow (l2, f2, t2) when l1 = l2 ->
  	let newsubst = aux subst f1 f2 in
  	aux newsubst t1 t2
    | RTtuple tl1, RTtuple tl2 when List.length tl1 = List.length tl2 -> 
  	List.fold_left2 (fun st t1 t2 -> aux st t1 t2) subst tl1 tl2
    | RTconstr (rp1, tl1), RTconstr (rp2, tl2) ->
	if rp1 <> rp2 then raise Exit;
	if List.length tl1 <> List.length tl2 then raise Exit;
  	List.fold_left2 (fun st t1 t2 -> aux st t1 t2) subst tl1 tl2
    | _ -> raise Exit
  in
  try
    ignore (aux [] g t); 

    if debug then begin
      prerr_endline "TRUE";
    end;

    true
  with
    Exit -> 

      if debug then begin
	prerr_endline "FALSE";
      end;

      false

(* compilation for dynamic values *)

let dynamic_comp =
  fun [|ty|] v -> 
    prerr_endline "dynamic_comp is called";
    prerr_endline ("type " ^ string_of_rtype ty);
    match ty with
    | RTconstr (RPident ("dyn",11), []) -> (Obj.magic v : 'a * rtype)
    | _ -> (v, ty : 'a * rtype)

exception Type_match_failure of rtype * string * int * int
let fail t m s e = raise (Type_match_failure ( t, m, s, e ))

let coerce_comp = fun (m,ls,le) [|ty1|] ((v, ty2) as d) ->
  match ty1 with
    RTconstr (RPident ("dyn",11), []) -> (Obj.magic d : 'a)
  | _ ->
      if is_instance ty2 ty1 then (v : 'a)
      else fail ty2 m ls le
