let debug = true
;;

(* Run time types *)
(* do not change these definitions without care!
   Their compilation code is in Transltype. *)

type label = string
;;

type run_ident =
  | Ride_ident of string * int
  | Ride_dot of run_ident * string * int
  | Ride_apply of run_ident * run_ident
;;

type 'a val_type =
  | Rtyp_var of int
  | Rtyp_arrow of label * 'a val_type * 'a val_type
  | Rtyp_tuple of 'a val_type list
  | Rtyp_constr of 'a * 'a val_type list
;;

type run_type = (run_ident * string) val_type
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

let rec string_of_run_ident = function
  | Ride_ident (s,i) -> string_of_rident (s,i)
  | Ride_dot (p,s,i) -> string_of_run_ident p ^ "." ^ string_of_rident (s,i)
  | Ride_apply (p1,p2) -> string_of_run_ident p1 ^"("^ string_of_run_ident p2 ^")"
;;

let string_of_val_type f = 
  let rec aux = function
    | Rtyp_var x -> 
	(* it is not ok if x becomes enough bigger, but at this moment
	   we do not mind *)
	"'" ^ (String.make 1 (Char.chr (Char.code 'a' + x)))
    | Rtyp_arrow (l,f,t) -> "(" ^ (if l <> "" then l ^ ":" else "") ^ aux f ^ " -> " ^ aux t ^ ")"
    | Rtyp_tuple typs -> "(" ^ catenate_sep " * " (List.map aux typs) ^ ")" 
    | Rtyp_constr (p, typs) ->
	let pstr = f p in
	if typs <> [] then 
	  "(" ^ catenate_sep ", " (List.map aux typs) ^ ") " ^ pstr
	else pstr
  in
  aux
;;

let string_of_digest s =
  let s' = String.copy s in
  for i = 0 to String.length s' - 1 do
    let c = 
      let c = Char.code s'.[i] in
      if c = Char.code '_' ||
         (c >= Char.code '0' && c <= Char.code '9') ||
         (c >= Char.code 'A' && c <= Char.code 'Z') ||
	 (c >= Char.code 'a' && c <= Char.code 'z') then Char.chr c
      else begin
	let x = Char.code s'.[i] mod 62 in
	if x < 10 then Char.chr (Char.code '0' + x)
	else if x < 36 then Char.chr (Char.code 'A' + (x-10))
	else Char.chr (Char.code 'a' + (x-36))
      end
    in
    s'.[i] <- c
  done;
  s'
;;

let string_of_run_type = 
  string_of_val_type (fun (p,d) -> 
    string_of_run_ident p ^ "[" ^ string_of_digest d ^ "]" );;

(* Predicates *)

(* [is_instance rt1 rt2] checks whether the run time type [rt2] is 
   an valid instance of the rn time type [rt1]. *)
let is_instance comppath g t =

  if debug then begin
    prerr_endline "IS INSTANCE CALLED";
  end;

  let rec aux subst g t =
    match g, t with
    | _, Rtyp_var i when i < 0 -> [] (* _ i.e. "any" type pattern *)
    | Rtyp_var i, _ ->
  	begin 
  	  try
  	    if List.assoc i subst = t then subst else raise Exit
  	  with
  	    Not_found -> (i, t) :: subst
  	end
    | Rtyp_arrow (l1, f1, t1), Rtyp_arrow (l2, f2, t2) when l1 = l2 ->
  	let newsubst = aux subst f1 f2 in
  	aux newsubst t1 t2
    | Rtyp_tuple tl1, Rtyp_tuple tl2 when List.length tl1 = List.length tl2 -> 
  	List.fold_left2 (fun st t1 t2 -> aux st t1 t2) subst tl1 tl2
    | Rtyp_constr (rp1, tl1), Rtyp_constr (rp2, tl2) ->
	if not (comppath rp1 rp2) then raise Exit;
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
    (* The run time type is given as an array [|ty|], so that
       we could be make the construct [dyn e] be a normal function
       using the generic flow compilation future.
     *)
    prerr_endline ("DYNAMIC type = " ^ string_of_run_type ty);
    (v, ty : 'a * run_type)
;;

exception Type_match_failure of run_type * run_type * string * int * int
let fail t t' m s e = raise (Type_match_failure ( t, t', m, s, e ))
let fail t t' m s e = 
  prerr_endline
    (Printf.sprintf "Run time type match failure:\nRun time type %s\nExpected type %s\nat file `%s' from char %d to %d" (string_of_run_type t)
       (string_of_run_type t') m s e);
  raise (Type_match_failure ( t, t', m, s, e ))
;;    

let coerce_comp = fun (m,ls,le) [|ty1|] ((v, ty2) as d) ->
  if is_instance (=) ty2 ty1 then (v : 'a)
  else fail ty2 ty1 m ls le

let import_comp = fun (m,ls,le) [|ty1|] ((v, ty2) as d) ->
  if is_instance (fun rp1 rp2 -> snd rp1 = snd rp2) ty2 ty1 then (v : 'a)
  else fail ty2 ty1 m ls le

