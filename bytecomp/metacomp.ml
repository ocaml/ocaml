open Asttypes
open Lambda
open Obj

exception Unsupported

exception Not_constant
let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

(*
let rec transl_constant v =
  if Obj.is_int v then
    Const_base (Const_int (Obj.obj v))
  else if Obj.is_block v then 
    let tag = Obj.tag v in
    if tag >= no_scan_tag then 
      if tag = string_tag then 
	Const_base (Const_string (Obj.obj v))
      else if tag = double_tag then
	Const_base (Const_float (Obj.obj v))
      else raise Unsupported
    else
      let len = Obj.size v in
      let rec transl_args pos =
	if pos = len then []
	else 
	  transl_constant (Obj.field v pos) :: transl_args (pos+1)
      in
      Const_block (tag, transl_args 0)
  else raise Unsupported
*)

let scan v =
  let blocks = ref [] in
  let rec scan v =
    if Obj.is_int v then ()
    else if Obj.is_block v then 
      try 
        let numref = List.assq v !blocks in
        incr numref
      with
      | Not_found ->
  	blocks := (v, ref 1) :: !blocks;
  	let tag = Obj.tag v in
  	if tag >= no_scan_tag then 
  	  if tag = string_tag then ()
  	  else if tag = double_tag then ()
  	  else raise Unsupported
  	else
  	  let len = Obj.size v in
  	  let rec scan_args pos =
  	    if pos = len then ()
  	    else begin
  	      scan (Obj.field v pos);
  	      scan_args (pos+1)
  	    end
  	  in
  	  scan_args 0
    else raise Unsupported
  in
  scan v;
  !blocks

(* Only for immutable constant without functions *)
let transl_constant overrides v =
  let shared_blocks = 
    List.map (fun (v,_) -> v, Ident.create "x")
      (List.filter (fun (v,numref) -> !numref > 1) (scan v))
  in
  let rec transl share v =
    if Obj.is_int v then
      Lconst (Const_base (Const_int (Obj.obj v)))
    else if Obj.is_block v then 
      try List.assq v overrides with Not_found ->
      try 
	if share then Lvar (List.assq v shared_blocks) 
	else raise Not_found
      with Not_found ->
	let tag = Obj.tag v in
	if tag >= no_scan_tag then 
	  if tag = string_tag then 
	    Lconst (Const_base (Const_string (Obj.obj v)))
	  else if tag = double_tag then
	    Lconst (Const_base (Const_float (Obj.obj v)))
	  else raise Unsupported
	else
	  let len = Obj.size v in
	  let rec transl_args pos =
	    if pos = len then []
	    else transl true (Obj.field v pos) :: transl_args (pos+1)
	  in
	  let args = transl_args 0 in
	  try
	    Lconst (Const_block (tag, List.map extract_constant args))
	  with
	  | Not_constant ->
	      Lprim(Pmakeblock(tag, Immutable), args)
    else raise Unsupported
  in
  let defs = List.map (fun (v,id) -> id, transl false v) shared_blocks in
  let body = transl false v in
  match defs with
  | [] -> body
  | _ -> Lletrec (defs, body)
