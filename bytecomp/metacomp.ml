open Asttypes
open Lambda
open Obj

exception Unsupported

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

