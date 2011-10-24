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

type style =
  |User of int
  |Normal
  |Bold  |Bold_off
  |Underline  |Underline_off
  |Inverse  |Inverse_off  
  |Blink_off
  |F_Black  |F_Red  |F_Green  |F_Yellow  |F_Blue  |F_Magenta  |F_Cyan  |F_Gray
  |F_Default
  |G_Black  |G_Red  |G_Green  |G_Yellow  |G_Blue  |G_Magenta  |G_Cyan  |G_Gray
  |G_Default
  |F_Black_B  |F_Red_B  |F_Green_B  |F_Yellow_B  |F_Blue_B  |F_Magenta_B
  |F_Cyan_B  |F_Gray_B  |F_Default_B
  |G_Black_B  |G_Red_B  |G_Green_B  |G_Yellow_B  |G_Blue_B  |G_Magenta_B
  |G_Cyan_B  |G_Gray_B |G_Default_B

let assoc_style =  function
  |User i ->(i,"m");
  (*|Move_up i -> (i,"A");*)
  |Normal->(0,"m");
  |Bold->(1,"m");
  |Bold_off->(22,"m");
  |Underline->(4,"m");
  |Underline_off->(24,"m");
  |Inverse->(7,"m");
  |Inverse_off->(27,"m");
  |Blink_off->(22,"m");
  |F_Black->(30,"m");
  |F_Red->(31,"m");
  |F_Green->(32,"m");
  |F_Yellow->(33,"m");
  |F_Blue->(34,"m");
  |F_Magenta->(35,"m");
  |F_Cyan->(36,"m");
  |F_Gray->(37,"m");
  |F_Default->(39,"m");
  |G_Black->(40,"m");
  |G_Red->(41,"m");
  |G_Green->(42,"m");
  |G_Yellow->(43,"m");
  |G_Blue->(44,"m");
  |G_Magenta->(45,"m");
  |G_Cyan->(46,"m");
  |G_Gray->(47,"m");
  |G_Default->(49,"m");
  |F_Black_B->(90,"m");
  |F_Red_B->(91,"m");
  |F_Green_B->(92,"m");
  |F_Yellow_B->(93,"m");
  |F_Blue_B->(94,"m");
  |F_Magenta_B->(95,"m");
  |F_Cyan_B->(96,"m");
  |F_Gray_B->(97,"m");
  |F_Default_B->(99,"m");
  |G_Black_B->(100,"m");
  |G_Red_B->(101,"m");
  |G_Green_B->(102,"m");
  |G_Yellow_B->(103,"m");
  |G_Blue_B->(104,"m");
  |G_Magenta_B->(105,"m");
  |G_Cyan_B->(106,"m");
  |G_Gray_B->(107,"m");
  |G_Default_B->(109,"m");

module M = Map.Make (String)

let tag_map = 
List.fold_left (fun m (k,e) -> M.add k e m) M.empty [
  ("C.Normal",Normal);
  ("C.Bold",Bold);
  ("C.Bold_off",Bold_off);
  ("C.Underline",Underline);
  ("C.Underline_off",Underline_off);
  ("C.Inverse",Inverse);
  ("C.Inverse_off",Inverse_off);  
  ("C.Blink_off",Blink_off);
  ("C.F_Black",F_Black);
  ("C.F_Red",F_Red);
  ("C.F_Green",F_Green);
  ("C.F_Yellow",F_Yellow);
  ("C.F_Blue",F_Blue);
  ("C.F_Magenta",F_Magenta);
  ("C.F_Cyan",F_Cyan);
  ("C.F_Gray",F_Gray);
  ("C.F_Default",F_Default);
  ("C.G_Black",G_Black);
  ("C.G_Red",G_Red);
  ("C.G_Green",G_Green);
  ("C.G_Yellow",G_Yellow);
  ("C.G_Blue",G_Blue);
  ("C.G_Magenta",G_Magenta);
  ("C.G_Cyan",G_Cyan);
  ("C.G_Gray",G_Gray);
  ("C.G_Default",G_Default);
  ("C.F_Black_B",F_Black_B);
  ("C.F_Red_B",F_Red_B);
  ("C.F_Green_B",F_Green_B);
  ("C.F_Yellow_B",F_Yellow_B);
  ("C.F_Blue_B",F_Blue_B);
  ("C.F_Magenta_B",F_Magenta_B);
  ("C.F_Cyan_B",F_Cyan_B);
  ("C.F_Gray_B",F_Gray_B);
  ("C.G_Black_B",G_Black_B);
  ("C.G_Red_B",G_Red_B);
  ("C.G_Green_B",G_Green_B);
  ("C.G_Yellow_B",G_Yellow_B);
  ("C.G_Blue_B",G_Blue_B);
  ("C.G_Magenta_B",G_Magenta_B);
  ("C.G_Cyan_B",G_Cyan_B);
  ("C.G_Gray_B",G_Gray_B)]


let rreset = ref true
let reset = (:=) rreset

let ddisable = ref true
let disable = (:=) ddisable

let new_stack () = 
  let q = Stack.create () in
    Stack.push "" q;q

let q = new_stack ()

let rec list_pretty char  = function
  |[] -> ""
  | [a] -> a
  | a::l -> Printf.sprintf "%s%c%s" a char (list_pretty char l)

let gen l = 
  (Printf.sprintf "[%sm" 
     (list_pretty ';' 
	(List.map  (fun x -> string_of_int (fst (assoc_style x))) l)))
    
let _sstart q l = 
  let s = gen l in
  Stack.push s q; if !ddisable then "" else s

let sstart = _sstart q
  
let start l = Printf.printf "%s" (_sstart q l)
let format_start l = Format.printf "@<0>%s" (_sstart q l)
let fstartf ff l = Format.fprintf ff "@<0>%s" (_sstart q l)

(*let sstop_gen l = let s = "[m"^(!ddefault)^(gen l) in
if !ddisable then "" else s;;*)

let _sstop q = 
  ignore(Stack.pop q);
  let s = "[m"^(Stack.top q) in if !ddisable then "" else s

let sstop () = _sstop q

let stop () = Printf.printf "%s" (_sstop q)
let format_stop () = Format.printf "@<0>%s" (_sstop q)
let fstopf ff = Format.fprintf ff "@<0>%s" (_sstop q)
  
let add_to_format_tag formatter =
  let q = new_stack () in
  let old_fs = Format.get_formatter_tag_functions () in
  let mark_open_tag s = try 
    _sstart q [(M.find s tag_map)] 
  with Not_found -> old_fs.Format.mark_open_tag s in
  let mark_close_tag s = 
    if M.mem s tag_map then _sstop q else old_fs.Format.mark_close_tag s 
  in
  Format.pp_set_formatter_tag_functions formatter
    {old_fs with Format.mark_open_tag = mark_open_tag; 
       Format.mark_close_tag = mark_close_tag}
    (* From ocamlbuild display.ml *)
    
let get_columns () =
  try
        int_of_string (input_line (Unix.open_process_in  "tput cols"))
  with
    | Failure _ -> 80
	
let set_margin_with_term_width formatter =
  Format.pp_set_margin formatter (get_columns ())
    

(* Just some tools *)
let rec print_list sep print_ele fmt = function
  |[] -> ()
  |[a] -> print_ele fmt a
  |a::l -> 
     Format.fprintf fmt "%a@ %s %a" print_ele a sep (print_list sep print_ele) l
       
(* Pour l'extérieur *)
