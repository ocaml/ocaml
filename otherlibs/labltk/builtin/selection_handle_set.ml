(* The function *must* use tkreturn *)
let handle_set command: cmd =
selection_handle_icccm_optionals (fun opts w ->
tkEval [|TkToken"selection";
	 TkToken"handle";
	 TkTokenList 
      	   (List.map opts fun:(cCAMLtoTKselection_handle_icccm w));
	 cCAMLtoTKwidget w;
	 let id = register_callback w callback:(function args ->
	   let a1 = int_of_string (List.hd args) in
	   let a2 = int_of_string (List.nth args pos:1) in
	   tkreturn (cmd pos:a1 len:a2)) in TkToken ("camlcb "^id)
       |];
  ())

