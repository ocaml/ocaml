open Protocol
(* Implementation of the tk_optionMenu *)

let create :parent :variable ?:name values =
  let w = Widget.new_atom "menubutton" :parent ?:name in
  let mw = Widget.new_atom "menu" parent:w name:"menu" in 
  (* assumes .menu naming *)
  let res = 
     tkEval [|TkToken "tk_optionMenu";
              TkToken (Widget.name w);
              cCAMLtoTKtextVariable variable;
	      TkTokenList (List.map fun:(fun x -> TkToken x) values)|] in
   if res <> Widget.name mw then
     raise (TkError "internal error in Optionmenu.create")
   else
     w,mw
