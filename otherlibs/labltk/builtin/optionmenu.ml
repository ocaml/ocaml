##ifdef CAMLTK

open Protocol;;
(* Implementation of the tk_optionMenu *)

let create ?name parent variable values =
  let w = Widget.new_atom "menubutton" ~parent ?name in
  let mw = Widget.new_atom "menu" ~parent:w ~name:"menu" in
  let res = 
    tkEval [|TkToken "tk_optionMenu";
             TkToken (Widget.name w);
             cCAMLtoTKtextVariable variable;
             TkTokenList (List.map (function x -> TkToken x) values)|] in
  if res <> Widget.name mw then
    raise (TkError "internal error in Optionmenu.create")
  else
    w,mw
;;

let create_named parent name variable values =
  let w = Widget.new_atom "menubutton" ~parent ~name in
  let mw = Widget.new_atom "menu" ~parent:w ~name: "menu" in
  let res = 
    tkEval [|TkToken "tk_optionMenu";
             TkToken (Widget.name w);
             cCAMLtoTKtextVariable variable;
             TkTokenList (List.map (function x -> TkToken x) values)|] in
  if res <> Widget.name mw then
    raise (TkError "internal error in Optionmenu.create")
  else
    w,mw
;;

##else

open Protocol;;
(* Implementation of the tk_optionMenu *)

let create ~parent ~variable ?name values =
  let w = Widget.new_atom "menubutton" ~parent ?name in
  let mw = Widget.new_atom "menu" ~parent:w ~name:"menu" in 
  (* assumes .menu naming *)
  let res = 
    tkEval [|TkToken "tk_optionMenu";
             TkToken (Widget.name w);
             cCAMLtoTKtextVariable variable;
             TkTokenList (List.map ~f:(fun x -> TkToken x) values)|] in
  if res <> Widget.name mw then
    raise (TkError "internal error in Optionmenu.create")
  else
    w, mw
;;

##endif
