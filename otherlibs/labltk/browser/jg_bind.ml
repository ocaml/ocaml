(* $Id$ *)

open Tk

let enter_focus w = 
  bind w events:[[], `Enter] action:(`Set ([], fun _ -> Focus.set w))

let escape_destroy ?destroy:tl w =
  let tl = match tl with Some w -> w | None -> w in
  bind w events:[[], `KeyPressDetail "Escape"]
      	 action:(`Set ([], fun _ -> destroy tl))

let return_invoke w :button =
  bind w events:[[], `KeyPressDetail "Return"]
      	 action:(`Set ([], fun _ -> Button.invoke button))
