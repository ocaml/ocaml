(* $Id$ *)

open Tk

let create :parent ?:command ?:width ?:textvariable () =
  let ew = Entry.create :parent ?:width ?:textvariable () in
  Jg_bind.enter_focus ew;
  begin match command with Some command ->
    bind ew events:[[], `KeyPressDetail "Return"]
      action:(`Set ([], fun _ -> command (Entry.get ew)))
  | None -> ()
  end;
  ew
