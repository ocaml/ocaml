(* $Id$ *)

open Tk

let create_destroyer :parent ?:text{="Ok"} tl =
    Button.create :parent :text command:(fun () -> destroy tl) ()

let add_destroyer ?:text tl =
    let b = create_destroyer tl parent:tl ?:text in
    pack [b] side:`Bottom fill:`X;
    b
