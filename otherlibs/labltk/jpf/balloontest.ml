open Tk
open Widget
open Balloon
open Protocol

let _ =
let t = openTk () in
Balloon.init ();
let b = Button.create parent: t text: "hello" in
Button.configure b command: (fun () -> destroy b);
pack [b];
Balloon.put on: b ms: 1000 "Balloon";
Printexc.catch mainLoop ()
 
