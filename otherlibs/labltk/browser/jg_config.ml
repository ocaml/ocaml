(* $Id$ *)

let init () =
  let font =
    let font =
      Option.get Widget.default_toplevel name:"variableFont" class:"Font" in
    if font = "" then "variable" else font
  in
  List.iter ["Button"; "Label"; "Menu"; "Menubutton"; "Radiobutton"]
    fun:(fun cl -> Option.add ("*" ^ cl ^ ".font") value:font);
  Option.add "*Button.padY" value:"0" priority:`StartupFile;
  Option.add "*Text.highlightThickness" value:"0" priority:`StartupFile;
  Option.add "*interface.background" value:"gray85" priority:`StartupFile;
  let foreground =
    Option.get Widget.default_toplevel
      name:"disabledForeground" class:"Foreground" in
  if foreground = "" then
    Option.add "*disabledForeground" value:"black"
