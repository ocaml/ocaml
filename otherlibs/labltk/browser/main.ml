(* $Id$ *)

open Tk

let _ =
  let path = ref [] in
  Arg.parse
    keywords:[ "-I", Arg.String (fun s -> path := s :: !path),
	       "<dir>  Add <dir> to the list of include directories" ]
    others:(fun name -> raise(Arg.Bad("don't know what to do with " ^ name)))
    errmsg:"lablbrowser :";
  Config.load_path := List.rev !path @ [Config.standard_library];
  begin
    try Searchid.start_env := Env.open_pers_signature "Pervasives" Env.initial
    with Env.Error _ -> ()
  end;
  
  Searchpos.view_defined_ref := Viewer.view_defined;
  Searchpos.editor_ref.contents <- Editor.f;

  let top = openTkClass "LablBrowser" in
  Jg_config.init ();

  bind top events:[[], `Destroy] action:(`Set ([], fun _ -> exit 0));
  at_exit Shell.kill_all;
  

  Viewer.f on:top ();

  while true do
    try
      Printexc.print mainLoop ()
    with Protocol.TkError _ -> ()
  done
