(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* language encoding using UTF-8 *)
open Tk

let top = opentk () 

(* declare Tk that we use utf-8 to communicate *)
(* problem: Text display is highly dependent on your font installation
   and configulation. The fonts with no-scale setting are selected
   only if the point sizes are exactly same???
*)
let _ = 
  Encoding.system_set "utf-8";
  let l = Label.create top ~text: "???" in
  pack [l];
  let t = Text.create top in
  pack [t];

  let create_hello lang hello =
    let b = Button.create t ~text: lang ~command: (fun () ->
      Label.configure l ~text: hello) 
    in
    Text.window_create t ~index: (`End,[]) ~window: b
  in
  List.iter (fun (lang, hello) -> create_hello lang hello)
    ["Amharic(አማርኛ)", "ሠላም";
     "Arabic", "�����������";
     "Croatian (Hrvatski)", "Bog (Bok), Dobar dan";
     "Czech (česky)",       "Dobrý den";
     "Danish (Dansk)", "Hej, Goddag";
     "English", "Hello";
     "Esperanto", "Saluton";
     "Estonian", "Tere, Tervist";
     "FORTRAN", "PROGRAM";
     "Finnish (Suomi)", "Hei";
     "French (Français)", "Bonjour, Salut";
     "German (Deutsch Nord)", "Guten Tag";
     "German (Deutsch Süd)", "Grüß Gott";
     "Greek (Ελληνικά)", "Γειά σας";
     "Hebrew", "שלום";
     "Italiano", "Ciao, Buon giorno";
     "Maltese", "Ciao";
     "Nederlands, Vlaams", "Hallo, Hoi, Goedendag";
     "Norwegian (Norsk)", "Hei, God dag";
     "Polish", "Cześć!";
     "Russian (Русский)", "Здравствуйте!";
     "Slovak", "Dobrý deň";
     "Spanish (Español)", "¡Hola!";
     "Swedish (Svenska)", "Hej, Goddag";
     "Thai (�������)", "�������, ������";
     "Tigrigna (ትግርኛ)", "ሰላማት";
     "Turkish (Türkçe)", "Merhaba";
     "Vietnamese (Tiếng Việt)", "Chào bạn";
     "Japanese (日本語)", "こんにちは";
     "Chinese (中文,普通话,汉语)", "你好";
     "Cantonese (粵語,廣東話)", "早晨, 你好";
     "Hangul (한글)", "안녕하세요, 안녕하십니까" ]
;;

let _ = Printexc.print mainLoop ()
