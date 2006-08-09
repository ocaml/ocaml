(* ------------------------------------------------- *
   to build your project, edit this settings, 
   and run `ocaml build.ml'
 * ------------------------------------------------- *)

(* Makefile to use *)
let makefile = "Makefile.ml"

(* Environment options *)
let ocamlrun = "OCAMLRUNPARAM=l=1M CAML_LD_LIBRARY_PATH=../otherlibs/unix ../boot/ocamlrun"
let ocamlc   = ocamlrun ^ " ../ocamlc -nostdlib -I ../stdlib -I ../otherlibs/unix"
let ocamlopt = ocamlrun ^ " ../ocamlopt -nostdlib -I ../stdlib -I ../otherlibs/unix"
let yam      = ocamlrun ^ " ./yam "

(* Compile YaM in native mode ? *)
let opt      = false

(* Arguments to YaM *)
let yam_args = "-verbosity " ^ (try Sys.getenv "VERBOSE" with Not_found -> "0")

(* ------------------------------------------------- *)











open Printf
#load "unix.cma"

let obj    = if opt then "cmx"    else "cmo"
let abj    = if opt then "cmxa"   else "cma"
let ocamlc = if opt then ocamlopt else ocamlc

let mtime f = (Unix.stat f).Unix.st_mtime
let newer f1 f2 = not (Sys.file_exists f2) || mtime f1 > mtime f2 
let command c = 
  printf "%s\n%!" c;
  match Sys.command c with 
    | 0 -> printf "[done], you can now use YaM directly !\n%!"
    | n -> printf "[error], aborting...\n"; exit n
let ocamlc s = kprintf (fun cmd -> command (ocamlc^" "^cmd)) s


(* nettoyage *)
let () = if Array.length Sys.argv > 1 && Sys.argv.(1)="-cleanall" then (
  let safe_remove f = try Sys.remove f with _ -> () in
    ignore (Sys.command (yam ^ "-clean"));
    List.iter safe_remove [
      "Makefile.cmo"; "Makefile.cmx"; "Makefile.o"; "Makefile.cmi";
      "build/YaM.cmo"; "build/YaM.cmx"; "build/YaM.o"; "build/YaM.cmi"; "yam"
    ];
    exit 0
)

(* mise à jour de YaM *)
let () =
  if      newer "build/YaM.mli" "build/YaM.cmi"   then ocamlc "-o yam unix.%s -I build build/YaM.mli build/YaM.ml build/camlp4_config.ml %s" abj     makefile
  else if newer "build/YaM.ml" ("build/YaM."^obj) then ocamlc "-o yam unix.%s -I build build/YaM.ml build/camlp4_config.ml %s" abj     makefile
  else if newer makefile  "yam"       then ocamlc "-o yam unix.%s -I build build/YaM.%s build/camlp4_config.ml %s" abj obj makefile

(* lancement de YaM *)
let cmd = 
  let cmd = ref (yam^yam_args) in
    for i=1 to Array.length Sys.argv -1 do cmd := !cmd^" "^Sys.argv.(i) done;
    !cmd

let ()  = exit (Sys.command cmd)
