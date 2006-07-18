(*
 *
 * Copyright (C) 2003-2004 Damien Pous
 * 
 * YaM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * YaM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with YaM; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *)

open Printf


let print_deps   = ref false
let print_cmds   = ref true
let debug_status = ref false
let debug_deps   = ref false
let debug_build  = ref false


(* ---- Définition des unités de compilation ---- *)

(* environnement / options *)
type options_t = {
  ocaml:              string ref;
  ocamlc:             string ref;
  ocamlopt:           string ref;
  ocamldep:           string ref;
  ocamldoc:           string ref;
  ocamlyacc:          string ref;
  ocamllex:           string ref;
  ocamlglade:         string ref;
  ocaml_P4:           string ref;
  ocaml_P4_opt:       string ref;
  ocaml_Flags:        string ref;
  ocaml_OptFlags:     string ref;
  ocaml_ByteFlags:    string ref;
  ocaml_LinkFlags:    string ref;
  ocaml_ForPack:      string ref;
  ocaml_Includes:     string list ref;
  ocaml_ExtIncludes:  string list ref;
  ocaml_ExtLibraries: string list ref;
}


(* options par défaut *)
let getenv n d = try Sys.getenv n with Not_found -> d
let options = ref {
  ocaml              = ref (getenv "OCAML"      "ocaml");
  ocamlc             = ref (getenv "OCAMLC"     "ocamlc.opt");
  ocamlopt           = ref (getenv "OCAMLOPT"   "ocamlopt.opt");
  ocamldep           = ref (getenv "OCAMLDEP"   "ocamldep.opt");
  ocamldoc           = ref (getenv "OCAMLDOC"   "ocamldoc.opt");
  ocamlyacc          = ref (getenv "OCAMLYACC"  "ocamlyacc");
  ocamllex           = ref (getenv "OCAMLLEX"   "ocamllex.opt");
  ocamlglade         = ref (getenv "OCAMLGLADE" "lablgladecc2 -hide-default");
  ocaml_P4           = ref "";
  ocaml_P4_opt       = ref "";
  ocaml_Flags        = ref "";
  ocaml_OptFlags     = ref "";
  ocaml_ByteFlags    = ref "";
  ocaml_LinkFlags    = ref "";
  ocaml_ForPack      = ref "";
  ocaml_Includes     = ref [];
  ocaml_ExtIncludes  = ref [];
  ocaml_ExtLibraries = ref [];
}
let dir = ref ""

(* calcul d'une valeur dans un nouvel environnement (options) *)
let new_scope v = 
  let options' = !options in
    options := {
      ocaml              = ref !(options'.ocaml);
      ocamlc             = ref !(options'.ocamlc);
      ocamlopt           = ref !(options'.ocamlopt);
      ocamldep           = ref !(options'.ocamldep);
      ocamldoc           = ref !(options'.ocamldoc);
      ocamlyacc          = ref !(options'.ocamlyacc);
      ocamllex           = ref !(options'.ocamllex);
      ocamlglade         = ref !(options'.ocamlglade);
      ocaml_P4           = ref !(options'.ocaml_P4);
      ocaml_P4_opt       = ref !(options'.ocaml_P4_opt);
      ocaml_Flags        = ref !(options'.ocaml_Flags);
      ocaml_OptFlags     = ref !(options'.ocaml_OptFlags);
      ocaml_ByteFlags    = ref !(options'.ocaml_ByteFlags);
      ocaml_LinkFlags    = ref !(options'.ocaml_LinkFlags);
      ocaml_ForPack      = ref !(options'.ocaml_ForPack);
      ocaml_Includes     = ref !(options'.ocaml_Includes);
      ocaml_ExtIncludes  = ref !(options'.ocaml_ExtIncludes);
      ocaml_ExtLibraries = ref !(options'.ocaml_ExtLibraries);
    }; 
    let v' = Lazy.force v in
      options := options';
      v'


(* type (interne) des unités *)
type unit_t = {

  name: string;

  (* ensembles de fichiers : *)
  sources:      string list; (* sources *)
  targets:      string list; (* générés ET ciblés *)
  pregenerated: string list; (* à créer avant de calculer des dépendances *)
  trash:        string list; (* générables "à nettoyer" *)

  (* cibles automatiques (quand aucune cible n'est spécifiée) *)
  auto_targets: string list;

  (* éventuelles sous-unités *)
  sub_units:    unit_t list;

  (* objet généré (à lier) *)
  objects: (string*string) option; (* natif / bytecode *)

  (* dépendances d'une cible f *)
  dependencies: native: bool -> string -> string list;

  (* fichiers dont dépendent le résultat précédent *)
  dep_files: string -> string list;

  (* commande pour la compilation d'une cible f
   * renvoie (cmd, out) où
   *  - cmd est la commande à exécuter
   *  - out est l'ensemble des fichiers générés par cette commande
   *)
  compile_cmd: string -> string * string list;

}




(* ---- Utilitaires ---- *)



let (^=) r s = r := if !r="" then s else !r^" "^s
let (+=) l x = l := x :: !l 
let (@=) l x = l := !l @ x
let (^^) s t = if t="" then s else if s="" then t else s^" "^t
let id x = x
let fcons f = fun x q -> f x::q
let rec rev_map_append f l1 l2 = match l1 with
  | [] -> l2
  | x::q -> rev_map_append f q (f x::l2)
let string_of_list f = List.fold_left (fun acc x -> acc^^(f x)) ""
let flatten = List.fold_left (^^) ""
let select b = if b then fst else snd
let select_set b = if b then (fun (_,y) z -> z,y) else (fun (x,_) z -> x,z)
let rec print_inc = function
  | [] -> ""
  | x::q -> "-I "^x^^(print_inc q)
let print_p4 = function "" -> "" | s -> "-pp "^s
let oget x   = function Some x -> x | _ -> x
let ofold  f   = List.fold_right (function Some o -> f o | _ -> id)
let omap   f l = ofold (fcons f) l []
let otfold f   = List.fold_right (function {objects=Some o} -> f o | _ -> id)
let otmap  f l = otfold (fcons f) l []

let mtime f = (Unix.stat f).Unix.st_mtime
let file_newer f1 f2 = not (Sys.file_exists f2) || mtime f1 > mtime f2 
let exists_file_newer f = 
  let mtf = mtime f in
    List.exists (fun f' -> mtime f' > mtf)
let silent_remove f = try Sys.remove f with Sys_error _ -> ()
let touch_file f = if not (Sys.file_exists f) then close_out (open_out f)

exception CmdError of string
let call = Sys.command
let ecall cmd = if (call cmd) <> 0 then raise (CmdError cmd)
let exitf ?(err=1) x = kprintf (fun msg -> eprintf "%s" msg; exit err) x


let mk_ext e = (fun n -> n^e), (fun n -> Filename.check_suffix n e)
let ml   , is_ml    = mk_ext ".ml"
let mli  , is_mli   = mk_ext ".mli"
let mly  , is_mly   = mk_ext ".mly"
let mll  , is_mll   = mk_ext ".mll"
let glade, is_glade = mk_ext ".glade"
let cmo  , is_cmo   = mk_ext ".cmo"
let cmi  , is_cmi   = mk_ext ".cmi"
let cmx  , is_cmx   = mk_ext ".cmx"
let cma  , is_cma   = mk_ext ".cma"
let cmxa , is_cmxa  = mk_ext ".cmxa"
let oo   , is_o     = mk_ext ".o"
let aa   , is_a     = mk_ext ".a"
let cc   , is_c     = mk_ext ".c"
let run  , is_run   = mk_ext ".run"
let opt  , is_opt   = mk_ext ".opt"
let annot, is_annot = mk_ext ".annot"

let rec iter_units f = function
  | [] -> ()
  | u::q -> f u; iter_units f u.sub_units; iter_units f q

let rec fold_units f a = function
  | [] -> a
  | u::q -> fold_units f (f u (fold_units f a u.sub_units)) q

let get_line c = 
  let s = input_line c in 
    s, String.length s 



(* ---- Outils OCaml (c,dep,opt...) ---- *)


(* parsing de la sortie d'ocamldep *)
let tokenize ?(skip=false) c =
  try
    if skip then (
      (* ignorage du premier bloc (cmo/cmx) *)
      let last c = let s = input_line c in s.[String.length s - 1] in
	try while last c = '\\' do () done 
	with End_of_file -> ()
    );
    let s,ls  = get_line c in
    let i = String.index s ':' in
    let rec aux i acc ((s,ls) as sls) =
      if s.[i]='\\' then aux 4 acc (get_line c)
      else
        let j = String.index_from s i ' ' in
          if j+1 = ls then String.sub s i (j-i) :: acc
          else let k,sls' = if s.[j+1]='\\' then 4, get_line c else j+1, sls in
            aux k (String.sub s i (j-i) :: acc) sls'
    in
      aux (i+2) [] (s,ls)
  with End_of_file -> []

let ocamldep ~native ~depc ~sf =
  let nat = if native then "-native " else "" in
  let cmd = depc^^nat^^sf in
    if !print_deps then printf "%s\n%!" cmd
    else printf "DEPENDENCIES: %s\n%!" sf;
    let c_in = Unix.open_process_in cmd in
    let deps = tokenize ~skip:(native && is_ml sf) c_in in
    let deps' = 
      if native then deps 
      else List.map (fun f -> if is_cmo f then cmi (Filename.chop_extension f) else f) deps
    in
      ignore (Unix.close_process_in c_in);
      sf::deps'

let ocamldepi ~native ~depc ~f ~n = 
  ocamldep ~native ~depc ~sf:(if is_cmi f then mli n else ml n)

let ocamlobj ~bytec ~optc ~f ~n = 
  (select (is_cmx f) (optc, bytec))^" -c "^(ml n), [f; cmi n]
  
let ocamlobji ~bytec ~optc ~impl_flags ~f ~n = 
  if is_cmi f then bytec^" -c "^(mli n), [f]
              else (select (is_cmx f) (optc, bytec))^^impl_flags^^"-c"^^(ml n), [f]

let for_pack o = if !(o.ocaml_ForPack) = "" then ""
                 else "-for-pack" ^^ !(o.ocaml_ForPack)

let ocaml_options ?(o= !options) ?(flags="") ?(byte_flags="") ?(opt_flags="") ?pp ?(includes=[]) ?(ext_includes=[]) n =
  let flags' = (print_inc !(o.ocaml_Includes))^^(print_inc includes)^^(print_p4 (oget !(o.ocaml_P4) pp)) in
  let depc   = !(o.ocamldep)^^flags' in
  let flags' = !(o.ocaml_Flags)^^flags^^(print_inc !(o.ocaml_ExtIncludes))^^(print_inc ext_includes)^^flags' in
  let bytec  = !(o.ocamlc)  ^^flags'^^byte_flags^^ !(o.ocaml_ByteFlags) in
  let opt_flags' = (print_inc !(o.ocaml_Includes))^^(print_inc includes)^^(print_p4 (oget !(o.ocaml_P4_opt) pp)) in
  let opt_flags' = !(o.ocaml_Flags)^^flags^^(print_inc !(o.ocaml_ExtIncludes))^^(print_inc ext_includes)^^opt_flags' in
  let optc   = !(o.ocamlopt)^^(for_pack o)^^opt_flags'^^opt_flags ^^ !(o.ocaml_OptFlags)  in
    (Filename.concat !dir n), depc, bytec, optc



(* fabrication générique d'unités *)
let generic_unit
  ~name ?(sources=[]) ~targets ?(trash=targets) ?(auto_targets=[]) ?(sub_units=[]) ?(pregenerated=[])
  ?objects ~dependencies ?(dep_files=fun _->[]) ~compile_cmd ()
  = 
  { name=name; sources=sources; targets=targets; objects=objects; 
    trash=trash; auto_targets=auto_targets; sub_units=sub_units; pregenerated=pregenerated;
    dependencies=dependencies; compile_cmd=compile_cmd; dep_files=dep_files;
  }




(* ----- Différents types d'unités ----- *)


(* module ocaml, sans interface *)
let ocaml_Module ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n =
  let n, depc, bytec, optc = 
    ocaml_options ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n
  in 
  let ml_n, cmo_n, cmi_n, cmx_n = ml n, cmo n, cmi n, cmx n in
  let targets = [cmo_n; cmi_n; cmx_n] in
    generic_unit 
      ~name:n
      ~sources:[ml_n] ~targets ~trash:(oo n :: annot n :: targets) ~objects:(cmx_n, cmo_n)  
      ~dependencies:(fun ~native f -> if is_cmi f then [select native (cmx_n, cmo_n)]
		                                  else ocamldep ~native ~depc ~sf:(ml_n))
      ~compile_cmd: (fun f -> ocamlobj ~bytec ~optc ~f ~n)
      ~dep_files:   (fun f -> if is_cmi f then [] else [ml_n])
      ()
    
let generic_ocaml_Module_extension extension command =
  fun ?o ?flags ?byte_flags
      ?opt_flags ?(cmd_flags="")
      ?pp ?includes ?ext_includes n ->
  let n', depc, _, _ = 
    ocaml_options ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n
  in 
  let ext_n, ml_n = n'^extension, n'^".ml" in
  let cmd = command cmd_flags ext_n ml_n in
    generic_unit
      ~name:n
      ~sources:[ext_n] ~targets:[ml_n] ~trash:[]
      ~objects:(n'^".cmx", n'^".cmo") ~pregenerated:[ml_n]
      ~sub_units:[ocaml_Module ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n]
      ~dependencies:(fun ~native f -> [ext_n])
      (* ~dependencies:(fun ~native f -> ocamldep ~native ~depc ~sf:(ext_n)) *)
      ~compile_cmd: (fun f -> cmd, [f])
      ()


(* module ocaml, avec interface *)
let ocaml_IModule ?o ?flags ?byte_flags ?opt_flags ?(impl_flags = "") ?pp ?includes ?ext_includes n =
  let n, depc, bytec, optc = 
    ocaml_options ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n
  in 
  let ml_n, mli_n, cmo_n, cmi_n, cmx_n = ml n, mli n, cmo n, cmi n, cmx n in
  let targets = [cmo_n; cmi_n; cmx_n] in
    generic_unit 
      ~name:n
      ~sources:[ml_n; mli_n] ~targets ~trash:(oo n :: annot n :: targets) ~objects:(cmx_n, cmo_n)  
      ~dependencies:(fun ~native f -> ocamldepi ~native ~depc ~f ~n)
      ~compile_cmd: (fun f -> ocamlobji ~bytec ~impl_flags ~optc ~f ~n)
      ~dep_files:   (fun f -> if is_cmi f then [mli_n] else [ml_n])
      ()


(* interface ocaml pure *)
let ocaml_Interface ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n =
  let n, depc, bytec, optc = 
    ocaml_options ?o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n
  in 
  let mli_n, cmi_n = mli n, cmi n in
  let targets = [cmi_n] in
    generic_unit 
      ~name:n
      ~sources:[mli_n] ~targets ~trash:(annot n :: targets)
      ~dependencies:(fun ~native f -> ocamldep ~native ~depc ~sf:mli_n)
      ~compile_cmd: (fun f -> bytec^" -c "^(mli_n), [f])
      ~dep_files:   (fun f -> [mli_n])
      ()


(* objet C *)
let c_Module ?(o= !options) ?(flags="") ?(source_deps=[]) n =
  let n = Filename.concat !dir n in 
  let sources = List.map (Filename.concat !dir) source_deps in
  let c_n, o_n = cc n, oo n in
  let cc = !(o.ocamlc)^" -c"^^flags^^c_n in
  let sources = c_n::sources in
    generic_unit 
      ~name:n
      ~sources ~targets:[o_n]
      ~dependencies:(fun ~native f -> sources)
      ~objects:(o_n,o_n)
      ~compile_cmd: (fun f -> cc, [f])
      ()


(* lexer ocaml *)
let ocaml_Lexer ?(o= !options) ?flags ?byte_flags ?opt_flags ?(lex_flags="") ?pp ?includes ?ext_includes n =
  let n' = Filename.concat !dir n in 
  let mll_n, ml_n = mll n', ml n' in
  let ocamllex = !(o.ocamllex)^^lex_flags^^mll_n in
    generic_unit 
      ~name:n
      ~sources:[mll_n] ~targets:[ml_n]
      ~objects:(cmx n', cmo n') ~pregenerated:[ml_n]
      ~sub_units:[ocaml_Module ~o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n]
      ~dependencies:(fun ~native f -> [mll_n])
      ~compile_cmd: (fun f -> ocamllex, [f])
      ()


(* parser ocaml *)
let ocaml_Parser ?(o= !options) ?flags ?byte_flags ?opt_flags ?(yacc_flags="") ?pp ?includes ?ext_includes n =
  let n' = Filename.concat !dir n in 
  let mly_n, ml_n, mli_n = mly n', ml n', mli n' in
  let ocamlyacc = !(o.ocamlyacc)^^yacc_flags^^mly_n in
  let gen = [ml_n; mli_n] in
    generic_unit 
      ~name:n
      ~sources:[mly_n] ~targets:gen ~trash:((n'^".output") :: gen)
      ~objects:(cmx n', cmo n') ~pregenerated:gen
      ~sub_units:[ocaml_IModule ~o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n]
      ~dependencies:(fun ~native f -> [mly_n] )
      ~compile_cmd: (fun f -> ocamlyacc, gen)
      ()


(* interface glade à compiler en ocaml *)
let ocaml_Glade ?(o= !options) ?flags ?byte_flags ?opt_flags ?(glade_flags="") ?pp ?includes ?ext_includes n =
  let n' = Filename.concat !dir n in 
  let glade_n, ml_n = glade n', ml n' in
  let ocamlglade = !(o.ocamlglade)^^glade_flags^^glade_n^" > "^ml_n in
    generic_unit 
      ~name:n
      ~sources:[glade_n] ~targets:[ml_n]
      ~objects:(cmx n', cmo n') ~pregenerated:[ml_n]
      ~sub_units:[ocaml_Module ~o ?flags ?byte_flags ?opt_flags ?pp ?includes ?ext_includes n]
      ~dependencies:(fun ~native f -> [glade_n])
      ~compile_cmd: (fun f -> ocamlglade, [f])
      ()


(* paquet de modules ocaml *)
let ocaml_Package ?(o= !options) n sub_units = 
  let n = Filename.concat !dir n in 
  let ml_n, cmo_n, cmi_n, cmx_n, o_n = ml n, cmo n, cmi n, cmx n, oo n in
  let otmap2 f = List.fold_right
    (function { objects=None; targets=[x] }
        when is_cmi x
          && Sys.file_exists (mli (Filename.chop_extension x))
        -> (fun accu -> x :: accu)
     | { objects=Some x } -> fcons f x
     | _ -> id) sub_units [] in
  let objs, sobjs = 
    let x = otmap2 fst in
    let y = otmap2 snd in
    let sx = flatten x in
    let sy = flatten y in
      (x, y), (sx, sy)
  in
  let targets = [cmo_n; cmi_n; cmx_n] in
    generic_unit 
      ~name:n
      ~targets ~objects:(cmx_n, cmo_n) ~trash:(ml_n::o_n::targets) ~pregenerated:[ml_n] ~sub_units
      ~dependencies:(fun ~native f -> 
                       if is_cmi f then [select native (cmx_n, cmo_n)]
                       else
                         select native objs
                       )
      ~compile_cmd: (fun f -> 
		       if is_cmx f then !(o.ocamlopt)^^(for_pack o)^^"-I"^^n^^"-pack -o"^^f^^(fst sobjs), [cmx_n; cmi_n]
		       else !(o.ocamlc)^^"-pack -o"^^cmo_n^^(snd sobjs), [cmo_n; cmi_n])
      ()
    
let add_for_pack o n =
  if !(o.ocaml_ForPack) = "" then o.ocaml_ForPack := n
  else o.ocaml_ForPack := !(o.ocaml_ForPack)^"."^n

(* paquet de modules regroupés dans un sous répertoire *)
let ocaml_PackageDir ?o n l =
  let n' = Filename.concat !dir n in 
  let dir' = !dir in
    dir := Filename.concat n' "";
    let l' = new_scope (lazy (!options.ocaml_Includes += n';
                              add_for_pack !options n; Lazy.force l)) in
      dir := dir';
      ocaml_Package ?o n l'


(* librairie ocaml *)
let ocaml_Library ?(o= !options) ?flags ?byte_flags ?opt_flags ?includes ?(libraries=[]) ?(default=`Byte) n sub_units =
  let n, depc, bytec, optc = 
    ocaml_options ~o ?flags ?byte_flags ?opt_flags ?includes n
  in 
  let objs b = 
    let scma = select b (cmxa, cma) in
      (* (map scma extlib) @ (map scma libs) @ (otmap (select b) sub_units) *)
      List.rev_append 
	(rev_map_append scma
	   libraries
	   (List.rev_map scma !(o.ocaml_ExtLibraries))
	) 
	(otmap (select b) sub_units)
  in
  let objs, sobjs = 
    let x, y = objs true, objs false in
    let sx = flatten x in
    let sy = flatten y in
      (x,y), (sx,sy)
  in
  let cma_n, cmxa_n, a_n = cma n, cmxa n, aa n in
    generic_unit 
      ~name:n
      ~targets:[cma_n; cmxa_n] ~trash:[a_n] ~objects:(cmxa_n, cma_n) ~sub_units
      ~auto_targets:[if default=`Byte; then cma_n else cmxa_n]
      ~dependencies:(fun ~native f -> select (is_cmxa f) objs)
      ~compile_cmd: (fun f -> 
		       if is_cmxa f then optc ^" -a -o "^f^^(fst sobjs), [f]
                                    else bytec^" -a -o "^f^^(snd sobjs), [f])
      ()


(* exécutable ocaml *)
let ocaml_Program ?(o= !options) ?flags ?byte_flags ?opt_flags ?includes ?(libraries=[]) ?(default=`Byte) n sub_units =
  let n, depc, bytec, optc = 
    ocaml_options ~o ?flags ?byte_flags ?opt_flags ?includes n
  in 
  let objs b = 
    let scma = select b (cmxa, cma) in
      (* (map scma extlib) @ (map scma libs) @ (otmap (select b) sub_units) *)
      List.rev_append 
	(rev_map_append scma
	   libraries
	   (List.rev_map scma !(o.ocaml_ExtLibraries))
	) 
	(otmap (select b) sub_units)
  in
  let objs, sobjs = 
    let x, y = objs true, objs false in
    let sx = flatten x in
    let sy = flatten y in
      (x,y), (sx,sy)
  in
  let run_n, opt_n = run n, opt n in
    generic_unit 
      ~name:n
      ~targets:[run_n; opt_n] ~sub_units
      ~auto_targets:[if default=`Byte; then run_n else opt_n]
      ~dependencies:(fun ~native f -> select (is_opt f) objs)
      ~compile_cmd: (fun f -> 
		       if is_opt f then optc ^" -o "^f^^(fst sobjs), [f]
                                   else bytec^" -o "^f^^(snd sobjs), [f])
      ()


(* cible silencieuse *)
let phony_unit ?(depends=["@FORCE@"]) ?(command="") name = 
  generic_unit ~targets:[name] 
    ~name
    ~dependencies:(fun ~native f -> depends)
    ~compile_cmd: (fun _ -> command,[])
    ()

let fold_units_sources units f =
  let rec fold units accu =
    List.fold_left (fun accu u -> f u.name u.sources (fold u.sub_units) accu) accu units
  in fold units

(* (\* unité utilisateur *\) *)
(* let user_unit ?trash ~command ~depends name = *)
(*   let targets = [Filename.concat !dir name] in *)
(*   let trash = oget targets trash in *)
(*     generic_unit ~targets ~trash  *)
(*       ~dependencies:(fun ~native _ -> depends) *)
(*       ~compile_cmd: command *)
(*       () *)


    
(* récupération des fichiers sources OCaml *)
let ocaml_sources = 
  let rec crev_append l1 l2 = match l1 with
    | [] -> l2
    | x::q when is_ml x || is_mli x -> crev_append q (x::l2)
    | x::q -> crev_append q l2
  in
    fold_units (fun u -> crev_append u.sources) []
  



(* ---- Statuts des fichiers ---- *)



(* statut d'un fichier *)
type status_t = {
  mutable modified: int;         (* dernière fois que l'on a _réellement_ été modifié *)
  mutable updated:  int;         (* date de la dernière mise à jour *)
  mutable mtime:    float;       (* mtime  lors de la dernière mise à jour *)
  mutable digest:   Digest.t;    (* digest lors de la dernière mise à jour *)
  mutable cmd:      Digest.t;    (* digest de la commande utiliséee lors de la dernière mise à jour *)
  mutable depended: int*int;                 (* date du dernier calcul des dépendances (mode natif et non natif) *)
  mutable deps:     string list*string list; (* listes des dépendances *)
}

let make_status ~t ~f = 
  { modified = if Sys.file_exists f then t else 0; updated = 0; 
    depended = -1,-1; mtime = 0.0; digest = Digest.string "$*%"; 
    deps = [],[]; cmd = Digest.string "%*$" }

let update_status ?cmdd ~t ~f st = 
  if !debug_status then printf "UPDATE (%s) : " f; 
  if Sys.file_exists f then 
    (if st.updated < t then (
       st.updated <- t;
       (match cmdd with Some d -> st.cmd <- d | None -> ());
       let mt = mtime f in
	 if mt > st.mtime then (
	   st.mtime <- mt;
	   let d = Digest.file f in
	     if st.digest <> d then (
	       st.digest <- d;
 	       if !debug_status then printf "modified"; 
	       st.modified <- t
	     )
	 )
     )
    ) else st.updated <- 0;
  if !debug_status then printf "\n"


(* s1 plus récent que s2 *)
let (>>) s1 s2 = s1.modified > s2.updated





(* ---- Gestion des projets ---- *)

(* type des "projets" *)
type project_t = {
  units:       unit_t list;         (* liste des unités *)
  get_unit:    string -> unit_t;    (* obtention de l'unité correspondant à une cible *)
  date:        int;                 (* date courante *)
  get_status:  string -> status_t;  (* statut d'un fichier *)
  write_cache: unit -> unit;        (* sauvegarde du cache des statuts *)
}
exception NoRuleFor of string


(* un fichier donné est-il une cible *)
let is_target p = fun f -> try ignore (p.get_unit f); true with _ -> false


(* récupération des fichiers sources *)
let sources_of_project p = 
  let rec crev_append l1 l2 = match l1 with
    | [] -> l2
    | x::q when is_target p x -> crev_append q l2
    | x::q -> crev_append q (x::l2)
  in
    fold_units (fun u -> crev_append u.sources) [] p.units
  
  


(* alias pour le type des expressions mises en cache *)
type status_ct   = int * (string, status_t) Hashtbl.t
let st_cache = ".cache-status"

(* création d'un projet *)
let project ?(rebuild="ocaml build.ml")  ?(deps=["Makefile.ml"]) units =

  (* mise à jour éventuelle de YaM *)
  let () = if exists_file_newer Sys.executable_name deps then (
    let rebuild = ref rebuild in
      for i=1 to Array.length Sys.argv -1 do rebuild := !rebuild^" "^Sys.argv.(i) done;
      printf "yam is out-dated, rebuilding it (%s)\n%!" !rebuild;
      exit (Sys.command !rebuild)
  ) in

  (* construction de la table cible -> unités *)
  let get_unit = 
    let table = Hashtbl.create 23 in
      iter_units (fun u -> 
		    List.iter 
		    (fun t -> Hashtbl.add table t u) 
		    u.targets
		 ) units;
      (fun x -> try Hashtbl.find table x with Not_found -> raise (NoRuleFor x))
  in

  (* récupération des statuts *)
  let date, get_status, write_cache =
    let get_status (d,gt) = 
      let t = d-1 in
	(fun f -> 
	   try Hashtbl.find gt f 
	   with Not_found -> 
	     let s = make_status ~t ~f in
	       Hashtbl.add gt f s;
	       s
	)
    in
    let write_cache v = 
      (fun () -> 
	 let c_out = open_out_bin st_cache in
	   output_value c_out (v: status_ct);
	   close_out c_out
      )
    in
    let get ((d,_) as v) = d, get_status v, write_cache v in
      if Sys.file_exists st_cache then
	let c_in = open_in_bin st_cache in      
	let d,gt = (input_value c_in: status_ct) in
	  close_in c_in;
	  get (d+1, gt)
      else
	get (1, Hashtbl.create 50)
  in

    { units=units; get_unit=get_unit; date=date; 
      get_status=get_status; write_cache=write_cache }


(* nettoyage d'un projet *)
let clean p = 
  silent_remove st_cache;
  iter_units (fun u -> List.iter silent_remove u.trash) p.units


(* (\* génération de la documentation *\) *)
(* let doc p = *)
(*   let cmd =  *)
(*     let rec crev_append l1 l2 = match l1 with *)
(*       | [] -> l2 *)
(*       | x::q when is_ml x || is_mli x && not (is_target p x) -> crev_append q (l2^^x) *)
(*       | x::q -> crev_append q l2 *)
(*     in *)
(*       fold_units (fun u -> crev_append u.sources) !(!options.ocamldoc) p.units *)
(*   in *)
(*     if !print_cmds then printf "%s\n%!" cmd; *)
(*     ecall cmd *)


(* ---- Compilation ---- *)


(* compilation d'un projet (ou d'une de ses cibles) *)
let build ?target p = 

  let date       = p.date in
  let get_status = p.get_status in
  let get_unit   = p.get_unit in
  (* FIXME unused let is_target  = is_target p in *)

  (* compilation d'une cible *)
  let compile u f stf (cmd,cmdd,out) =
    if !print_cmds then printf "%s\n%!" cmd
    else printf "COMPILE: %s\n%!" f;
    ecall cmd;
    List.iter (fun f -> update_status ~cmdd ~t:date ~f (get_status f)) out
  in

  (* calcul de la liste des dépendances de f d'unité u et de statut st *)
  let rec dependencies ~native f u st = 
    let stf = get_status f in
      List.iter (fun sf -> 
		   try ignore(get_unit sf); build ~native [sf]
		   with NoRuleFor _ -> update_status ~t:date ~f:sf (get_status sf)
		) u.sources;
      if !debug_deps then printf "DEP: `%s' -> " f; 
      let l = u.dep_files f in
      let ddate = select native stf.depended in
      let recent df = (get_status df).modified > ddate in
	if l=[] || List.exists recent l then 
	  let deps = u.dependencies ~native f in
	    stf.deps     <- select_set native stf.deps     deps;
	    stf.depended <- select_set native stf.depended date;
	    if !debug_deps then printf "[%s]\n" (flatten deps);
	    deps
	else (
	  if !debug_deps then printf "cached [%s]\n" (flatten (select native stf.deps));
	  select native stf.deps
	)

  (* compilation par continuations d'une pile de cibles *)
  and build ?(native=false) ?(k=fun ()->()) = function
    | []   -> k()
    | f::q ->
	let stf = get_status f in 
	  if stf.updated < date then (
	    if !debug_build then printf "BUILD: `%s'\n" f;
	    let u = get_unit f in
	    let native = native || is_cmx f || is_opt f || is_cmxa f in
	    let cmd, out = u.compile_cmd f in
	    let deps = dependencies ~native f u stf in
	    let deps' = List.filter (fun f -> try ignore(get_unit f); true with NoRuleFor _ -> false) deps in
	    let cmdd = Digest.string cmd in
	    let k' () = 
	      if stf.cmd <> cmdd || 
		List.exists (fun f' -> get_status f' >> stf) deps 
	      then compile u f stf (cmd,cmdd,out)
	      else update_status ~cmdd ~t:date ~f stf;
	      build ~native ~k q
	    in
	      build ~native ~k:k' deps'
	  ) else
	    build ~native ~k q
  in
    
  (* ensemble des cibles à compiler *)
  let targets = match target with
    | Some t -> [t]
    | _ -> List.fold_left (fun acc u -> List.rev_append u.auto_targets acc) [] p.units
  in
    
    (* pré-génération des fichiers nécessaires au calcul des dépendances *)
    iter_units (fun u -> List.iter touch_file u.pregenerated) p.units;

    (* compilation des cibles *)
    try build ~k:p.write_cache targets with
      | CmdError _  -> p.write_cache(); exit 1
      | NoRuleFor t -> eprintf "No rule to build `%s'\n" t; p.write_cache(); exit 1



(* ---- Point d'entrée par défaut ---- *)

let main ?rebuild ?deps l =   
  let p = project ?rebuild ?deps l in
  let cwd() = Sys.chdir (Filename.dirname Sys.executable_name) in
  let targets = ref [] in
  let main = ref (fun () ->
		    match !targets with 
		      | [] -> build p
		      | l  -> List.iter (fun target -> build ~target p) (List.rev l)
		 )
  in
  let alone s = 
    if !targets <> [] || !Arg.current <> Array.length Sys.argv - 1
    then Printf.eprintf "Warning: `%s' specified, other arguments are ignored.\n" s
  in
  let version() = alone "-version"; Printf.printf "YaM version 1.0\n"; exit 0 in
  let clean()   = alone "-clean"; clean p; exit 0 in
    Arg.parse [
      "-version", Arg.Unit   version,    " \tdisplay version information";
      "-clean",   Arg.Unit   clean,      " \tremove all generated files";
      "-v",       Arg.Set    print_deps, " \t\tbe verbose: print dependencies commands";
      "-q",       Arg.Clear  print_cmds, " \t\tbe quiet: do not print commands";
      "-r",       Arg.String Sys.chdir,  " <dir>\tset `dir' as root directory";
      "-R",       Arg.Unit   cwd,        " \t\tset directory of YaM as root directory";

          "-db",    Arg.Set   debug_build,  " \tdebug build";
          "-dd",    Arg.Set   debug_deps,   " \tdebug deps";
          "-ds",    Arg.Set   debug_status, " \tdebug status";

    ] ((+=) targets) "usage: yam {-version, -clean, [options] [targets...]}";
    !main ()
