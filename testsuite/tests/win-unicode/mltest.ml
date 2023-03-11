(* TEST
include unix
flags += "-strict-sequence -w +A -warn-error +A"
* windows-unicode
** toplevel
*)

let foreign_names =
  List.sort compare
    [
      "simple";
      "\xE4\xBD\xA0\xE5\xA5\xBD"; (* "你好" *)
      "\x73\xC5\x93\x75\x72"; (* "sœur" *)
      "e\204\129te\204\129"; (* "été" *)
    ]
;;

let test_files =
  List.map (fun s -> s ^ ".txt") foreign_names
;;

let to_create_and_delete_files =
  [
    (* "верблюды" *)
    "\xD0\xB2\xD0\xB5\xD1\x80\xD0\xB1\xD0\xBB\xD1\x8E\xD0\xB4\xD1\x8B";
    "\xE9\xAA\x86\xE9\xA9\xBC"; (* "骆驼" *)
    "\215\167\215\162\215\158\215\156"; (* "קעמל" *)
    "\216\167\217\136\217\134\217\185"; (* "اونٹ" *)
    "L\225\186\161c \196\145\195\160"; (* "Lạc đà" *)
    "\224\176\146\224\176\130\224\176\159\224\177\134"; (* "ఒంటె" *)
    "\224\174\146\224\174\159\224\175\141\224\174\159\224\174\149\224\
     \174\174\224\175\141"; (* "ஒட்டகம்" *)
    "\217\136\216\180\216\170\216\177"; (* "وشتر" *)
    "\224\164\137\224\164\183\224\165\141\224\164\159\224\165\141\224\
     \164\176\224\164\131"; (* "उष्ट्रः" *)
    "\216\167\217\186"; (* "اٺ" *)
  ]
;;

let foreign_names2 =
  let rec take n l =
    if n = 0 then []
    else List.hd l :: take (n-1) (List.tl l)
  in
  take (List.length foreign_names) to_create_and_delete_files
;;

(* let env0 =
     List.sort compare
     (List.mapi (fun i v -> Printf.sprintf "OCAML_UTF8_VAR%d=%s" i v)
                foreign_names2) *)

(* let read_all ic = *)
(*   set_binary_mode_in ic false; *)
(*   let rec loop acc = *)
(*     match input_line ic with *)
(*     | exception End_of_file -> *)
(*         List.rev acc *)
(*     | s -> *)
(*         loop (s :: acc) *)
(*   in *)
(*   loop [] *)

(** WRAPPERS *)

let getenvironmentenv s =
  let env = Unix.environment () in
  let rec loop i =
    if i >= Array.length env then
      ""
    else begin
      let e = env.(i) in
      let pos = String.index e '=' in
      if String.sub e 0 pos = s then
        String.sub e (pos+1) (String.length e - pos - 1)
      else
        loop (i+1)
    end
  in
  loop 0
;;

let unix_getcwd () =
  Filename.basename (Unix.getcwd ())
;;

let sys_getcwd () =
  Filename.basename (Sys.getcwd ())
;;

let unix_readdir s =
  let h = Unix.opendir s in
  let rec loop acc =
    match Unix.readdir h with
    | s ->
        loop (s :: acc)
    | exception End_of_file ->
        Unix.closedir h;
        acc
  in
  List.sort compare (loop [])
;;

let sys_readdir s =
  List.sort compare (Array.to_list (Sys.readdir s))
;;

(* let open_process_in cmdline = *)
(*   let f cmdline = *)
(*     let ic as proc = Unix.open_process_in cmdline in *)
(*     let l = List.tl (read_all ic) in *)
(*     ignore (Unix.close_process_in proc); *)
(*     l *)
(*   in *)
(*   wrap "Unix.open_process_in" f ell cmdline (list quote) *)

(* let open_process_full filter cmdline env =
     let f cmdline env =
       let (ic, _, _) as proc =
         Unix.open_process_full cmdline (Array.of_list env)
       in
       let l = read_all ic in
       ignore (Unix.close_process_full proc);
       List.sort compare (List.filter filter l)
     in
     wrap2 "Unix.open_process_full" f ell (list quote) cmdline env (list quote)
*)

let test_readdir readdir =
  let filter s = List.mem s test_files && Filename.check_suffix s ".txt" in
  List.filter filter (readdir Filename.current_dir_name)
;;

let test_open_in () =
  let check s =
    let ic = open_in s in
    let l = input_line ic in
    close_in ic;
    l
  in
  let filter s = List.mem s test_files in
  let files = List.filter filter (sys_readdir Filename.current_dir_name) in
  List.map check files
;;

let test_getenv () =
  let equiv l r =
    assert (l = r);
    l, r
  in
  let doit key s =
    Unix.putenv key s;
    let l = equiv (Sys.getenv key) (getenvironmentenv key) in
    let r =
      Unix.putenv key (s ^ s);
      equiv (Sys.getenv key) (getenvironmentenv key)
    in
      l, r
  in
  List.map2 doit foreign_names foreign_names2
;;

let test_mkdir () =
  let doit s =
    Unix.mkdir s 0o755;
    Sys.file_exists s, Sys.is_directory s
  in
  List.map doit foreign_names
;;

let test_chdir chdir getcwd =
  let doit s =
    chdir s;
    let d = getcwd () in
    chdir Filename.parent_dir_name;
    d
  in
  List.map doit foreign_names
;;

let test_rmdir () =
  let doit s =
    Unix.rmdir s;
    Sys.file_exists s
  in
  List.map doit foreign_names
;;

let test_stat () =
  let doit s =
    (Unix.stat s).Unix.st_kind,
    (Unix.lstat s).Unix.st_kind,
    (Unix.LargeFile.stat s).Unix.LargeFile.st_kind,
    (Unix.LargeFile.lstat s).Unix.LargeFile.st_kind
  in
  List.map doit to_create_and_delete_files
;;

let test_access () =
  List.iter (fun s -> Unix.access s [Unix.F_OK]) to_create_and_delete_files

let test_rename rename =
  let doit s =
    let s' = s ^ "-1" in
    rename s s';
    let x = Sys.file_exists s, Sys.file_exists s' in
    rename s' s;
    let y = Sys.file_exists s, Sys.file_exists s' in
    x, y
  in
  List.map doit to_create_and_delete_files
;;

let test_open_out () =
  let doit s =
    let oc = open_out s in
    Printf.fprintf oc "Hello, %s\n" s;
    close_out oc;
    let ic = open_in s in
    let l = input_line ic in
    close_in ic;
    l
  in
  List.map doit to_create_and_delete_files
;;

let test_file_exists () =
  List.map Sys.file_exists to_create_and_delete_files
;;

let test_remove () =
  let doit s =
    Sys.remove s;
    Sys.file_exists s
  in
  List.map doit to_create_and_delete_files
;;

let create_file s =
  let oc = open_out_bin s in
  output_string oc s;
  close_out oc
;;

let test_symlink () =
  let foodir = "UNIQU\xE4\xBD\xA0\xE5\xA5\xBD" (* "UNIQU你好" *) in
  let foofile = "UNIQU\xE4\xBD\xA0\xE5\xA5\xBD/\xE4\xBD\xA0\xE5\xA5\xBD.txt"
                                                          (* "UNIQU你好/你好.txt" *)
  in
  let fileln = "\xE4\xBD\xA0\xE5\xA5\xBD-file-ln-s" (* "你好-file-ln-s" *) in
  let dirln = "\xE4\xBD\xA0\xE5\xA5\xBD-dir-ln-s" (* "你好-dir-ln-s" *) in
  Unix.mkdir foodir 0o777;
  create_file foofile;
  Unix.symlink ~to_dir:true foodir dirln;
  Unix.symlink ~to_dir:false foofile fileln;
  let res =
    (Unix.stat fileln).Unix.st_kind = Unix.S_REG &&
    (Unix.stat dirln).Unix.st_kind = Unix.S_DIR &&
    (Unix.lstat fileln).Unix.st_kind = Unix.S_LNK &&
    (Unix.lstat dirln).Unix.st_kind = Unix.S_LNK
  in
  Sys.remove foofile;
  Sys.remove fileln;
  Unix.rmdir dirln;
  Unix.rmdir foodir;
  res
;;

List.iter create_file test_files;;

let t_unix_readdir = test_readdir unix_readdir;;
let t_sys_readdir = test_readdir sys_readdir;;
let t_open_in = test_open_in ();;
let t_open_out = test_open_out ();;
let t_file_exists = test_file_exists ();;
let t_stat = test_stat ();;
test_access ();;
let t_unix_rename = test_rename Unix.rename;;
let t_sys_rename = test_rename Sys.rename;;
test_remove ();;
test_mkdir ();;
let t_sys_chdir = test_chdir Sys.chdir sys_getcwd;;
let t_unix_chdir = test_chdir Unix.chdir unix_getcwd;;
test_rmdir ();;
let t_getenv = test_getenv ();;
if Unix.has_symlink () then test_symlink () else true;;
