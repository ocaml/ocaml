(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 & for_all2 pred tl1 tl2
  | (_, _) -> false

(* File functions *)

let find_in_path path name =
  if Filename.is_absolute name then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

let temp_file base suffix =
  let rec try_name counter =
    let name = "/tmp/" ^ base ^ string_of_int counter ^ suffix in
    if Sys.file_exists name then try_name (counter + 1) else name
  in try_name 0

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.new size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* String functions *)

let capitalize s =
  let r = String.create (String.length s) in
  String.blit s 0 r 0 (String.length s);
  let c = s.[0] in
  if c >= 'a' & c <= 'z' then r.[0] <- Char.chr(Char.code c - 32);
  r

let lowercase s =
  let r = String.create (String.length s) in
  String.blit s 0 r 0 (String.length s);
  let c = s.[0] in
  if c >= 'A' & c <= 'Z' then r.[0] <- Char.chr(Char.code c + 32);
  r

(* File copy *)

let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = String.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 1 else 1 + log2(n asr 1)

let align n a =
  (n + a - 1) land (-a)
