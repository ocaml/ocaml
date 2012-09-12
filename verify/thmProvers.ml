open EscSyn
open ToErgosrc
open Why_ptree
open Unix

let write_tasks_to_file filename tasks = 
    let outchnl = open_out filename in
    out_ergotasks outchnl tasks;
    close_out outchnl
    (* ignore (Sys.command (Printf.sprintf "rm %s" filename)) *)

let isValid s = 
  (* print_string s; *)
  try let _ = Str.search_forward (Str.regexp "Valid") s 0 in
  Valid
  with Not_found -> Unknown

let mValid s n = 
  try
  let rec aux j m = 
  if j=n then []
  else 
  let i = Str.search_forward (Str.regexp "Valid") s m in
      (j,Valid)::(aux (j+1) (i+1))  
  in aux 0 0   
  with Not_found -> []

(* 
let askErgo tasks = 
  let filename = "temp.why" in
  let outchnl = open_out filename in
  out_ergotasks outchnl tasks;
  close_out outchnl;
  ignore (Sys.command (Printf.sprintf "alt-ergo -redondance 4 -stop 2 %s" filename));
  let s = input_line stdin in
  (* close_in stdin; *)
  print_string ("the result"^s);
  isValid s
*)

let channel_contents_buf cin =
  let buf = Buffer.create 1024
  and buff = String.make 1024 ' ' in
  let n = ref 0 in
  while n := input cin buff 0 1024; !n <> 0 do
    Buffer.add_substring buf buff 0 !n
  done;
  buf

let channel_contents cin = Buffer.contents (channel_contents_buf cin)

let askmErgo filename tasks n = 
  let outchnl = open_out filename in
  out_ergotasks outchnl tasks;
  close_out outchnl;
  let (cin, cout) as p = open_process (Printf.sprintf "alt-ergo -redondance 4 -stop 2 %s" filename) in
  let out = channel_contents cin in
  let _ = close_process p in
  mValid out n

let askErgo filename tasks = 
    match askmErgo filename tasks 1 with
    | [] -> Unknown
    | (i,v)::xs -> Valid
  

