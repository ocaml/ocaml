open Printf
;;

(* Set up*)
let n = 10
;;

let buf = Buffer.create n
;;

let () =
  for i = 1 to 10 do
    Buffer.add_char buf 'a'
  done
;;

assert (Buffer.length buf = n)
;;

(* Helpers *)

let output result str =
  print_string ("Buffer " ^ str ^ " " ^ result ^ "\n")
;;

let passed = output "passed"
;;

let failed = output "failed"
;;

(* Tests *)
let () = print_string "Standard Library: Module Buffer\n"
;;

let truncate_neg : unit = 
  let msg =  "truncate: negative" in
  try 
    Buffer.truncate buf (-1);
    failed msg
  with
    Invalid_argument "Buffer.truncate" ->
      passed msg
;;

let truncate_large : unit =
  let msg = "truncate: large" in
  try
    Buffer.truncate buf (n+1);
    failed msg
  with
    Invalid_argument "Buffer.truncate" ->
      passed msg
;;

let truncate_correct : unit =
  let n' = n - 1 
  and msg =  "truncate: in-range" in
  try
    Buffer.truncate buf n';
    if Buffer.length buf = n' then
      passed msg
    else
      failed msg
  with
    Invalid_argument "Buffer.truncate" ->
      failed msg
;;

let reset_non_zero : unit =
  let msg = "reset: non-zero" in
  Buffer.reset buf;
  if Buffer.length buf = 0 then
    passed msg
  else
    failed msg
;;

let reset_zero : unit =
  let msg = "reset: zero" in
  Buffer.reset buf;
  if Buffer.length buf = 0 then
    passed msg
  else
    failed msg
;;
