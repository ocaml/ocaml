open Printf
;;

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

let output result str =
  print_string ("Test Buffer " ^ str ^ " " ^ result ^ "\n")
;;

let passed = output "passed"
;;

let failed = output "failed"
;;

let truncate_neg = 
  try 
    Buffer.truncate buf (-1);
    failed "truncate: negative"
  with
    Invalid_argument "Buffer.truncate" -> passed "truncate: negative"
;;

let truncate_large =
  try
    Buffer.truncate buf (n+1);
    failed "truncate: large"
  with
    Invalid_argument "Buffer.truncate" -> passed "truncate: large"
;;

let truncate_correct =
  let n' = n - 1 in
  try
    Buffer.truncate buf n';
    if Buffer.length buf = n' then
      passed "truncate: in-range"
    else
      failed "truncate: in-range"
  with
    Invalid_argument "Buffer.truncate" -> failed "truncate: in-range"
;;

