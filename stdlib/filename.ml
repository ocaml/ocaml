let check_suffix name suff =
 String.length name >= String.length suff &
 String.sub name (String.length name - String.length suff) (String.length suff)
    = suff

let chop_suffix name suff =
  let n = String.length name - String.length suff in
  if n < 0 then invalid_arg "chop_suffix" else String.sub name 0 n

let current_dir_name = "."

let concat dirname filename =
  let l = String.length dirname - 1 in
  if l < 0 or String.get dirname l = '/'
  then dirname ^ filename
  else dirname ^ "/" ^ filename

let is_absolute n =
     (String.length n >= 1 & String.sub n 0 1 = "/")
  or (String.length n >= 2 & String.sub n 0 2 = "./")
  or (String.length n >= 3 & String.sub n 0 3 = "../")

let slash_pos s =
  let rec pos i =
    if i < 0 then raise Not_found
    else if String.get s i = '/' then i
    else pos (i - 1)
  in pos (String.length s - 1)

let basename name =
  try
    let p = slash_pos name + 1 in
      String.sub name p (String.length name - p)
  with Not_found ->
    name

let dirname name =
  try
    match slash_pos name with
      0 -> "/"
    | n -> String.sub name 0 (slash_pos name)
  with Not_found ->
    "."






