open Printf

let () =
  let argc = Array.length Sys.argv in
  let out = ref stdout in
  if argc > 1 then begin
    for i = 1 to argc - 1 do
      match Sys.argv.(i) with
      | "-err" -> flush !out; out := stderr
      | "-out" -> flush !out; out := stdout
      | arg    -> fprintf !out "argv[%d] = {|%s|}\n" i arg
    done
  end else begin
    try
      while true do
        let l = input_line stdin in
        printf "%s\n" l
      done
    with End_of_file -> ()
  end
