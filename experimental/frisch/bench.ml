let lexing s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- Lexing.dummy_pos;
  let rec loop () =
    match My_lexer.token lexbuf with
    | Parser.EOF -> ()
    | token -> loop ()
  in
  loop ()

let s =
  let ic = open_in "../../typing/typecore.ml" in
  let b = Buffer.create 16 in
  begin
    try while true do
        Buffer.add_string b (input_line ic);
        Buffer.add_char b '\n'
      done
    with End_of_file -> ()
  end;
  close_in ic;
  Buffer.contents b

let () =
  let alloc0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  let n = 100 in
  for _ = 1 to n do
    lexing s
  done;
  let time = Unix.gettimeofday () -. t0 in
  let alloc = Gc.allocated_bytes () -. alloc0 in

  let len = float (String.length s) *. float n in
  let mb = len /. 1024. /. 1024. in
  Printf.printf " % 8.02f Mb/s   % 8.02f ms/Mb   alloc x % 8.02f \n%!"
    (mb /. time)
    (time *. 1000. /. mb)
    (alloc /. len)
