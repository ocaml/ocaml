let print_exn = function
    Out_of_memory ->
      prerr_string "Out of memory\n"
  | Match_failure(file, first_char, last_char) ->
      prerr_string "Pattern matching failed, file ";
      prerr_string file;
      prerr_string ", chars "; prerr_int first_char;
      prerr_char '-'; prerr_int last_char; prerr_char '\n'
  | x ->
      prerr_string "Uncaught exception: ";
      prerr_string (Obj.magic(Obj.field (Obj.field (Obj.repr x) 0) 0));
      if Obj.size (Obj.repr x) > 1 then begin
        prerr_char '(';
        for i = 1 to Obj.size (Obj.repr x) - 1 do
          if i > 1 then prerr_string ", ";
          let arg = Obj.field (Obj.repr x) i in
          if not (Obj.is_block arg) then
            prerr_int (Obj.magic arg : int)
          else if Obj.tag arg = 253 then begin
            prerr_char '"';
            prerr_string (Obj.magic arg : string);
            prerr_char '"'
          end else
            prerr_char '_'
        done;
        prerr_char ')'
      end;
      prerr_char '\n'

let print fct arg =
  try
    fct arg
  with x ->
    print_exn x;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    print_exn x;
    exit 2
