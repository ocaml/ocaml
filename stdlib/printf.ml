external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let fprintf outchan format =
  let format = (Obj.magic format : string) in
  let rec doprn i =
    if i >= String.length format then
      Obj.magic ()
    else
      match String.get format i with
        '%' ->
          let j = skip_args (succ i) in
          begin match String.get format j with
            '%' ->
              output_char outchan '%';
              doprn (succ j)
          | 's' ->
              Obj.magic(fun s ->
                if j <= i+1 then
                  output_string outchan s
                else begin
                  let p =
                    try
                      int_of_string (String.sub format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg "fprintf: bad %s format" in
                  if p > 0 & String.length s < p then begin
                    output_string outchan
                                  (String.make (p - String.length s) ' ');
                    output_string outchan s
                  end else if p < 0 & String.length s < -p then begin
                    output_string outchan s;
                    output_string outchan
                                  (String.make (-p - String.length s) ' ')
                  end else
                    output_string outchan s
                end;
                doprn (succ j))
          | 'c' ->
              Obj.magic(fun c ->
                output_char outchan c;
                doprn (succ j))
          | 'd' | 'o' | 'x' | 'X' | 'u' ->
              Obj.magic(doint i j)
          | 'f' | 'e' | 'E' | 'g' | 'G' ->
              Obj.magic(dofloat i j)
          | 'b' ->
              Obj.magic(fun b ->
                output_string outchan (string_of_bool b);
                doprn (succ j))
          | 'a' ->
              Obj.magic(fun printer arg ->
                printer outchan arg;
                doprn(succ j))
          | 't' ->
              Obj.magic(fun printer ->
                printer outchan;
                doprn(succ j))
          | c ->
              invalid_arg ("fprintf: unknown format")
          end
      |  c  -> output_char outchan c; doprn (succ i)

  and skip_args j =
    match String.get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j
    
  and doint i j n =
    let len = j-i in
    let fmt = String.create (len+2) in
    String.blit format i fmt 0 len;
    String.set fmt len 'l';
    String.set fmt (len+1) (String.get format j);
    output_string outchan (format_int fmt n);
    doprn (succ j)

  and dofloat i j f =
    output_string outchan (format_float (String.sub format i (j-i+1)) f);
    doprn (succ j)

  in doprn 0

let printf fmt = fprintf stdout fmt
and eprintf fmt = fprintf stderr fmt

