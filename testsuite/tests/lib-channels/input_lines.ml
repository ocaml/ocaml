(* TEST *)

open Printf

let data_file =
  "data.txt"

let length = 500

let rec check lo hi l =
  if lo = hi + 1 then begin
    if l <> [] then failwith "list too long"
  end else begin
    match l with
    | [] -> failwith "list too short"
    | h :: t ->
        if int_of_string h <> lo then failwith "wrong value";
        check (lo + 1) hi t
  end

let _ =
  Out_channel.with_open_text data_file
    (fun oc ->
      fprintf oc "0";
      for i = 1 to length do fprintf oc "\n%d" i done);
  In_channel.with_open_text data_file In_channel.input_lines
  |> check 0 length;
  In_channel.with_open_text data_file
    (In_channel.fold_lines (fun accu line -> line :: accu) [])
  |> List.rev
  |> check 0 length;
  Sys.remove data_file
