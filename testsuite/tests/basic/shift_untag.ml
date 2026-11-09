(* TEST *)

let[@inline never] lsr62 x = x lsr 62
let[@inline never] lsr63 x = x lsr 63
let[@inline never] asr62 x = x asr 62
let[@inline never] asr63 x = x asr 63

let[@inline never] lsr62_int64 x = Int64.of_int (x lsr 62)
let[@inline never] lsr63_int64 x = Int64.of_int (x lsr 63)
let[@inline never] asr62_int64 x = Int64.of_int (x asr 62)
let[@inline never] asr63_int64 x = Int64.of_int (x asr 63)

let[@inline never] lsr62_float x = float_of_int (x lsr 62)
let[@inline never] lsr63_float x = float_of_int (x lsr 63)
let[@inline never] asr62_float x = float_of_int (x asr 62)
let[@inline never] asr63_float x = float_of_int (x asr 63)

let inputs = [min_int; -max_int; -5; -1; 0; 1; 5; max_int]

let check name f g =
  List.iter
    (fun x ->
      let got = f x and want = g x in
      if got <> want then
        Printf.printf "%s %d: got %s, expected %s\n" name x got want)
    inputs

let () =
  if Sys.int_size = 63 then begin
    let n62 = Sys.opaque_identity 62 and n63 = Sys.opaque_identity 63 in
    let i = string_of_int and l = Int64.to_string
    and f = Printf.sprintf "%.0f" in
    check "lsr 62" (fun x -> i (lsr62 x)) (fun x -> i (x lsr n62));
    check "lsr 63" (fun x -> i (lsr63 x)) (fun x -> i (x lsr n63));
    check "asr 62" (fun x -> i (asr62 x)) (fun x -> i (x asr n62));
    check "asr 63" (fun x -> i (asr63 x)) (fun x -> i (x asr n63));
    check "Int64.of_int (lsr 62)" (fun x -> l (lsr62_int64 x))
      (fun x -> l (Int64.of_int (x lsr n62)));
    check "Int64.of_int (lsr 63)" (fun x -> l (lsr63_int64 x))
      (fun x -> l (Int64.of_int (x lsr n63)));
    check "Int64.of_int (asr 62)" (fun x -> l (asr62_int64 x))
      (fun x -> l (Int64.of_int (x asr n62)));
    check "Int64.of_int (asr 63)" (fun x -> l (asr63_int64 x))
      (fun x -> l (Int64.of_int (x asr n63)));
    check "float_of_int (lsr 62)" (fun x -> f (lsr62_float x))
      (fun x -> f (float_of_int (x lsr n62)));
    check "float_of_int (lsr 63)" (fun x -> f (lsr63_float x))
      (fun x -> f (float_of_int (x lsr n63)));
    check "float_of_int (asr 62)" (fun x -> f (asr62_float x))
      (fun x -> f (float_of_int (x asr n62)));
    check "float_of_int (asr 63)" (fun x -> f (asr63_float x))
      (fun x -> f (float_of_int (x asr n63)))
  end;
  print_endline "OK"
