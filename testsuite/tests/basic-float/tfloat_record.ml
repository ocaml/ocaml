let s = { Float_record.f = Float_record.make 1.0 };;

print_float (Float_record.from s.Float_record.f);;
print_newline ();;


let b = (Float_array.small_float_array [@inlined]) 12
let c = (Float_array.longer_float_array [@inlined]) 34

let print_array a =
  Array.iter (fun f ->
      print_float f;
      print_newline ()) a;
  print_newline ()

let () =
  print_array (fst b);
  print_array (fst c);
