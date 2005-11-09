let rec print = generic
| print_int | print_float 
| : 'a * 'b -> unit =>
  fun (x,y) -> print x; print y
| : 'a list -> unit => 
  function [] -> () | x::xs -> print x; print xs
in
print ([[1];[2;3];[4;5;6]], [[[1.2];[3.4;5.6];[7.8;9.0]]]);
print_newline ()


 
