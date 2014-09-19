let t = Array.init 10 (fun i -> i)

let m = max t      (* seen in a real beginner's code! *)

let _ =
   for i = 1 to m do 
      print_int i;
   done