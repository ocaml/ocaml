let x = [: int :];;
let y = [: ^x :];;

open Format;;

type foo = T of int;;

fprintf std_formatter "%a@." Gprint.print (1,2,"a",[1;2;3],[|1.2;3.4;5.6|], true, (fun x -> x), T 1);;
