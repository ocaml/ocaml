type how = Nicely | Badly;;

let greet how who =
  match how with Nicely -> Printf.printf "Hello, %s !\n" who
               | Badly  -> Printf.printf "Oh, here is that %s again.\n" who
;;
