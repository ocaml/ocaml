(* Classic producer-consumer with semaphores *)

type 'a prodcons =
  { buffer: 'a array;
    mutable readpos: int;
    mutable writepos: int;
    readsem: Semaphore.t;
    writesem: Semaphore.t }

let create size init =
  { buffer = Array.create size init;
    readpos = 0;
    writepos = 0;
    readsem = Semaphore.create 0;
    writesem = Semaphore.create size }

let put p data =
  Semaphore.wait p.writesem;
  p.buffer.(p.writepos) <- data;
  p.writepos <- (p.writepos + 1) mod Array.length p.buffer;
  Semaphore.post p.readsem

let get p =
  Semaphore.wait p.readsem;
  let data = p.buffer.(p.readpos) in
  p.readpos <- (p.readpos + 1) mod Array.length p.buffer;
  Semaphore.post p.writesem;
  data

(* Test *)

let buff = create 20 0

let rec produce n =
  print_int n; print_string "-->"; print_newline();
  put buff n;
  produce (n+1)

let rec consume () =
  let n = get buff in
  print_string "-->"; print_int n; print_newline();
  consume ()

let _ =
  Thread.create produce 0;
  consume()



