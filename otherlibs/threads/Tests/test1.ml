(* Classic producer-consumer *)

type 'a prodcons =
  { buffer: 'a array;
    lock: Mutex.t;
    mutable readpos: int;
    mutable writepos: int;
    notempty: Condition.t;
    notfull: Condition.t }

let new size init =
  { buffer = Array.new size init;
    lock = Mutex.new();
    readpos = 0;
    writepos = 0;
    notempty = Condition.new();
    notfull = Condition.new() }

let put p data =
  Mutex.lock p.lock;
  while (p.writepos + 1) mod Array.length p.buffer = p.readpos do
    Condition.wait p.notfull p.lock
  done;
  p.buffer.(p.writepos) <- data;
  p.writepos <- (p.writepos + 1) mod Array.length p.buffer;
  Condition.signal p.notempty;
  Mutex.unlock p.lock

let get p =
  Mutex.lock p.lock;
  while p.writepos = p.readpos do
    Condition.wait p.notempty p.lock
  done;
  let data = p.buffer.(p.readpos) in
  p.readpos <- (p.readpos + 1) mod Array.length p.buffer;
  Condition.signal p.notfull;
  Mutex.unlock p.lock;
  data

(* Test *)

let buff = new 20 0

let rec produce n =
  print_int n; print_string "-->"; print_newline();
  put buff n;
  produce (n+1)

let rec consume () =
  let n = get buff in
  print_string "-->"; print_int n; print_newline();
  consume ()

let _ =
  Thread.new produce 0;
  consume()



