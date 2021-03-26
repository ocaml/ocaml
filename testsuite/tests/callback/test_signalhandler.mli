external mycallback1 : ('a -> 'b) -> 'a -> 'b = "mycallback1"
external mycallback2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = "mycallback2"
external mycallback3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  = "mycallback3"
external mycallback4 :
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e = "mycallback4"
val tak : int * int * int -> int
val tak2 : int -> int * int -> int
val tak3 : int -> int -> int -> int
val tak4 : int -> int -> int -> int -> int
val raise_exit : unit -> unit
val trapexit : unit -> int
external mypushroot : 'a -> ('b -> 'c) -> 'b -> 'a = "mypushroot"
external mycamlparam : 'a -> ('b -> 'c) -> 'b -> 'a = "mycamlparam"
val tripwire : (string -> (unit -> int) -> unit -> 'a) -> 'a
val sighandler : 'a -> unit
external unix_getpid : unit -> int = "unix_getpid" [@@noalloc]
external unix_kill : int -> int -> unit = "unix_kill" [@@noalloc]
val callbacksig : unit -> string
