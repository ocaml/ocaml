(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module Classdef = struct
  class virtual ['a, 'b, 'c] cl0 =
    object
      constraint 'c = < m : 'a -> 'b -> int; .. >
    end

  class virtual ['a, 'b] cl1 =
    object
      method virtual raise_trouble : int -> 'a
      method virtual m : 'a -> 'b -> int
    end

  class virtual ['a, 'b] cl2 =
    object
      method virtual as_cl0 : ('a, 'b, ('a, 'b) cl1) cl0
    end
end

module M : sig
  type refer = { poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) }
end = struct
  type refer = { poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) }
end

type refer1 = < poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) >
type refer2 = < poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) >

(* Now this works too *)
let f (x : refer1) = (x : refer2)
