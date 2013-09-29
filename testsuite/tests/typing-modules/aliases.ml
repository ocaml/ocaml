module C = Char;;

C.chr 66;;

module C' : module type of Char = C;;

module C'' : (module C) = C';; (* fails *)

module C'' : (module Char) = C;;
