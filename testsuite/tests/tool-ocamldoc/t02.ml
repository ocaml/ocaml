module Foo = struct type t = int let x = 1 end;;
module type TFoo = module type of Foo;;
