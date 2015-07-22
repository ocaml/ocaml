
  (* auxiliary type *)
  type 'a box = { x : ' a}

  (* Identifiers to be shadowed *)
  let value = 1
  type t = Constructor
  class c = object end
  module Module = struct end
  type record = { test_label : unit; other_label : unit }
  module type module_type = sig end

(* Shadowing module *)
module Shadower = struct
  let value = 2
  type t = Constructor
  type record = { test_label : unit; other_label_shadower : unit }
  let r = { test_label = (); other_label_shadower = () }
  class c = object end
  module Module = struct end
  module type module_type = sig end
end;;

(* First, test shadowing with global open *)
module Global = struct
  open Shadower (* Trigger warnings 44 and 45 *)
  let value = value
  let record = { test_label = (); other_label_shadower = () }
  let constructor: Shadower.t  = Constructor
  let o = new c
  module Module = Module
  module type  module_type = module_type
end;;

module Global_with_override = struct
  open! Shadower (* explicit override : do not trigger warnings 44 and 45 *)
  let value = value
  let record = { test_label = (); other_label_shadower = () }
  let constructor: Shadower.t  = Constructor
  let o = new c
  module Module = Module
  module type  module_type = module_type
end;;

(* Then, test shadowing with local open *)
let local =
  let open Shadower in
  let module Module : module_type = Module in
  value, { test_label = (); other_label_shadower = () } , (Constructor: Shadower.t) , new c

let local_with_override =
  let open! Shadower in
  value, { test_label = (); other_label_shadower = () } , (Constructor: Shadower.t) , new c, ( module Module : module_type )


module S = Shadower (* Shadower is too long to type here *)
(* Last, test shadowing with delimited local open *)
let local_delimited_parens =
  S.( value, { test_label = (); other_label_shadower = () } , (Constructor: Shadower.t) , new c, ( module Module : module_type ) )
and list =
  S.[value], S.[{ test_label = (); other_label_shadower = () }], S.[(Constructor: Shadower.t)], S.[new c], S.[( module Module : module_type )]
and array =
  S.[|value|], S.[|{ test_label = (); other_label_shadower = () }|], S.[|(Constructor: Shadower.t)|], S.[|new c|],  S.[|( module Module : module_type )|]
and record =
  S.{ x =value }, S.{ x = { test_label = (); other_label_shadower = () } }, S.{ x = (Constructor: Shadower.t) }, S.{ x = new c }, S.{ x = ( module Module : module_type ) }
and objects =
  object val x = value method m = S.{< x = value >} end, object val x = S.r method m = S.{< x = { test_label = (); other_label_shadower = () } >} end, object val x = S.Constructor method m = S.{< x = (Constructor: Shadower.t) >} end, object val x = new S.c  method m = S.{< x = new c >} end, object val x = ( module S.Module : S.module_type ) method m = S.{< x = ( module Module : module_type ) >} end

let local_delimited_parens_with_override =
  S.!( value, { test_label = (); other_label_shadower = () } , (Constructor: Shadower.t) , new c, ( module Module : module_type ) )
and list_override =
  S.![value], S.![{ test_label = (); other_label_shadower = () }], S.![(Constructor: Shadower.t)], S.![new c], S.![( module Module : module_type )]
and array_override =
  S.![|value|], S.![|{ test_label = (); other_label_shadower = () }|], S.![|(Constructor: Shadower.t)|], S.![|new c|],  S.![|( module Module : module_type )|]
and record_override =
  S.!{ x =value }, S.!{ x = { test_label = (); other_label_shadower = () } }, S.!{ x = (Constructor: Shadower.t) }, S.!{ x = new c }, S.!{ x = ( module Module : module_type ) }
and objects_override =
  object val x = value method m = S.!{< x = value >} end, object val x = S.r method m = S.!{< x = { test_label = (); other_label_shadower = () } >} end, object val x = S.Constructor method m = S.!{< x = (Constructor: Shadower.t) >} end, object val x = new S.c  method m = S.!{< x = new c >} end, object val x = ( module S.Module : S.module_type ) method m = S.!{< x = ( module Module : module_type ) >} end
