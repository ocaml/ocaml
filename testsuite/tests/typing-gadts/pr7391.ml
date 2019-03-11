(* TEST
   * expect
*)

class virtual child1 parent =
  object
    method private parent = parent
  end

class virtual child2 =
  object(_ : 'self)
    constraint 'parent = < previous: 'self option; .. >
    method private virtual parent: 'parent
  end

(* Worked in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
class virtual child1 : 'a -> object method private parent : 'a end
class virtual child2 :
  object ('a)
    method private virtual parent : < previous : 'a option; .. >
  end
- : < child : child1; previous : child1 option > = <obj>
|}]

(* Worked in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child (_ : unit) =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
- : < child : unit -> child1; previous : child1 option > = <obj>
|}]

(* Worked in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child () =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
- : < child : unit -> child1; previous : child1 option > = <obj>
|}]

(* Didn't work in 4.03, but works in 4.07 *)
let _ =
  object(self)
    method previous = None
    method child =
      let o =
      object
        inherit child1 self
        inherit child2
      end
      in o
  end;;
[%%expect{|
- : < child : child1; previous : child1 option > = <obj>
|}]

(* Also didn't work in 4.03 *)

type gadt = Not_really_though : gadt

let _ =
  object(self)
    method previous = None
    method child Not_really_though =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
type gadt = Not_really_though : gadt
- : < child : gadt -> child1; previous : child1 option > = <obj>
|}]
