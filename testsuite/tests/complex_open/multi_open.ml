let sp = Printf.sprintf
let not s  = sp "Module %s not opened" s
let right s = sp "Module %s correctly opened" s
let wrong child parent = sp "Wrong module %s: Nested Module %s.%s" child parent child
let shadow x m = sp "Wrong variable %s: shadowed from %s" x m
			    
let x = not "M"
and y = not "N"
and z = not "L"

module M = struct
    let x = right "M"
    let y = shadow "y" "M"
    let z = shadow "z" "M"
    module N = struct let y= wrong "N" "M" end
  end
	     
module N = struct
    let z = shadow "z" "N"
    let y = right "N"
    module L = struct let z= wrong "L" "N" end
  end
	     
module L = struct
    let z = right "L"
    module M = struct let x= wrong "M" "L" end
  end

let pp = Printf.printf
let test global l=
  pp "%s open:\n" global;
  List.iter (pp "\t%s\n") l

let () =
  let open M[@local][@attribute] and N and L [@attribute] in
  test "Local" [x;y;z]

open! M[@one_attribute][@another] and N and L[@local] [@@item]

let () =
  test "Global" [x;y;z]
