let sp = Printf.sprintf
let not s  = sp "Module %s not included" s
let right s = sp "Module %s correctly included" s
let wrong child parent = sp "Wrong module %s: Nested Module %s.%s" child parent child
let shadow x m = sp "Wrong variable %s: shadowed from %s" x m

let x = not "M"
and y = not "N"
and z = not "L"

module type S_X = sig val x: string end
module type S_Y = sig val y: string end
module type S_Z = sig val z:string end

module type S_XYZ =
  sig
    include S_X[@attribute] and S_Y and S_Z [@@item]
  end

module M =
  struct
    let x = right "M"
  end
module N =
  struct
    let y = right "N"
  end
module L =
struct
  let z = right "L"
end
module Id ( M: S_X ) = struct let x =  M.x end

let pp = Printf.printf
let test label l=
  pp "%s include:\n" label;
  List.iter (pp "\t%s\n") l;;

module MNL = struct module Inner = struct include M and N and L end end;;

let () =
  let module D = struct module I = struct include MNL end end in
  test "Two-level nested" D.I.Inner.[x;y;z];;

let () =
  let module I : S_XYZ  = struct include  M[@local][@attribute] and N and L end in
  test "Local" I.[x;y;z];;

let () =
  let module I : S_XYZ  =
    struct
      include Id(M) and N and L;;
      test "Nested_global" [x;y;z]
    end in
  ();;

include Id(M)[@att] and N and L [@@item];;
let () =
  test "Global" [x;y;z];;
