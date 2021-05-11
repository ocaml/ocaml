(* TEST

flags = "-w A-3-44-70 -alert +polymorphic_comparison"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let _a = compare
let _b = 1 = 1
let _c = 1 <> 1
let _d = 1 >= 1
let _e = 1 > 1
let _f = 1 <= 1
let _g = 1 < 1
let _h = min 1 1
let _i = max 1 1
let _j = List.mem
let _k = List.assoc
let _l = List.assoc_opt
let _m = List.mem_assoc
let _n = List.remove_assoc

let _a = Stdlib.compare
let _b = Stdlib.(1 = 1)
let _c = Stdlib.(1 <> 1)
let _d = Stdlib.(1 >= 1)
let _e = Stdlib.(1 > 1)
let _f = Stdlib.(1 <= 1)
let _g = Stdlib.(1 < 1)
let _h = Stdlib.min 1 1
let _i = Stdlib.max 1 1
let _j = Stdlib.List.mem
let _k = Stdlib.List.assoc
let _l = Stdlib.List.assoc_opt
let _m = Stdlib.List.mem_assoc
let _n = Stdlib.List.remove_assoc

let _a = Stdlib.Pervasives.compare
let _b = Stdlib.Pervasives.(1 = 1)
let _c = Stdlib.Pervasives.(1 <> 1)
let _d = Stdlib.Pervasives.(1 >= 1)
let _e = Stdlib.Pervasives.(1 > 1)
let _f = Stdlib.Pervasives.(1 <= 1)
let _g = Stdlib.Pervasives.(1 < 1)
let _h = Stdlib.Pervasives.min 1 1
let _i = Stdlib.Pervasives.max 1 1

let _a = Pervasives.compare
let _b = Pervasives.(1 = 1)
let _c = Pervasives.(1 <> 1)
let _d = Pervasives.(1 >= 1)
let _e = Pervasives.(1 > 1)
let _f = Pervasives.(1 <= 1)
let _g = Pervasives.(1 < 1)
let _h = Pervasives.min 1 1
let _i = Pervasives.max 1 1

let _a = Hashtbl.find
let _b = Hashtbl.find_opt
let _c = Hashtbl.find_all
let _d = Hashtbl.mem
let _e = Hashtbl.remove
let _f = Hashtbl.replace
let _g = Hashtbl.replace_seq
let _h = Hashtbl.of_seq

let test [@ocaml.alert polymorphic_comparison "use <monomorphic function> instead"] = ()
let _o = test

[@@@ocaml.alert "-polymorphic_comparison"]

let _a = compare
let _b = 1 = 1
let _c = 1 <> 1
let _d = 1 >= 1
let _e = 1 > 1
let _f = 1 <= 1
let _g = 1 < 1
let _h = min 1 1
let _i = max 1 1
let _j = List.mem
let _k = List.assoc
let _l = List.assoc_opt
let _m = List.mem_assoc
let _n = List.remove_assoc
let _o = Hashtbl.find
let _p = Hashtbl.find_opt
let _q = Hashtbl.find_all
let _r = Hashtbl.mem
let _s = Hashtbl.remove
let _t = Hashtbl.replace
let _u = Hashtbl.replace_seq
let _v = Hashtbl.of_seq
