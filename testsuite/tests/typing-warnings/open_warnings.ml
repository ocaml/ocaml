module T1 : sig end = struct
  module M = struct type t end  (* unused type t *)
  open M  (* unused open *)
end;;

module T2 : sig type s end = struct
  module M = struct type t end
  open M  (* used by line below *)
  type s = t
end;;

module T3 : sig end = struct
  type t0 = A  (* unused type and constructor *)
  module M = struct type t = A end
  open M (* used by line below; shadow constructor A *)
  let _ = A (* A belongs to several types *)
end;;

module T4 : sig end = struct
  type t0 = A
  module M = struct type t = A end (* unused type and constructor *)
  open M (* unused open; no shadowing (A below refers to the one in t0) *)
  let _ : t0 = A (* disambiguation used *)
end;;

module T5 : sig end = struct
  type t0 = A (* unused type and constructor *)
  module M = struct type t = A end
  open M (* shadow constructor A *)
  let _ : t = A
end;;


module T1_bis : sig end = struct
  module M = struct type t end  (* unused type t *)
  open! M  (* unused open *)
end;;

module T2_bis : sig type s end = struct
  module M = struct type t end
  open! M  (* used by line below *)
  type s = t
end;;

module T3_bis : sig end = struct
  type t0 = A  (* unused type and constructor *)
  module M = struct type t = A end
  open! M (* used by line below; shadow constructor A (disabled) *)
  let _ = A (* A belongs to several types *)
end;;

module T4_bis : sig end = struct
  type t0 = A
  module M = struct type t = A end (* unused type and constructor *)
  open! M (* unused open; no shadowing (A below refers to the one in t0) *)
  let _ : t0 = A (* disambiguation used *)
end;;

module T5_bis : sig end = struct
  type t0 = A (* unused type and constructor *)
  module M = struct type t = A end
  open! M (* shadow constructor A (disabled) *)
  let _ : t = A
end;;
