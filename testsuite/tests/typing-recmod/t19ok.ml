(* PR 4758, PR 4266 *)

module PR_4758 = struct
  module type S = sig end
  module type Mod = sig
    module Other : S
  end
  module rec A : S = struct
  end and C : sig include Mod with module Other = A end = struct
    module Other = A
  end
end
