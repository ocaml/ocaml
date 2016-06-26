module type S = sig
  type addressing_mode
  type specific_operation

  module Arch : Arch_intf.S with type addressing_mode = addressing_mode
                             and type specific_operation = specific_operation
  module Proc : Proc_intf.S with type addressing_mode = addressing_mode
                             and type specific_operation = specific_operation

  type fundecl = (addressing_mode, specific_operation) Mach.fundecl
  type linearize_fundecl = (addressing_mode, specific_operation) Linearize.fundecl

  module Reload : sig
    val fundecl: fundecl -> fundecl * bool
  end
  module Scheduling : sig
    val fundecl: linearize_fundecl -> linearize_fundecl
  end
  module Selection : sig
    val fundecl: Cmm.fundecl -> fundecl
  end
  module CSE : sig
    val fundecl: fundecl -> fundecl
  end
  module Emit : sig
    val fundecl: linearize_fundecl -> unit
    val data: Cmm.data_item list -> unit
    val begin_assembly: unit -> unit
    val end_assembly: unit -> unit
  end
end
