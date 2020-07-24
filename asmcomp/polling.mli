val instrument_fundecl : future_funcnames:Misc.Stdlib.String.Set.t
    -> Mach.fundecl -> Mach.fundecl

val requires_prologue_poll : future_funcnames:Misc.Stdlib.String.Set.t
    -> Mach.instruction -> bool
