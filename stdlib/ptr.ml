type 'a t = nativeint

let offset : nativeint -> 'a t -> 'a t = Nativeint.(+) [@@inline]
external cast_ptr : 'a t -> 'b t = "%identity"
external to_nativeint : 'a t -> nativeint = "%identity"
external of_nativeint : nativeint -> 'a t = "%identity"
external aligned_load8 : 'a t -> char = "%load8"
external aligned_load16 : 'a t -> int = "%aligned_load16"
external aligned_load32 : 'a t -> int32 = "%aligned_load32"
external aligned_load64 : 'a t -> int64 = "%aligned_load64"
external aligned_loadnative : 'a t -> nativeint = "%aligned_loadnative"
external unaligned_load16 : 'a t -> int = "%unaligned_load16"
external unaligned_load32 : 'a t -> int32 = "%unaligned_load32"
external unaligned_load64 : 'a t -> int64 = "%unaligned_load64"
external unaligned_loadnative : 'a t -> nativeint = "%unaligned_loadnative"
