type 'a t;;
        (*
          The type of a set of weak pointers. 
        *)
val create: int -> 'a t;;
        (* [Weak.set_create len] creates a set of weak pointers which an
           initial size of [len] weak pointers. The set can contains more
           than [len] weak pointers at a time, but then the set will be 
           reallocated.
        *)
val add: 'a t -> 'a -> 'a t;;
        (* [Weak.set_add set obj] adds a weak pointer to [obj] to a set
           [set] of weak pointers and returns the new set. Indeed, if the
           set [set] is full, a new set is allocated.
        *)
val to_array: 'a t -> 'a array;;
        (* [Weak.set_array set] returns an array containing all life objects
           in the set [set].
        *)
val remove: 'a t -> 'a -> bool;;
        (* [Weak.set_remove set obj] removes [obj] from the set [set]. This
           operation can be expensive, since all the set must be scanned.
        *)
val info: 'a t -> int * int;;
        (* [Weak.set_info set] returns a couple ([size],[nobjs]) where
           [size] is the total size of [set] and [nobjs] the number of
           life objects in [set].
        *)
