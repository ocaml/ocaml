/* Nats are represented as unstructured blocks with tag Nat_tag. */

#define Nat_tag Abstract_tag    /* works OK with equal because no other
                                   object uses that tag yet. */

#define Bignum_val(nat) ((BigNum) nat)

