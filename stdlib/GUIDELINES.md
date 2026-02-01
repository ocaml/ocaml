# OCaml Stdlib design guidelines

The OCaml standard library is developed according to the following
design guidelines.

1. **Scope**. The standard library provides support for the linguistic
   idioms OCaml supports or promotes. It also establishes the minimal
   common ground for interoperatibility that enables the software
   libraries for today's and tomorrow's technologies to be built by
   language users in a usable, lean, and modular way.

2. **Consistency**. The standard library strives for consistency of idioms
   and naming. However given its long past and future history,
   inconsistencies are expected. In particular, existing idioms and
   names that have proven unsatisfactory over time should not be used
   to justify the form of new additions.

3. **Pragmatism**. Since it is easy to design in the abstract unused or
   unfit functionality, the standard library mostly considers
   additions that are proven to work and successfully used in some form
   by language users. Additions that are made for the sake of
   consistency, regardless of usage, may not be welcomed.

4. **Ergonomics**. The standard library considers human factors as
   a cornerstone of its development. It favors excellent code reading with
   minimal cognitive overhead. It favors excellent code writing by luring
   programmers to ask themselves the right questions about the problem
   they are solving with the standard library.

5. **Generality**. The standard library aims to solve the common cases
   simply. Specialized needs may be addressed only if they do not
   compromise the ergonomics of the common cases.

6. **Naming**. The standard library obviously strives for good
   names. Names must be clear when used in context, they must provide
   a smooth reading flow and convey the intent of the
   code. Judging names in isolation or solely on their
   self-explanatory nature is rarely a good guide.

7. **Stability**. The standard library balances the needs of past, current
   and future users. For past users, the standard library avoids
   breaking changes unless major language changes occur. For current
   users, functionality may be rejected in the light of the long-term
   evolution of the language or standard library. For future users, this
   stance should convince them to become a current user.
