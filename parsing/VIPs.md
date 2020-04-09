# VIPs

A VIP is a common syntax error, for which a good error message should be
given.

## Structures versus signatures

Everything that is allowed in a structure but forbidden in a signature,
or vice-versa, is a VIP. For instance, writing:

```
  exception A = B
```

is allowed in a structure, but forbidden in a signature. (Here, we might
wish to make the error message depend on the lookahead token; the token
`=` suggests that the user confuses a structure and a signature.)

Similarly, writing `struct` where `sig` is expected, or vice-versa, is
probably a common mistake.
