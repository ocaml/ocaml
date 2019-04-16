# Conflicts

Some of the conflicts and issues in the grammar are documented here.

## A variant type that lists a single atomic type

Why can't `[t]` be considered a valid atomic type? (A variant type.)

(This is related to MPR #3835.)

A class type that begins with `[t] foo` could continue as follows:

```
  [t] foo -> <class_type>
```

Here `t` is understood as a variant type,
and is used as an actual parameter of the parameterized type `'a foo`.

Or it could continue as follows:

```
  [t] foo
```

Here `t` is a type (there is no variant type)
and is used as an actual parameter of the class `['a] foo`.

After we have read the closing bracket and are looking ahead at `foo`,
we need to decide which of the above two situations we have. (The first
situation requires a reduction; the second situation requires shifting.)
But we cannot decide yet; we would need to look at the arrow `->` beyond
`foo` in order to decide. In this example LR(2) is required; in general,
`foo` could be replaced with an arbitrary qualified name, so unbounded
lookahead is required.

As a result of this issue, we must abandon the idea that `[t]` could be
a well-formed variant type. In the syntax of atomic types, instead of:

```
  atomic_type: LBRACKET row_field RBRACKET
```

we must use the more restricted form:

```
  atomic_type: LBRACKET tag_field RBRACKET
```

In other words, we rule out exactly the following:

```
  atomic_type: LBRACKET atomic_type RBRACKET
```
