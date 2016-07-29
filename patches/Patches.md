# OCaml patches
The `patches:` line of `ocamlc -config` will display the basename of every file
in this directory with a `.patched` extension which is all lowercase and
consists of letters, digits and the `#`, `/`, `+`, `.` and `-` symbols only.
Other files will be ignored. The patterns `mpr#nnnn` and `gpr#nnnn` should be
reserved for patches implementing functionality of a Mantis or GitHub PR for the
[main repository](https://github.com/ocaml/ocaml).

This system was introduced in OCaml 4.04.0, but may be retrospectively applied
to OCaml 3.09.0&ndash;3.11.2 by running:

```
patch -p1 -i tools/gpr#465-3.09.0+.patch
```

or for OCaml 3.12.0&ndash;4.03.0 by running:

```
patch -p1 -i tools/gpr#465-3.12.0+.patch
```
