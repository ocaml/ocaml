# OCaml patches
Patches placed in this directory are applied by `configure` (or by
`Makefile.nt`) and appear in the `patches:` item of `ocamlc -config`.

Patch files must have a `.patch` extension and the name must lowercase and
consist of letters, digits and the `#`, `/`, `+`, `.` and `-` symbols only.
Other files will be ignored. The patterns `mpr#nnnn` and `gpr#nnnn` should be
reserved for patches implementing functionality of a Mantis or GitHub PR for the
[main repository](https://github.com/ocaml/ocaml).

Patches are applied from the root directory with `-p1`.

Alternatively, if you patch the OCaml sources using a different mechanism, touch
`patches/name.patched` for `name` to be included in the list of applied patches.
