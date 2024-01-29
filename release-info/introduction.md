# What does an OCaml version number mean?

OCaml releases follow a linux-like scheme for its version string. The OCaml
version string consists in three numbers, optionally followed by either a
prerelease or development tag (`%i.%i.%i[~alpha%i|~beta%i|~rc%i|+%s]`).

- The first version number is the major version of OCaml.
  This version number is updated when major new features are added to the OCaml
  language. For instance, OCaml 5 added shared memory parallelism and effect
  handlers and OCaml 4 introduced GADTs (Generalised Abstract Data Types).

- The second version number is the minor version of OCaml.
  This number is increased for every new release of OCaml. In particular, a new
  minor version of OCaml can contain breaking changes. However, we strive to
  maintain backward compatibility as much as possible.

- The last number is the bugfix number.
  Updating to the latest bugfix release is always safe, those bugfix versions
  are meant to be completely backward compatible and only contain important or
  very safe bug fixes.

- The prerelease tag `~alpha%i`, `~beta%i`, `~rc%i` describes a prerelease
  version of the compiler that is currently being tested. See [below](##
  Prerelease versions) for a more thorough explanation.

- The development tag `+tag` indicates a development or experimental version of
  the compiler.
  For instance, the compiler uses `+dev%i-%i-%i-%i` for its development
  versions.

# When new versions are released?

Since OCaml 4.03, we are using a time-based release schedule:
a new (minor) version of OCaml is released every six months.

At the date of writing, the next planned releases of OCaml are:

- OCaml 5.2: around April 2024
- OCaml 5.3: around October 2024

Our past experiences are that releases are often late however.


# What happen between releases?

##  Feature freeze

Three months before a new release, we create a separate branch for the next
version of OCaml. This new branch is feature frozen: between the branching and
the official release of this branch, only bug fixes (and documentation updates)
are merged in this branch.

We sometimes make an exception for specific PRs with a nearly finished review
at the time of the freeze, but it is better to not rely on such review extension.

During this period, new features are still accepted in the development branch of
the compiler (called trunk for historical reasons).

The objective of those dual branches is to stabilise the released versions of
OCaml, while still integrating new features in the trunk branch, and
acknowledging that we don't have the resources to maintain more than one dev
branch, one prelease branch, and one LTS branch.


At the date of writing, the last feature freeze was the one for

- OCaml 5.2, on December 2023

and the next planned feature freeze should be:

- OCaml 5.3 feature freeze: around July 2024
- OCaml 5.4 feature freeze: around January 2025

## Prerelease versions

Once a new version branch has been created, and the feature set has been
stabilised, we start publishing prerelease versions of this branch.

For instance, after branching OCaml 5.2, we start publishing alpha versions: for
instance `5.2.0~alpha1`, then `5.2.0~alpha2`.

Once core development tools have been ported to work on those alpha versions, we
switch to releasing beta versions (e.g. `5.2.0~beta1`).

At last, just before the official release, we publish a release candidate (e.g.
`5.2.0~rc1`) to have one last check with the hopefully final version
of the branch.

Our idea for alpha, beta, and rc releases is to have release with increasing
stability guarantees, and which can be tested by an increasingly wider audience:

- `alpha` releases:
   * nearly stable internal compiler-libs API, only API fixes accepted
   * no new features, problematic features might be removed at this stage
   * bug fixes very welcome
   * documentation PRs still accepted
   * aimed to core development tools (merlin, ppxlib, dune) to unlock the rest
     of the opam ecosystem

- `beta` releases:
  * stable internal compiler-libs API
  * stable set of features
  * bug fixes very welcome
  * documentation PRs still accepted
  * aimed to early adopters: opam library authors should be able to test their
    libraries at this point.

- `rc` releases:
  * stable internal compiler-libs API
  * stable feature sets
  * only emergency bug fixes
  * documentation PRs postponed to after the release
  * aimed to wide tests (and detecting deployment or production issues among
    large private code base)

Starting from the first alpha release, there is a small team effort to try to
test every package available on opam with the new release. This has been
incredibly useful in the past to catch bugs or usability regressions.

## Bugfix versions

Bugfix versions are published if we discover issues that significantly impede
the use of the initially released version. In that situation, it is not uncommon
that we backport safe bug fixes that were integrated in the trunk after the
release.

Users are strongly encouraged to switch to the last bugfix versions as soon as
possible.


## LTS versions

Switching from OCaml 4 to OCaml 5 required a full rewrite of the OCaml runtime.
This has negatively affected the stability of the releases of OCaml 5 in term of

- supported architectures
- supported OS
- performance stability
- number of runtime bugs

To keep a stable version easily available, we are still maintaining OCaml 4.14
as the long term support version of OCaml. New bugfix versions of OCaml 4.14
will be released in the future until OCaml 5 is considered mature enough.

Currently, we expect to support OCaml 4.14 until OCaml 5.4 (around April 2025).

# How new versions of OCaml are released?

See [the release howto](https://github.com/ocaml/ocaml/release-info/howto.md)
