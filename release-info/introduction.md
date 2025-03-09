# What does an OCaml version mean?

OCaml releases follow a linux-like scheme for their version string. The
OCaml version string consists in three numbers, optionally followed by
either a prerelease or development tag
(`%i.%i.%i[~alpha%i|~beta%i|~rc%i|+%s]`). For example, `4.14.1`,
5.1.0~alpha2 and 5.3.0+dev0-2023-12-22 are valid OCaml versions.

- The first version number (`4` in `4.14.1`) is the major version of OCaml.
  This version number is updated when major new features are added to the OCaml
  language. For instance, OCaml 5 added shared memory parallelism and effect
  handlers and OCaml 4 introduced GADTs (Generalised Abstract Data Types).

- The second version number (`14` in` 4.14.1`) is the minor version of OCaml.
  This number is increased for every new release of OCaml. In particular, a new
  minor version of OCaml can contain breaking changes. However, we strive to
  maintain backward compatibility as much as possible.

- The last number (`1` in `4.14.1`) is the bugfix number.
  Updating to the latest bugfix release is always safe, those bugfix versions
  are meant to be completely backward compatible and only contain important or
  very safe bug fixes.

- The prerelease tag `~alpha%i`, `~beta%i`, `~rc%i` (`~alpha2` in
  `5.1.0~alpha2`) describes a prerelease version of the compiler that is
  currently being tested. See [below](#prerelease-versions) for
  a more thorough explanation.

- The development tag `+tag` indicates a development or experimental version of
  the compiler. `+dev0-2023-12-22` in `5.3.0+dev0-2023-12-22` is an example of the
  tags of the form `+dev%i-%date` used by the compiler for its development
  versions.


# When are new versions released?

Since OCaml 4.03, we are using a time-based release schedule:
a new minor version of OCaml is released every six months.

For instance, at the date of writing, the next planned releases of OCaml are:

- OCaml 5.3: around October 2024
- OCaml 5.4: around April 2025

The timing is approximate, as we often delay a release to ensure quality when
unforeseen issues come up. In consequence, releases are often late, typically by
up to two months. There is a prospective release calendar available at
[calendar.md](calendar.md)

We may release bugfix releases at any time.


# What happens between minor releases?

## Feature freeze

All PRs go to the development branch of the compiler, called 'trunk'. (This was
the standard name in the SVN era, and it remains more descriptive than 'main'.)

Three months before a new release, that is, at the half of the time window
between two releases, we create a separate branch for the next version of
OCaml. The intention (there are always exceptions) is that the release should
correspond to the state of 'trunk' at the time this branch was created, but we
wait three more months for quality analysis, to get feedback and integrate
bugfixes.

We do not integrate new features in the release branch, to avoid unplanned
regressions coming from last-minute changes. Only bugfixes and documentation
improvements go to the release branch -- by cherry-picking them from 'trunk'.

We do not have the resources to maintain more than one dev branch, one prelease
branch, and [one exceptional LTS branch](#Exceptional-LTS-versions).

### Example

5.1.0 was released in September 2023, and the feature freeze for 5.2 happened on
December 2023, for a 5.2 release planned around April 2024.

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

Our idea for alpha, beta, and rc releases is to have releases with increasing
stability guarantees, and which can be tested by an increasingly wider audience:

- `alpha` releases:
   * nearly stable internal compiler-libs API, only API fixes accepted
   * no new features, problematic features might be removed at this stage
   * bug fixes very welcome
   * documentation PRs still accepted
   * intended for core development tools (merlin, ppxlib, dune) to unlock the rest
     of the opam ecosystem

- `beta` releases:
  * stable internal compiler-libs API
  * stable set of features
  * bug fixes very welcome
  * documentation PRs still accepted
  * intended for early adopters: opam library authors should be able to test their
    libraries at this point.

- `rc` releases:
  * stable internal compiler-libs API
  * stable feature sets
  * only emergency bug fixes
  * documentation PRs postponed to after the release
  * intended for wide testing (and detecting deployment or production issues among
    large private code base)

Starting from the first alpha release, there is a small team effort to try to
build and test every package available on opam with the new release, with the
bulk of the work done by Kate Deplaix. This has been incredibly useful in the
past to catch bugs or usability regressions.

## Bugfix versions

Bugfix versions are published if we discover issues that significantly impede
the use of the initially released version. In that situation, it is not uncommon
that we backport safe bug fixes that were integrated in the trunk after the
release.

Most bugfix releases are M.m.1 releases that happened one or two months after
the M.m.0 minor release, to fix an important issue that was not found during
prerelease testing.

Users are strongly encouraged to switch to the last bugfix versions as soon as
possible. We make this easy by doing our best to avoid any regression there.


# Exceptional LTS versions

Switching from OCaml 4 to OCaml 5 required a full rewrite of the OCaml runtime.
This has negatively affected the stability of the releases of OCaml 5 in term of

- supported architectures
- supported OS
- performance stability
- number of runtime bugs

To keep a stable version easily available, we are exceptionally maintaining
OCaml 4.14 as a long term support version of OCaml. New bugfix versions of OCaml
4.14 will be released in the future until OCaml 5 is considered mature enough.

User feedback is welcome on which fixes from OCaml 5 should be also included in
4.14.

Once OCaml 5 is stabilized, this extended support of OCaml 4.14  will stop.
Currently, we expect to support OCaml 4.14 until OCaml 5.4 (around April 2025).


# How are new versions of OCaml released?

The release process is documented at [howto.md](howto.md).
