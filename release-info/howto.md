These are informal notes on how to do an OCaml release.

Following these steps requires commit right in the OCaml repository,
as well as SSH access to the inria.fr file servers hosting the
distribution archives and manual.

We are not fully confident that those steps are correct, feel free to
check with other release managers in case of doubt.

Note: we say that a new release is a "testing release" if it is a Beta
version or Release Candidate. Otherwise, we call it a "production
release".


## A few days in advance

Send a mail on caml-devel to warn Gabriel (to make a pass on Changes;
see the "Changes curation" appendix for more details) and the
OCamlLabs folks (for OPAM testing).

## 0: release environment setup

```
rm -f /tmp/env-$USER.sh
cat >/tmp/env-$USER.sh <<EOF
# Update the data below
export MAJOR=4
export MINOR=12
export BUGFIX=0
export PLUSEXT=

# names for the release announce
export HUMAN=

# do we need to use tar or gtar?
export TAR=tar

export WORKTREE=~/o/\$MAJOR.\$MINOR
  # must be the git worktree for the branch you are releasing

export BRANCH=\$MAJOR.\$MINOR
export VERSION=\$MAJOR.\$MINOR.\$BUGFIX\$PLUSEXT
export TAGVERSION=\`echo \$VERSION | sed s/\~/-/g\`

export REPO=https://github.com/ocaml/ocaml

# these values are specific to caml.inria's host setup
# they are defined in the release manager's .bashrc file
export ARCHIVE_HOST="$OCAML_RELEASE_ARCHIVE_HOST"
export ARCHIVE_PATH="$OCAML_RELEASE_ARCHIVE_PATH"
export WEB_HOST="$OCAML_RELEASE_WEB_HOST"
export WEB_PATH="$OCAML_RELEASE_WEB_PATH"

export DIST="\$ARCHIVE_PATH/ocaml/ocaml-\$MAJOR.\$MINOR"
export INSTDIR="/tmp/ocaml-\$VERSION"


EOF
source /tmp/env-$USER.sh
echo $VERSION
```


## 1: check repository state

```
cd $WORKTREE
git checkout $MAJOR.$MINOR
git status  # check that the local repo is in a clean state
git pull
```

## 2: magic numbers

If you are about to do a major release, you should check that the
magic numbers have been updated since the last major release. It is
preferable to do this just before the first testing release for this
major version, typically the first beta.

See the `utils/HACKING.adoc` file for documentation on how to bump the
magic numbers.

## 3: build, refresh dependencies, sanity checks

```
make distclean
git clean -n -d -f -x  # Check that "make distclean" removed everything

rm -rf ${INSTDIR}
./configure --prefix=${INSTDIR}

make -j5

# Check that dependencies are up-to-date
make alldepend

git diff
# should have empty output

# check that .depend files have no absolute path in them
find . -name .depend | xargs grep ' /'
  # must have empty output

# Run the check-typo script
./tools/check-typo


make install
./tools/check-symbol-names runtime/*.a
  # must have empty output and return 0
```


## 4: tests

```
make tests
```


## 5: build, tag and push the new release

```
# at this point, the VERSION file contains N+devD
# increment it into N+dev(D+1); for example,
#   4.07.0+dev8-2018-06-19 => 4.07.0+dev9-2018-06-26
# for production releases: check and change the Changes header
#  (remove "next version" and add a date)
make -B configure
git commit -a -m "last commit before tagging $VERSION"

# update VERSION with the new release; for example,
#   4.07.0+dev9-2018-06-26 => 4.07.0+rc2
# Update ocaml-variants.opam with new version.
# Update \year in manual/src/macros.hva
make -B configure
# For a production release
make coreboot -j5
make coreboot -j5 # must say "Fixpoint reached, bootstrap succeeded."
git commit -m "release $VERSION" -a
git tag -m "release $VERSION" $TAGVERSION

# for production releases, change the VERSION file into (N+1)+dev0; for example,
#   4.08.0 => 4.08.1+dev0
# for testing candidates, use N+dev(D+2) instead; for example,
#   4.07.0+rc2 => 4.07.0+dev10-2018-06-26
# Revert ocaml-variants.opam to its "trunk" version.
make -B configure
git commit -m "increment version number after tagging $VERSION" VERSION configure ocaml-variants.opam
git push
git push --tags
```

## 5-bis: Alternative for branching

This needs to be more tested, tread with care.
```
# at this point, the VERSION file contains N+devD
# increment it into N+dev(D+1); for example,
#   4.07.0+dev0-2018-06-19 => 4.07.0+dev1-2018-06-26
# Rename the "Working version" header in Changes
# to "OCaml $BRANCH"
make -B configure
git commit -a -m "last commit before branching $BRANCH"
git branch $BRANCH

# update VERSION with the new future branch,
#   4.07.0+dev1-2018-06-26 => 4.08.0+dev0-2018-06-30
# Update ocaml-variants.opam with new version.
make -B configure
# Add a "Working version" section" to Changes
# Add common subsections in Changes, see Changelog.
git commit -m "first commit after branching $BRANCH" -a
git push

# Switch to the new branch
git checkout $BRANCH
# increment VERSION, for instance
#   4.07.0+dev1-2018-06-26 => 4.07.0+dev2-2018-06-30
make -B configure
git commit -m "first commit on branch $BRANCH" -a
git push --set-upstream origin $BRANCH
```

Adjust github branch settings:

Go to
  https://github.com/ocaml/ocaml/settings/branches
and add a rule for protecting the new branch
(copy the rights from the previous version)

## 5.1: create the release on github (only for a production release)

open https://github.com/ocaml/ocaml/releases
# and click "Draft a new release"
# for a minor release, the description is:
 Bug fixes. See [detailed list of changes](https://github.com/ocaml/ocaml/blob/$MAJOR.$MINOR/Changes).

## 5.3: Inria CI (for a new release branch)

Add the new release branch to the Inria CI list.
Remove the oldest branch from this list.

## 5.4 new badge in README.adoc (for a new release branch)

Add a badge for the new branch in README.adoc.
Remove the oldest badge.

## 6: create OPAM packages

Clone the opam-repository
```
git clone https://github.com/ocaml/opam-repository
```

Create a branch for the new release
```
git checkout -b OCaml_$VERSION
```

The following opam packages are needed for all releases:

- `ocaml-base-compiler.$VERSION`
- `ocaml-variants.$VERSION+options`

For production release, the following packages need to be updated:

- `ocaml-system.$VERSION`
- `ocaml-src.$VERSION`
- `ocaml-src.$MAJOR.$MINOR.dev`
- `ocaml-manual.$VERSION`
- `ocaml.$NEXTVERSION`
- `ocaml-variants.$NEXTVERSION+trunk` should be moved to
   `ocaml-variants.$NEXTNEXTVERSION+trunk`

Note that the `ocaml` virtual package needs to be updated to the next version.

Similarly, the `ocurrent/ocaml-version` library should be updated.

Do not forget to add/update the checksum field for the tarballs in the
"url" section of the opam files. Use opam-lint before sending the pull
request.

You can test the new opam package before sending a PR to the
main opam-repository by using the local repository:

```
opam repo add local /path/to/your/opam-repository
opam switch create --repo=local,beta=git+https://github.com/ocaml/ocaml-beta-repository.git ocaml-variants.$VERSION
```
The switch should build.

For a production release, you also need to create new opam files for the ocaml-manual and
ocaml-src packages.

## 6.1 Update OPAM dev packages after branching

Create a new ocaml/ocaml.$NEXT/opam file.
Copy the opam dev files from ocaml-variants/ocaml-variants.$VERSION+trunk*
into ocaml-variants/ocaml-variants.$NEXT+trunk+* .
Update the version in those opam files.

Update the synopsis and "src" field in the opam $VERSION packages.
The "src" field should point to
 src: "https://github.com/ocaml/ocaml/archive/$VERSION.tar.gz"
The synopsis should be "latest $VERSION development(,...)".


## 7: build the release archives

```
cd $WORKTREE
TMPDIR=/tmp/ocaml-release
git checkout $TAGVERSION
git checkout-index -a -f --prefix=$TMPDIR/ocaml-$VERSION/
cd $TMPDIR
$TAR -c --owner 0 --group 0 -f ocaml-$VERSION.tar ocaml-$VERSION
gzip -9 <ocaml-$VERSION.tar >ocaml-$VERSION.tar.gz
xz <ocaml-$VERSION.tar >ocaml-$VERSION.tar.xz
```

## 8: upload the archives and compute checksums

For the first beta of a major version, create the distribution directory on
the server:
```
ssh $ARCHIVE_HOST "mkdir -p $DIST"
```

Upload the archives:
```
scp ocaml-$VERSION.tar.{xz,gz} $ARCHIVE_HOST:$DIST
```

To update the checksum files on the remote host, we first upload the
release environment.
(note: this assumes the user name is the same on the two machines)

```
scp /tmp/env-$USER.sh $ARCHIVE_HOST:/tmp/env-$USER.sh
```

and then login there to update the checksums (MD5SUM, SHA512SUM)

```
ssh $ARCHIVE_HOST
source /tmp/env-$USER.sh
cd $DIST

cp MD5SUM MD5SUM.old
md5sum ocaml-$VERSION.tar.{xz,gz} > new-md5s
# check new-md5s to ensure that they look right, and then
cat new-md5s >> MD5SUM
# if everything worked well,
rm MD5SUM.old new-md5s

# same thing for SHA512
cp SHA512SUM SHA512SUM.old
sha512sum ocaml-$VERSION.tar.{xz,gz} > new-sha512s
cat new-sha512s >> SHA512SUM
rm SHA512SUM.old new-sha512s

# clean up
rm /tmp/env-$USER.sh
exit
```


## 9: update note files (technical documentation)

```
ssh $ARCHIVE_HOST "mkdir -p $DIST/notes"
cd ocaml-$VERSION
scp INSTALL.adoc LICENSE README.adoc README.win32.adoc Changes \
   $ARCHIVE_HOST:$DIST/notes/
```


## 10: upload the reference manual

You don't need to do this if the previous release had the same
$MAJOR.$MINOR ($BRANCH) value and the exact same manual -- this is frequent if
it was a release candidate.

```
cd $WORKTREE
make
make install
export PATH="$INSTDIR/bin:$PATH"
cd manual
make clean
make
rm -rf /tmp/release
mkdir -p /tmp/release
RELEASENAME="ocaml-$BRANCH-"
make -C manual release RELEASE=/tmp/release/$RELEASENAME
scp /tmp/release/* $ARCHIVE_HOST:$DIST/


# upload manual checksums
ssh $ARCHIVE_HOST "cd $DIST; md5sum ocaml-$BRANCH-refman* >>MD5SUM"
ssh $ARCHIVE_HOST "cd $DIST; sha512sum ocaml-$BRANCH-refman* >>SHA512SUM"
```

Releasing the manual online happens on another machine:
Do this ONLY FOR A PRODUCTION RELEASE

```
scp /tmp/env-$USER.sh $ARCHIVE_HOST:/tmp/env-$USER.sh
ssh $ARCHIVE_HOST
source /tmp/env-$USER.sh
scp /tmp/env-$USER.sh $WEB_HOST:/tmp
ssh $WEB_HOST
source /tmp/env-$USER.sh

cd $WEB_PATH/caml/pub/docs
mkdir -p manual-ocaml-$BRANCH
cd manual-ocaml-$BRANCH
rm -fR htmlman ocaml-$BRANCH-refman-html.tar.gz
wget http://caml.inria.fr/pub/distrib/ocaml-$BRANCH/ocaml-$BRANCH-refman-html.tar.gz
tar -xzvf ocaml-$BRANCH-refman-html.tar.gz # this extracts into htmlman/
/bin/cp -r htmlman/* . # move HTML content to docs/manual-caml-$BRANCH
rm -fR htmlman ocaml-$BRANCH-refman-html.tar.gz

cd $WEB_PATH/caml/pub/docs
rm manual-ocaml
ln -sf manual-ocaml-$BRANCH manual-ocaml
```


## 11: prepare web announce for the release

For production releases, you should get in touch with ocaml.org to
organize the webpage for the new release. See

  <https://github.com/ocaml/ocaml.org/issues/819>


## 13: announce the release on caml-list, caml-announce, and discuss.ocaml.org

See the email announce templates in the `templates/` directory.



# Appendix

## Announce templates

See

- templates/beta.md for alpha and beta releases
- templates/rc.md for release candidate
- templates/production.md for the production release


## Changelog template for a new version

A list of common subsection for the "Changes" file:

```
### Language features:

### Runtime system:

### Code generation and optimizations:

### Standard library:

### Other libraries:

### Tools:

### Manual and documentation:

### Compiler user-interface and warnings:

### Internal/compiler-libs changes:

### Build system:

### Bug fixes:
```


## Changes curation

Here is the process that Gabriel uses to curate the Changes entries of
a release in preparation. Feel free to take care of it if you wish.

(In theory it would be possible to maintain the Changes in excellent
 shape so that no curation would be necessary. In practice it is less
 work and less friction to tolerate imperfect Changes entries, and
 curate them before the release.)

### Synchronizing the trunk Changes with release branches

The Changes entries of a release branch or past release should be
exactly included in the trunk Changes, in the section of this release
(or release branch). Use an interactive diffing tool (for example
"meld") to compare and synchronize the Changes files of trunk and
release branches.

Here are typical forms of divergence and their usual solutions:

- A change entry is present in a different section in two branches.
  (Typically: in the XX.YY section of the XX.YY release branch,
  but in the trunk section of the trunk branch.)

  This usually happens when the PR is written for a given branch
  first, and then cherry-picked in an older maintenance branch, but
  the cherry-picker forgets to move the Change entry in the first
  branch.

  Fix: ensure that the entry is in the same section on all branches,
  by putting it in the "smallest" version -- assuming that all bigger
  versions also contain this change.

- A change entry is present in a given section, but the change is not
  present in the corresponding release branch.

  There are two common causes for this with radically different solutions:

  + If a PR is merged a long time after they were submitted, the merge
    may put their Changes entry in the section of an older release,
    while it should go in trunk.

    Fix: in trunk, move the entry to the trunk section.

  + Sometimes the author of a PR against trunk intends it to be
    cherry-picked in an older release branch, and places it in the
    corresponding Changes entry, but we forget to cherry-pick.

    Fix: cherry-pick the PR in the appropriate branch.

  Reading the PR discussion is often enough to distinguish between the
  two cases, but one should be careful before cherry-picking in
  a branch (for an active release branch, check with the release
  manager(s)).

Figuring out the status of a given Changes entry often requires
checking the git log for trunk and branches. Grepping for the PR
number often suffices (note: when you cherry-pick a PR in a release
branch, please target the merge commit to ensure the PR number is
present in the log), or parts of the commit message text.

### Ensure each entry is in the appropriate section

(of course)

### Fill more details in unclear Changes entries

Expert users want to learn about the changes in the new release. We
want to avoid forcing them to read the tortuous PR discussion, by
giving enough details in the Changes entry.

In particular, for language changes, showing a small example of
concrete syntax of the new feature is very useful, and giving a few
words of explanations helps.

Compare for example

    - #8820: quoted string extensions
      (Gabriel Radanne, Leo White and Gabriel Scherer,
       request by Bikal Lem)

with

    - #8820: quoted extensions: {%foo|...|} is lighter syntax for
      [%foo {||}], and {%foo bar|...|bar} for [%foo {bar|...|bar}].
      (Gabriel Radanne, Leo White and Gabriel Scherer,
       request by Bikal Lem)

This is also important for changes that break compatibility; users
will scrutinize them with more care, so please give clear information on
what breaks and, possibly, recommended update methods.

Having enough details is also useful when you will grep the Changes
later to know when a given change was introduced (knowing what to grep
can be difficult).

### Ordering of Changes entries

In the past, we would order Changes entries numerically (this would
also correspond to a chronological order). Since 4.09 Gabriel is
trying to order them by importance (being an exciting/notable feature
for a large number of users). What is the best ordering of sections,
and the best entry ordering within a section, to put the most
important changes first? This is guesswork of course, and we commonly
have a long tail of "not so important PRs" in each section which don't
need to be ordered with respect to each other -- one may break two
lines just before this long tail.

The ordering of sections depends on the nature of the changes within
the release; some releases have an exciting "Runtime" section, many
release don't. Usually "Language features" is among the first, and
"Bug fixes" is the very last (who cares about bugs, right?).

If some entries feel very anecdotal, consider moving them to the Bug
Fixes section.

### Extract release highlights to News

From time to time, synchronize the `News` file with the release highlights
of each version.
