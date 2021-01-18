#!/usr/bin/env bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*              Anil Madhavapeddy, OCaml Labs                             *
#*                                                                        *
#*   Copyright 2014 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

set -e

# TRAVIS_COMMIT_RANGE has the form   <commit1>...<commit2>
# TRAVIS_CUR_HEAD is <commit1>
# TRAVIS_PR_HEAD is <commit2>
#
# The following diagram illustrates the relationship between
# the commits:
#
#      (trunk)         (pr branch)
#  TRAVIS_CUR_HEAD   TRAVIS_PR_HEAD
#        |            /
#       ...         ...
#        |          /
#  TRAVIS_MERGE_BASE
#
echo "TRAVIS_COMMIT_RANGE=$TRAVIS_COMMIT_RANGE"
echo "TRAVIS_COMMIT=$TRAVIS_COMMIT"
if [[ $TRAVIS_EVENT_TYPE = 'pull_request' ]] ; then
  FETCH_HEAD=$(git rev-parse FETCH_HEAD)
  echo "FETCH_HEAD=$FETCH_HEAD"
else
  FETCH_HEAD=$TRAVIS_COMMIT
fi

if [[ $TRAVIS_EVENT_TYPE = 'push' ]] ; then
  if ! git cat-file -e "$TRAVIS_COMMIT" 2> /dev/null ; then
    echo 'TRAVIS_COMMIT does not exist - CI failure'
    exit 1
  fi
else
  if [[ $TRAVIS_COMMIT != $(git rev-parse FETCH_HEAD) ]] ; then
    echo 'WARNING! Travis TRAVIS_COMMIT and FETCH_HEAD do not agree!'
    if git cat-file -e "$TRAVIS_COMMIT" 2> /dev/null ; then
      echo 'TRAVIS_COMMIT exists, so going with it'
    else
      echo 'TRAVIS_COMMIT does not exist; setting to FETCH_HEAD'
      TRAVIS_COMMIT=$FETCH_HEAD
    fi
  fi
fi

set -x

PREFIX=~/local

MAKE="make $MAKE_ARG"
SHELL=dash

TRAVIS_CUR_HEAD=${TRAVIS_COMMIT_RANGE%%...*}
TRAVIS_PR_HEAD=${TRAVIS_COMMIT_RANGE##*...}
case $TRAVIS_EVENT_TYPE in
   # If this is not a pull request then TRAVIS_COMMIT_RANGE may be empty.
   pull_request)
     DEEPEN=50
     while ! git merge-base "$TRAVIS_CUR_HEAD" "$TRAVIS_PR_HEAD" >& /dev/null
     do
       echo "Deepening $TRAVIS_BRANCH by $DEEPEN commits"
       git fetch origin --deepen=$DEEPEN "$TRAVIS_BRANCH"
       ((DEEPEN*=2))
     done
     TRAVIS_MERGE_BASE=$(git merge-base "$TRAVIS_CUR_HEAD" "$TRAVIS_PR_HEAD");;
esac

CheckSyncStdlibDocs () {
  cat<<EOF
------------------------------------------------------------------------
This test checks that running tools/sync-stdlib-docs is a no-op in the current
state, which means that the labelled/unlabelled .mli files are in sync.  If
this check fails, it should be fixable by just running the script and reviewing
the changes it makes.
------------------------------------------------------------------------
EOF
  tools/sync_stdlib_docs
  git diff --quiet --exit-code && result=pass || result=fail
  case $result in
      pass)
          echo "CheckSyncStdlibDocs: success";;
      fail)
          echo "CheckSyncStdlibDocs: failure with the following differences:"
          git --no-pager diff
          exit 1;;
  esac
}

CheckDepend () {
  cat<<EOF
------------------------------------------------------------------------
This test checks that 'alldepend' target is a no-op in the current
state, which means that dependencies are correctly stored in .depend
files. It should only be run after the compiler has been built.
If this check fails, it should be fixable by just running 'make alldepend'.
------------------------------------------------------------------------
EOF
  ./configure --disable-dependency-generation \
              --disable-debug-runtime \
              --disable-instrumented-runtime
  # Need a runtime
  $MAKE -j coldstart
  # And generated files (ocamllex compiles ocamlyacc)
  $MAKE -j ocamllex
  $MAKE alldepend
  # note: we cannot use $? as (set -e) may be set globally,
  # and disabling it locally is not worth the hassle.
  # note: we ignore the whitespace in case different C dependency
  # detectors use different indentation styles.
  git diff --ignore-all-space --quiet --exit-code **.depend \
      && result=pass || result=fail
  case $result in
      pass)
          echo "CheckDepend: success";;
      fail)
          echo "CheckDepend: failure with the following differences:"
          git --no-pager diff --ignore-all-space **.depend
          exit 1;;
  esac
}

BuildAndTest () {
  mkdir -p $PREFIX
  cat<<EOF
------------------------------------------------------------------------
This test builds the OCaml compiler distribution with your pull request
and runs its testsuite.

Failing to build the compiler distribution, or testsuite failures are
critical errors that must be understood and fixed before your pull
request can be merged.
------------------------------------------------------------------------
EOF

  # Ensure that make distclean can be run from an empty tree
  $MAKE distclean

  if [ "$MIN_BUILD" = "1" ] ; then
    configure_flags="\
      --prefix=$PREFIX \
      --disable-shared \
      --disable-debug-runtime \
      --disable-instrumented-runtime \
      --disable-systhreads \
      --disable-str-lib \
      --disable-unix-lib \
      --disable-bigarray-lib \
      --disable-ocamldoc \
      --disable-native-compiler \
      --enable-ocamltest \
      --disable-dependency-generation \
      $CONFIG_ARG"
  else
    configure_flags="\
      --prefix=$PREFIX \
      --enable-flambda-invariants \
      --enable-ocamltest \
      --disable-dependency-generation \
      $CONFIG_ARG"
  fi
  case $XARCH in
  x64)
    ./configure $configure_flags
    ;;
  i386)
    ./configure --build=x86_64-pc-linux-gnu --host=i386-linux \
      CC='gcc -m32' AS='as --32' ASPP='gcc -m32 -c' \
      PARTIALLD='ld -r -melf_i386' \
      $configure_flags
    ;;
  *)
    echo unknown arch
    exit 1
    ;;
  esac

  export PATH=$PREFIX/bin:$PATH
  if [ "$MIN_BUILD" = "1" ] ; then
    if $MAKE world.opt ; then
      echo "world.opt is not supposed to work!"
      exit 1
    else
      $MAKE world
    fi
  else
    $MAKE world.opt
    $MAKE ocamlnat
  fi
  echo Ensuring that all names are prefixed in the runtime
  ./tools/check-symbol-names runtime/*.a
  cd testsuite
  echo Running the testsuite with the normal runtime
  $MAKE all
  if [ "$MIN_BUILD" != "1" ] ; then
    echo Running the testsuite with the debug runtime
    $MAKE USE_RUNTIME='d' OCAMLTESTDIR="$(pwd)/_ocamltestd" TESTLOG=_logd all
  fi
  cd ..
  if command -v pdflatex &>/dev/null  ; then
    echo Ensuring that all library documentation compiles
    $MAKE -C ocamldoc html_doc pdf_doc texi_doc
  fi
  $MAKE install
  if command -v hevea &>/dev/null ; then
    echo Ensuring that the manual compiles
    # These steps rely on the compiler being installed and in PATH
    $MAKE -C manual/manual/html_processing duniverse
    $MAKE -C manual web
  fi
  if fgrep 'SUPPORTS_SHARED_LIBRARIES=true' Makefile.config &>/dev/null ; then
    echo Check the code examples in the manual
    $MAKE manual-pregen
  fi
  # check_all_arches checks tries to compile all backends in place,
  # we would need to redo (small parts of) world.opt afterwards to
  # use the compiler again
  $MAKE check_all_arches
  # Ensure that .gitignore is up-to-date - this will fail if any untreacked or
  # altered files exist.
  test -z "$(git status --porcelain)"
  # check that the 'clean' target also works
  $MAKE clean
  $MAKE -C manual clean
  # check that the `distclean` target definitely cleans the tree
  $MAKE distclean
  $MAKE -C manual distclean
  # Check the working tree is clean
  test -z "$(git status --porcelain)"
  # Check that there are no ignored files
  test -z "$(git ls-files --others -i --exclude-standard)"
}

CheckChangesModified () {
  cat<<EOF
------------------------------------------------------------------------
This test checks that the Changes file has been modified by the pull
request. Most contributions should come with a message in the Changes
file, as described in our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#changelog

Some very minor changes (typo fixes for example) may not need
a Changes entry. In this case, you may explicitly disable this test by
adding the code word "No change entry needed" (on a single line) to
a commit message of the PR, or using the "no-change-entry-needed" label
on the github pull request.
------------------------------------------------------------------------
EOF
  # check that Changes has been modified
  git diff "$TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD" --name-only --exit-code \
    Changes > /dev/null && CheckNoChangesMessage || echo pass
}

CheckNoChangesMessage () {
  API_URL=https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/labels
  if [[ -n $(git log --grep='[Nn]o [Cc]hange.* needed' --max-count=1 \
    "$TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD") ]]
  then echo pass
  elif [[ -n $(curl "$API_URL" | grep 'no-change-entry-needed') ]]
  then echo pass
  else exit 1
  fi
}

CheckManual () {
      cat<<EOF
--------------------------------------------------------------------------
This test checks the global structure of the reference manual
(e.g. missing chapters).
--------------------------------------------------------------------------
EOF
  # we need some of the configuration data provided by configure
  ./configure
  $MAKE check-stdlib check-case-collision -C manual/tests

}

CheckTestsuiteModified () {
  cat<<EOF
------------------------------------------------------------------------
This test checks that the OCaml testsuite has been modified by the
pull request. Any new feature should come with tests, bugs should come
with regression tests, and generally any change in behavior that can
be exercised by a test should come with a test or modify and existing
test. See our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#test-you-must

Modifications that result in no change in observable behavior
(documentation contributions for example) can hardly be tested, in
which case it is acceptable for this test to fail.

Note: the heuristic used by this test is extremely fragile; passing it
does *not* imply that your change is appropriately tested.
------------------------------------------------------------------------
EOF
  # check that at least a file in testsuite/ has been modified
  git diff "$TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD" --name-only --exit-code \
    testsuite > /dev/null && exit 1 || echo pass
}

# Test to see if any part of the directory name has been marked prune
not_pruned () {
  DIR=$(dirname "$1")
  if [[ $DIR = '.' ]] ; then
    return 0
  else
    case ",$(git check-attr typo.prune "$DIR" | sed -e 's/.*: //')," in
      ,set,)
      return 1
      ;;
      *)

      not_pruned "$DIR"
      return $?
    esac
  fi
}

CheckTypoTree () {
  export OCAML_CT_HEAD=$1
  export OCAML_CT_LS_FILES="git diff-tree --no-commit-id --name-only -r $2 --"
  export OCAML_CT_CAT='git cat-file --textconv'
  export OCAML_CT_PREFIX="$1:"
  GIT_INDEX_FILE=tmp-index git read-tree --reset -i "$1"
  git diff-tree --diff-filter=d --no-commit-id --name-only -r "$2" \
    | (while IFS= read -r path
  do
    if not_pruned "$path" ; then
      echo "Checking $1: $path"
      if ! tools/check-typo "./$path" ; then
        touch check-typo-failed
      fi
    else
      echo "NOT checking $1: $path (typo.prune)"
    fi
    case "$path" in
      configure|configure.ac|VERSION|tools/ci/travis/travis-ci.sh)
        touch CHECK_CONFIGURE;;
    esac
  done)
  rm -f tmp-index
  if [[ -e CHECK_CONFIGURE ]] ; then
    rm -f CHECK_CONFIGURE
    echo "configure generation altered in $1"
    echo 'Verifying that configure.ac generates configure'
    git checkout "$1"
    mv configure configure.ref
    make configure
    if ! diff -q configure configure.ref >/dev/null ; then
      echo "configure.ac no longer generates configure, \
please run rm configure ; make configure and commit"
      exit 1
    fi
  fi
}

CHECK_ALL_COMMITS=0

CheckTypo () {
  export OCAML_CT_GIT_INDEX='tmp-index'
  export OCAML_CT_CA_FLAG='--cached'
  # Work around an apparent bug in Ubuntu 12.4.5
  # See https://bugs.launchpad.net/ubuntu/+source/gawk/+bug/1647879
  rm -f check-typo-failed
  if [[ -z $TRAVIS_COMMIT_RANGE ]]
  then CheckTypoTree "$TRAVIS_COMMIT" "$TRAVIS_COMMIT"
  else
    if [[ $TRAVIS_EVENT_TYPE = 'pull_request' ]]
    then TRAVIS_COMMIT_RANGE=$TRAVIS_MERGE_BASE..$TRAVIS_PULL_REQUEST_SHA
    fi
    if [[ $CHECK_ALL_COMMITS -eq 1 ]]
    then
      for commit in $(git rev-list "$TRAVIS_COMMIT_RANGE" --reverse)
      do
        CheckTypoTree "$commit" "$commit"
      done
    else
      if [[ -z $TRAVIS_PULL_REQUEST_SHA ]]
      then CheckTypoTree "$TRAVIS_COMMIT" "$TRAVIS_COMMIT"
      else CheckTypoTree "$TRAVIS_COMMIT" "$TRAVIS_COMMIT_RANGE"
      fi
    fi
  fi
  echo complete
  if [[ -e check-typo-failed ]]
  then exit 1
  fi
}


case $CI_KIND in
build) BuildAndTest;;
changes)
    case $TRAVIS_EVENT_TYPE in
        pull_request) CheckChangesModified;;
    esac;;
manual)
    CheckManual;;
tests)
    case $TRAVIS_EVENT_TYPE in
        pull_request) CheckTestsuiteModified;;
    esac;;
check-typo)
   set +x
   CheckTypo;;
check-depend)
    CheckSyncStdlibDocs
    CheckDepend;;
*) echo unknown CI kind
   exit 1
   ;;
esac
