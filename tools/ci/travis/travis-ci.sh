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
echo TRAVIS_COMMIT_RANGE=$TRAVIS_COMMIT_RANGE
echo TRAVIS_COMMIT=$TRAVIS_COMMIT
if [[ $TRAVIS_EVENT_TYPE = "pull_request" ]] ; then
  FETCH_HEAD=$(git rev-parse FETCH_HEAD)
  echo FETCH_HEAD=$FETCH_HEAD
else
  FETCH_HEAD=$TRAVIS_COMMIT
fi

if [[ $TRAVIS_COMMIT != $(git rev-parse FETCH_HEAD) ]] ; then
  echo "WARNING! Travis TRAVIS_COMMIT and FETCH_HEAD do not agree!"
  if git cat-file -e $TRAVIS_COMMIT 2> /dev/null ; then
    echo "TRAVIS_COMMIT exists, so going with it"
  else
    echo "TRAVIS_COMMIT does not exist; setting to FETCH_HEAD"
    TRAVIS_COMMIT=$FETCH_HEAD
  fi
fi

set -x

PREFIX=~/local

MAKE=make SHELL=dash

TRAVIS_CUR_HEAD=${TRAVIS_COMMIT_RANGE%%...*}
TRAVIS_PR_HEAD=${TRAVIS_COMMIT_RANGE##*...}
case $TRAVIS_EVENT_TYPE in
   # If this is not a pull request then TRAVIS_COMMIT_RANGE may be empty.
   pull_request)
     DEEPEN=50
     while ! git merge-base $TRAVIS_CUR_HEAD $TRAVIS_PR_HEAD > /dev/null 2>&1
     do
       echo Deepening $TRAVIS_BRANCH by $DEEPEN commits
       git fetch origin --deepen=$DEEPEN $TRAVIS_BRANCH
       ((DEEPEN*=2))
     done
     TRAVIS_MERGE_BASE=$(git merge-base $TRAVIS_CUR_HEAD $TRAVIS_PR_HEAD);;
esac

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

  configure_flags="\
    --prefix=$PREFIX \
    --enable-flambda-invariants \
    $CONFIG_ARG"
  case $XARCH in
  x64)
    ./configure $configure_flags
    ;;
  i386)
    ./configure --build=x86_64-pc-linux-gnu --host=i386-pc-linux-gnu \
      AS="as" ASPP="gcc -c" \
      $configure_flags
    ;;
  *)
    echo unknown arch
    exit 1
    ;;
  esac

  export PATH=$PREFIX/bin:$PATH
  $MAKE world.opt
  $MAKE ocamlnat
  cd testsuite
  echo Running the testsuite with the normal runtime
  $MAKE all
  echo Running the testsuite with the debug runtime
  $MAKE USE_RUNTIME="d" OCAMLTESTDIR=$(pwd)/_ocamltestd TESTLOG=_logd all
  cd ..
  $MAKE install
  echo Check the code examples in the manual
  $MAKE manual-pregen
  # check_all_arches checks tries to compile all backends in place,
  # we would need to redo (small parts of) world.opt afterwards to
  # use the compiler again
  $MAKE check_all_arches
  # check that the 'clean' target also works
  $MAKE clean
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
  git diff $TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD --name-only --exit-code Changes \
    > /dev/null && CheckNoChangesMessage || echo pass
}

CheckNoChangesMessage () {
  API_URL=https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/labels
  if test -n "$(git log --grep="[Nn]o [Cc]hange.* needed" --max-count=1 \
    ${TRAVIS_MERGE_BASE}..${TRAVIS_PR_HEAD})"
  then echo pass
  elif test -n "$(curl $API_URL | grep 'no-change-entry-needed')"
  then echo pass
  else exit 1
  fi
}

CheckManual () {
      cat<<EOF
--------------------------------------------------------------------------
This test checks that all standard library modules are referenced by the
standard library chapter of the manual.
--------------------------------------------------------------------------
EOF
  # we need some of the configuration data provided by configure
  ./configure
  $MAKE check-stdlib -C manual/tests
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
  git diff $TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD --name-only --exit-code \
    testsuite > /dev/null && exit 1 || echo pass
}

# Test to see if any part of the directory name has been marked prune
not_pruned () {
  DIR=$(dirname "$1")
  if [ "$DIR" = "." ] ; then
    return 0
  else
    case ",$(git check-attr typo.prune "$DIR" | sed -e 's/.*: //')," in
      ,set,)
      return 1
      ;;
      *)

      not_pruned $DIR
      return $?
    esac
  fi
}

CheckTypoTree () {
  export OCAML_CT_HEAD=$1
  export OCAML_CT_LS_FILES="git diff-tree --no-commit-id --name-only -r $2 --"
  export OCAML_CT_CAT="git cat-file --textconv"
  export OCAML_CT_PREFIX="$1:"
  GIT_INDEX_FILE=tmp-index git read-tree --reset -i $1
  git diff-tree --diff-filter=d --no-commit-id --name-only -r $2 \
    | (while IFS= read -r path
  do
    if not_pruned $path ; then
      echo "Checking $1: $path"
      if ! tools/check-typo ./$path ; then
        touch check-typo-failed
      fi
    else
      echo "NOT checking $1: $path (typo.prune)"
    fi
  done)
  rm -f tmp-index
}

CHECK_ALL_COMMITS=0

CheckTypo () {
  export OCAML_CT_GIT_INDEX="tmp-index"
  export OCAML_CT_CA_FLAG="--cached"
  # Work around an apparent bug in Ubuntu 12.4.5
  # See https://bugs.launchpad.net/ubuntu/+source/gawk/+bug/1647879
  export OCAML_CT_AWK="awk --re-interval"
  rm -f check-typo-failed
  if test -z "$TRAVIS_COMMIT_RANGE"
  then CheckTypoTree $TRAVIS_COMMIT $TRAVIS_COMMIT
  else
    if [ "$TRAVIS_EVENT_TYPE" = "pull_request" ]
    then TRAVIS_COMMIT_RANGE=$TRAVIS_MERGE_BASE..$TRAVIS_PULL_REQUEST_SHA
    fi
    if [ $CHECK_ALL_COMMITS -eq 1 ]
    then
      for commit in $(git rev-list $TRAVIS_COMMIT_RANGE --reverse)
      do
        CheckTypoTree $commit $commit
      done
    else
      if [ -z "$TRAVIS_PULL_REQUEST_SHA" ]
      then CheckTypoTree $TRAVIS_COMMIT $TRAVIS_COMMIT
      else CheckTypoTree $TRAVIS_COMMIT $TRAVIS_COMMIT_RANGE
      fi
    fi
  fi
  echo complete
  if [ -e check-typo-failed ]
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
*) echo unknown CI kind
   exit 1
   ;;
esac
