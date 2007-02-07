#!/bin/sh
set -e
set -x
cd `dirname $0`/../..
./_build/ocamlbuild.native -quiet -build-dir _buildtest -no-links test/test9/testglob.native $@
./_buildtest/test/test9/testglob.native
