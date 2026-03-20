#!/bin/sh
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            David Allsopp, University of Cambridge & Tarides            *
#*                                                                        *
#*   Copyright 2025 David Allsopp Ltd.                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

set -eu

# POSIX.1-2024 (Issue 8) lifts this from being a bashism. The sub-shell dance is
# necessary because set is a builtin and is permitted to abort the script
# unconditionally on error.
if (set -o pipefail 2> /dev/null); then
  set -o pipefail
fi

# This script is responsible for building and cloning OCaml installations. It is
# invoked by both the build and install sections of an opam package.
#   $1 = make command (the `make` variable in opam). This should be the path to
#        a binary only and is invoked without word-splitting (i.e. any
#        additional arguments should be passed in $2 and the command is invoked
#        "$1").
#   $2 = additional arguments passed to "$1". This variable will be used
#        unquoted - arguments with spaces cannot be passed. From the build
#        section, this allows the -j argument to be specified. For the install
#        section, this argument must be "install".
#   $3 = opam build-id variable of this package.
#   $4 = name of the opam package to be used when generating .install and
#        .config files.
# The remaining arguments depend on the value of $2. When it is "install":
#   $5 = installation prefix, which may be a native Windows path, rather than a
#        Cygwin path.
# When $2 is not "install" (the build opam section):
#   $5 = "enabled" if cloning the compiler from an existing switch is permitted
#        and "disabled" to force the compiler to be built from sources.
#   $6, and any further arguments are additional options to pass to `configure`
#        if the compiler is built from sources.

make="$1"
make_args="$2"
build_id="$3"
package_name="$4"

if [ x"$make_args" = 'xinstall' ]; then
  prefix="$5"

  echo "📦 Installing the compiler to $prefix"
  if [ -e 'config.status' ]; then
    echo "📜 Using make install"
    "$make" install
  else
    origin="$(tail -n 1 build-id)"
    origin_prefix="$(opam var --safe --switch="$origin" prefix | tr -d '\r')"
    echo "🪄 Duplicating $origin_prefix"
    ( cd "$origin_prefix" && sh ./share/ocaml/clone "$prefix" )
  fi

  exit 0
fi

# Build the package

cloning="$5"
shift 5
# "$@" now expands to the correctly-quoted arguments to pass to configure

origin=''
clone_mechanism=''
if [ x"$cloning" = 'xenabled' ]; then
  echo "🕵️ Searching for a switch containing build-id $build_id"

  if [ -e "$OPAM_SWITCH_PREFIX/share/ocaml/build-id" ]; then
    switch="$(tail -n 1 "$OPAM_SWITCH_PREFIX/share/ocaml/build-id")"
    if [ -n "$switch" ]; then
      switch_share_dir="$(opam var --safe --switch="$switch" share \
                            | tr -d '\r')"
      switch_build_id="$switch_share_dir/ocaml/build-id"
      if [ -e "$switch_build_id" ]; then
        if [ x"$build_id" = x"$(head -n 1 "$switch_build_id")" ]; then
          echo "🔁 Prefer to re-clone from $switch"
          echo "$switch" > opam-switches
          origin="$switch"
          if ln "$switch_build_id" __cp_test 2>/dev/null; then
            rm __cp_test
            clone_mechanism='hard-linking'
          fi
        fi
      fi
    fi
  fi

  echo "🐫 Requesting list of switches from opam"
  opam switch list --safe --short | tr -d '\r' | grep -Fxv "$OPAMSWITCH" \
    >> opam-switches 2> /dev/null || true

  while IFS= read -r switch; do
    switch_share_dir="$(opam var --safe --switch="$switch" share | tr -d '\r')"
    switch_build_id="$switch_share_dir/ocaml/build-id"
    if [ -e "$switch_build_id" ]; then
      if [ x"$build_id" = x"$(head -n 1 "$switch_build_id")" ]; then
        # There are three ways of cloning a switch:
        #   - Copy-on-Write (cp --reflink=always)
        #   - Hard linking
        #   - Copy
        # Copy-on-Write is the ideal - virtually no space overhead, but with
        # defence against accidental subsequent alterations. Hard linking is
        # preferred over copying for the space-saving, and because the
        # compiler should not being subsequently altered.
        if cp --reflink=always "$switch_build_id" __cp_test 2>/dev/null; then
          rm __cp_test
          echo "📝  - can reflink from: $switch"
          origin="$switch"
          clone_mechanism='copy-on-write'
          break
        elif ln "$switch_build_id" __cp_test 2>/dev/null; then
          rm __cp_test
          if [ -z "$clone_mechanism" ]; then
            echo "🔗  - can hard link from: $switch"
            origin="$switch"
            clone_mechanism='hard-linking'
          fi
        elif [ -z "$origin" ]; then
          echo "📄  - can copy from: $switch"
          origin="$switch"
        fi
      elif [ -z "$origin" ]; then
        echo "⛔  - different compiler: $switch"
      fi
    fi
  done < opam-switches
fi

{ echo "$build_id"; echo "$origin" ; } > build-id

if [ -n "$origin" ]; then

  echo "🧬 Will clone the compiler from $origin"
  test -n "$clone_mechanism" || clone_mechanism='copying'

  cloned='true'
  clone_source="$(sed -e '1d;s/\\/\\\\/g;s/%/%%/g;s/"/\\"/g' build-id)"
  case "$origin" in
    */*|*\\*) clone_source="local switch $clone_source";;
    *) clone_source="global switch $clone_source";;
  esac

  cat > "$package_name.install" <<'EOF'
share_root: [
  "build-id" {"ocaml/build-id"}
]
EOF

else

  echo "🏗️ Will build the compiler from sources"

  cloned='false'
  clone_source=''

  ./configure --cache-file=config.cache "$@"
  "$make" $make_args
  "$make" OPAM_PACKAGE_NAME=ocaml-compiler INSTALL_MODE=clone install

  cat > "$package_name.install" <<'EOF'
share_root: [
  "build-id" {"ocaml/build-id"}
  "ocaml-compiler-clone.sh" {"ocaml/clone"}
  "config.cache" {"ocaml/config.cache"}
  "config.status" {"ocaml/config.status"}
]
EOF
fi

# Create the .config file
cat > "$package_name.config" <<EOF
opam-version: "2.0"
variables {
  cloned: $cloned
  clone-source: "$clone_source"
  clone-mechanism: "$clone_mechanism"
}
EOF
