#! /bin/sh
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 David Allsopp, OCaml Labs, Cambridge.                  *
#*                                                                        *
#*   Copyright 2019 MetaStack Solutions Ltd.                              *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# This script should have the same shebang as configure
if test -e '.git' ; then :
  extra_args=$(git config ocaml.configure 2>/dev/null)
  if test -n "$extra_args" ; then :
    if test -z "$ac_read_git_config" ; then :
      echo "Detected Git configuration option ocaml.configure set to \
\"$extra_args\""
      # Too much effort to get the echo to show appropriate quoting - the
      # invocation itself intentionally quotes $0 and passes $@ exactly as given
      # but allows a single expansion of ocaml.configure
      echo "Re-running $0 $extra_args $@"
      ac_read_git_config=true exec "$0" $extra_args "$@"
    fi
  fi
fi
