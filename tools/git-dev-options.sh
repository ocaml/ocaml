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
  if test -z "$ac_read_git_config" ; then :
    extra_args=$(git config ocaml.configure 2>/dev/null)
    extended_cache=$(git config ocaml.configure-cache 2>/dev/null)
    cache_file=

    # If ocaml.configure-cache is set, parse the command-line for the --host
    # option, in order to determine the name of the cache file.
    if test -n "$extended_cache" ; then :
      echo "Detected Git configuration option ocaml.configure-cache set to \
\"$extended_cache\""
      dashdash=
      prev=
      host=default
      # The logic here is pretty borrowed from autoconf's
      for option in $extra_args "$@"
      do
        if test -n "$prev" ; then :
          host=$option
          continue
        fi

        case $dashdash$option in
          --)
            dashdash=yes ;;
          -host | --host | --hos | --ho)
            prev=host ;;
          -host=* | --host=* | --hos=* | --ho=*)
            case $option in
              *=?*) host=$(expr "X$option" : '[^=]*=\(.*\)') ;;
              *=) host= ;;
            esac ;;
        esac
      done
      cache_file="`dirname "$0"`/$extended_cache/ocaml-$host.cache"
    fi

    # If either option has a value, re-invoke configure
    if test -n "$extra_args$cache_file" ; then :
      echo "Detected Git configuration option ocaml.configure set to \
\"$extra_args\""
      # Too much effort to get the echo to show appropriate quoting - the
      # invocation itself intentionally quotes $0 and passes $@ exactly as given
      # but allows a single expansion of ocaml.configure
      if test -n "$cache_file" ; then :
        echo "Re-running $0 $extra_args --cache-file \"$cache_file\" $@"
        ac_read_git_config=true exec "$0" $extra_args \
                                          --cache-file "$cache_file" "$@"
      else
        echo "Re-running $0 $extra_args $@"
        ac_read_git_config=true exec "$0" $extra_args "$@"
      fi
    fi
  fi
fi
