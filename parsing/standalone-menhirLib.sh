#!/bin/sh

usage() {
    cat <<EOF
Usage: $(basename "$0") <kind> <directory> <module1> [module2 ...]

Wraps OCaml modules into standalone sub-module definitions.

Arguments:
  kind       'ml' for implementation, 'mli' for interface
  directory  Path containing the .ml/.mli files
  module(s)  Base names to wrap (without extensions)

Example:
  $(basename "$0") ml ./src Foo Bar Baz
EOF
    exit 1
}

# wrap_module NAME
# Wraps a single OCaml module (`.ml`/`.mli`) into a sub-module definition.
# Handles interface-only, implementation-only, or both cases.
wrap_module() {
    local name="$1"
    local Name="${name^}" 2>/dev/null || Name=$(printf '%s' "$name" | sed 's/^\(.\)/\U\1/')

    if [ -r "${name}.mli" ] && [ -r "${name}.ml" ]; then
        echo "module ${Name} : sig"
        cat "${name}.mli"
        echo "end = struct"
        cat "${name}.ml"
        echo "end"
    elif [ -r "${name}.ml" ]; then
        echo "module ${Name} = struct"
        cat "${name}.ml"
        echo "end"
    elif [ -r "${name}.mli" ]; then
        echo "module rec ${Name} : sig"
        cat "${name}.mli"
        echo "end = ${Name}"
    else
        echo >&2 "Module $name not found."
        exit 1
    fi
}

# wrap_interface NAME
# Wraps a single OCaml interface (`.mli`) into a sub-module declaration.
wrap_interface() {
    local name="$1"
    local Name="${name^}" 2>/dev/null || Name=$(printf '%s' "$name" | sed 's/^\(.\)/\U\1/')

    if [ -r "${name}.mli" ]; then
        echo "module ${Name} : sig"
        cat "${name}.mli"
        echo "end"
    elif [ -r "${name}.ml" ]; then
        echo "module ${Name} : module type of struct"
        cat "${name}.ml"
        echo "end"
    else
        echo >&2 "Warning: Interface $name.mli not found."
        exit 1
    fi
}

# wrap_implementation NAME
# Wraps a single OCaml implementation (`.ml`) into a sub-module.
wrap_implementation() {
    local name="$1"
    local Name="${name^}" 2>/dev/null || Name=$(printf '%s' "$name" | sed 's/^\(.\)/\U\1/')

    if [ -r "${name}.ml" ]; then
        echo "module ${Name} = struct"
        cat "${name}.ml"
        echo "end"
    elif [ -r "${name}.mli" ]; then
        echo "module rec ${Name} : sig"
        cat "${name}.mli"
        echo "end = ${Name}"
    else
        echo >&2 "Implementation $name.ml not found."
        exit 1
    fi
}

# iter FUNC ARGS...
# Applies FUNC to each argument sequentially.
iter() {
    local f=$1
    shift 1
    for i in "$@"; do
        "$f" "$i"
    done
}

# sort_modules NAMES...
# Resolves OCaml module dependencies using `ocamldep` and `tsort`.
# Filters to requested modules and outputs them in correct compilation order.
sort_modules() {
    ocamldep $(printf '%s.mli ' "$@") $(printf '%s.ml ' "$@") -modules |
        sed -E 's/\.ml:?|\.mli:?//g' |
        while read lhs rhss; do
            if [[ -z "$rhss" ]]; then
                rhss="$lhs"
            fi
            printf "%s $lhs\n" $rhss;
        done |
        tsort |
        grep -F "$(printf '%s\n' "$@")"
}

# Validate arguments before processing
if [ $# -lt 3 ]; then usage; fi

KIND="$1"
DIR="$2"
cd "$DIR" || exit 1
shift 2

case "$KIND" in
    ml)  iter wrap_implementation $(sort_modules "$@");;
    mli) iter wrap_interface $(sort_modules "$@");;
    sort) sort_modules "$@";;
    *)   usage ;;
esac
