#!/bin/sh

# wrap_module NAME
# Wraps a single OCaml module (`.ml`/`.mli`) into a sub-module definition.
# Capitalizes the name and handles interface-only, implementation-only, or both cases.
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
        echo >&@ "Module $name not found."
        exit 1
    fi
}

# wrap_modules NAMES...
# Iterates over provided module names and wraps each one sequentially.
# Outputs concatenated module definitions to stdout.
wrap_modules() {
    for i in "$@"; do
        wrap_module "$i"
    done
}

# sort_modules NAMES...
# Resolves OCaml module dependencies using `ocamldep` and `tsort`.
# Filters to requested modules and outputs them in correct compilation order.
sort_modules() {
    ocamldep $(printf '%s.mli ' "$@") $(printf '%s.ml ' "$@") -modules |
        sed -E 's/\.ml:?|\.mli:?//g' |
        while read lhs rhss; do
            if [[ -n "$rhss" ]]; then
                printf "%s $lhs\n" $rhss;
            fi;
        done |
        tsort |
        grep -F "$(printf '%s\n' "$@")"
}

cd "$1"
shift 1
wrap_modules $(sort_modules "$@")
