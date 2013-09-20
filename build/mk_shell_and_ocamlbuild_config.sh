#!/bin/sh -eu

# Generate config.sh and myocamlbuild_config.ml from config/Makefile.
#
# The approach is to create a new Makefile that "include" the config one and has
# a single rule that prints the value of each variable config/Makefile.

# Replace a variable name like "VAR" with [echo 'VAR="$(VAR)"; \\] for use in a
# Makefile rule.
print_value_shell='s/.*/	echo &="\\"$(&)\\""; \\/'

# Replace a variable name like "VAR" with [echo 'let var = "$(VAR)";;'; \\] for
# use in a Makefile rule.
# It's fairly annoying to have to rely on tr but there is no portable way to
# get the lowercase value of a variable in make.
print_value_ocaml='s/.*/	echo let $$(echo & | tr "[:upper:]" "[:lower:]") = "\\"$(&)\\";;"; \\/'

main() {
  # Command to run to output a line of code to set a variable to its value.
  local print_value="${1}"

  # List the variables in a Makefile except the ones that are commented out.
  local list_variables='/[^#].*=/ s/\([^ \t:=]\{1,\}\).*=.*$/\1/ p'

  # Variables we don't want to retrieve.
  local exclude='^#|\<CAML(C|OPT)_BIN\>|\<(MKLIB|SYSLIB|FLEXDIR|^SET_LD_PATH)\>'

  # Some variables in the config have a value that is shell script and has to be
  # evaluated to make sense for the outputs.
	local eval_command='s/\\"\(.*\)\\"/\\"$$(\1)\\"/'
  local eval_commands_1='/CAMLC_BIN_CMD_TO_EVAL/ '"${eval_command}"
  local eval_commands_2='/CAMLOPT_BIN_CMD_TO_EVAL/ '"${eval_command}"

  printf "include Makefile\n\n"
  printf "p:\n\t@true; \\\\\n"
  sed -n "${list_variables}" Makefile \
    | grep -v -E "${exclude}" \
    | sed "${print_value}" \
    | sed -e "${eval_commands_1}" -e "${eval_commands_2}"
  printf "\ttrue\n\n"
  printf ".PHONY: p"
}

# Unset both MAKEFLAGS and MAKELEVEL which we inherit from the recursive make
# calls in the build system. This prevents (gnu) make from printing the current
# directory [ make[4]: Leaving directory `/foo/ocaml/config' ].
unset MAKEFLAGS
unset MAKELEVEL

cd `dirname $0`/../config

main "${print_value_shell}" > mkconfig.Makefile
make -f mkconfig.Makefile > config.sh

main "${print_value_ocaml}" > mkconfig.Makefile
# The OCaml code expects bools instead of strings for true and false.
make -f mkconfig.Makefile \
	| sed -e 's/"true"/true/' -e 's/"false"/false/' \
  > ../myocamlbuild_config.ml
# ./configure outputs in config/Makefile some OCaml functions which we need;
# they are commented out and preceded with '#ml '; since they depend on other
# values, output them at the very end of the output.
sed -n 's/^#ml // p' Makefile >> ../myocamlbuild_config.ml

rm -f mkconfig.Makefile
