#!/bin/sh

set -e

if ! command -v jq >/dev/null 2>&1; then
  echo "jq could not be found" > "${ocamltest_response:?}"
  exit "${TEST_SKIP}"
fi


gnuc=$("$@" -E -P - <<'EOF'
__GNUC__
EOF
)

if [ "$gnuc" -ge 9 ]; then
  exit "${TEST_PASS}"
else
  echo "GCC >= 9 is required" > "${ocamltest_response:?}"
  exit "${TEST_SKIP}"
fi
