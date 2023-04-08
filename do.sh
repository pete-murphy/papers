#!/bin/bash

# Use like
# ./do.sh declarative-uis
# or
# ./do.sh declarative-uis UI

# Run the first command and store its output in a variable
output=$(cabal build --dry-run | awk -F 'lib:' 'NF>1{print $2}' | awk -F ')' '{print $1}')

# Check if the input line matches any of the output lines
if ! echo "$output" | grep -qwF "$1"; then
  echo "Expected one of"
  echo $output
  echo ""
  echo "but received"
  echo $1
  exit 1
fi

module=${2:-Main.hs}

ghcid --command "cabal repl" --project "$1" --allow-eval "$module"

