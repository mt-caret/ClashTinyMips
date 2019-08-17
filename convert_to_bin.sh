#!/usr/bin/env bash
set -euo pipefail
cat "$1" |
while read -r line; do
  echo "ibase=16;obase=2;${line^^}" |
  bc |
  xargs printf "%32s\n" |
  tr ' ' '0'
done
