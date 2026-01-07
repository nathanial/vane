#!/bin/bash
set -e

cd "$(dirname "$0")"

./build.sh vane
./.lake/build/bin/vane "$@"
