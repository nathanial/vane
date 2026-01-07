#!/bin/bash
set -e

cd "$(dirname "$0")"

./.lake/build/bin/vane "$@"
