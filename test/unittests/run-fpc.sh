#!/bin/sh
# Build and run the unit tests with Free Pascal / Lazarus, headless.
#
#   ./run-fpc.sh                                  whole suite, plain text output
#   ./run-fpc.sh --format=xml --file=results.xml  custom FPCUnit options
#
# Set LAZBUILD to your lazbuild binary if it is not on PATH.
set -e

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

"${LAZBUILD:-lazbuild}" -B --build-mode=Console "$here/Unittests.lpi"

exe="$here/UnittestsConsole"
[ -f "$exe.exe" ] && exe="$exe.exe"

if [ "$#" -eq 0 ]; then
  exec "$exe" --all --format=plain
fi
exec "$exe" "$@"
