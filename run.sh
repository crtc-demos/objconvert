#!/bin/sh
HERE=$(dirname "$0")
LD_LIBRARY_PATH="$HERE/strips" "$HERE/objconvert" $@
