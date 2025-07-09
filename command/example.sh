#!/bin/bash

set -e

options="-Wall -Wextra -pedantic -std=c99 -g -fsanitize=address -fsanitize=undefined"
macos_suppress_options="-Wno-deprecated-declarations -Wno-c11-extensions"

if [[ "$OSTYPE" == "darwin"* ]]; then
    options="$options $macos_suppress_options"
fi

gcc "examples/$1.c" optiscope.c -o "$1" $options
./"$1"
rm "$1"
