#!/bin/bash

set -e

if [ -z $CC ]; then
    CC=gcc
fi

options="-Wall -Wextra -pedantic -std=c99 -g -fsanitize=address -fsanitize=undefined"
gcc_suggest_options="-Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=noreturn -Wsuggest-attribute=cold"
macos_suppress_options="-Wno-deprecated-declarations -Wno-c11-extensions"

if [ "$CC" = "gcc" ] && [[ "$OSTYPE" == "linux-gnu"* ]]; then
    options="$options $gcc_suggest_options"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    options="$options $macos_suppress_options"
fi

# Detect potential fallback linked list bugs.
options="$options -DOPTISCOPE_MULTIFOCUS_COUNT=1000"

$CC tests.c optiscope.c -o tests $options
./tests
rm tests
