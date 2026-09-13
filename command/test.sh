#!/bin/bash

set -e

if [ -z $CC ]; then
    CC=gcc
fi

options="-I. -Wall -Wextra -pedantic -std=c99 -g -fsanitize=address,undefined"
gcc_suggest_options="-Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=noreturn -Wsuggest-attribute=cold"
macos_suppress_options="-Wno-deprecated-declarations -Wno-c11-extensions"

if [ "$CC" = "gcc" ] && [[ "$OSTYPE" == "linux-gnu"* ]]; then
    options="$options $gcc_suggest_options"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    options="$options $macos_suppress_options"
fi

# Run all the combinations of optimizations, exhaustively.
first=1
for a in "" "-DOPTISCOPE_DISABLE_DELIMITER_COMPRESSION"; do
for b in "" "-DOPTISCOPE_DISABLE_DELIMITER_SCHEDULING"; do
for c in "" "-DOPTISCOPE_DISABLE_DELIMITER_EXTRUSION"; do
for d in "" "-DOPTISCOPE_DISABLE_CLOSEDNESS_ANNOTATIONS"; do
for e in "" "-DOPTISCOPE_DISABLE_SEGMENTATION"; do
for f in "" "-DOPTISCOPE_DISABLE_ZERO_DELIMITER_ABSORPTION"; do
    if [ -n "$d" ] && [ -z "$e" ]; then continue; fi
    if [ -n "$a" ] && [ -z "$b" ]; then continue; fi
    if [ "$first" -eq 0 ]; then echo ""; fi
    first=0
    extra=$(echo $a $b $c $d $e $f)
    if [ -z "$extra" ]; then echo "<empty>"; else echo "$extra"; fi
    $CC tests.c optiscope.c -o tests $options $extra
    ./tests
    rm tests
done
done
done
done
done
done
