#!/bin/bash

set -e

optiscope_options="-DNDEBUG -DOPTISCOPE_ENABLE_HUGE_PAGES"
compiler_options="-Wall -Wextra -std=gnu99 -O3 -march=native -Wno-unused-function"
all_options="$optiscope_options $compiler_options"

if [ -z $CC ]; then
    CC=gcc
fi

for filename in benchmarks/*.c; do
    base_filename="$(basename $filename .c)"
    $CC "$filename" optiscope.c -o "$base_filename" $all_options
    if [ "$GITHUB_ACTIONS" = "true" ]; then
        echo "Running './$base_filename'..."
        ./$base_filename
        echo "Done."
    else
        hyperfine --warmup=1 --runs=5 ./$base_filename
    fi
    rm "$base_filename"
done
