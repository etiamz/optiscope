#!/bin/bash

set -e

options="-Wall -Wextra -std=gnu99 -O3 -march=native -DNDEBUG -Wno-unused-function"

mimalloc="/usr/local/lib/libmimalloc.so"

if [ -z $CC ]; then
    CC=gcc
fi

if [ "$GITHUB_ACTIONS" = "true" ]; then
    warmup=0
    runs=1
else
    warmup=1
    runs=5
fi

for filename in benchmarks/*.c; do
    base_filename="$(basename $filename .c)"
    $CC "$filename" -o "$base_filename" $options
    LD_PRELOAD=$mimalloc hyperfine --warmup=$warmup --runs=$runs ./$base_filename
    rm "$base_filename"
done
