#!/bin/bash

set -e

cd benchmarks-haskell

for file in *.hs; do
    basename_file=$(basename "$file" .hs)
    ghc -i "$file" -o "$basename_file" -O0
done
