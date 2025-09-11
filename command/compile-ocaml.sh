#!/bin/bash

set -e

cd benchmarks-ocaml

for file in *.ml; do
    if [[ -f "$file" ]]; then
        basename_file=$(basename "$file" .ml)
        ocamlopt -I . "$file" -o "$basename_file"
    fi
done
