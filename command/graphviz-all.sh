#!/bin/bash

for filename in target/*.dot; do
    dot -Tsvg "$filename" -o "target/$(basename $filename).svg"
done
