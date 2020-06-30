#!/bin/bash

echo "ROOT=$PWD"

for EXT in "dll" "so" "dylib"; do
    LIB="$PWD/src/runtime/target/release/runtime.$EXT"

    if [ -f "$LIB" ]; then
        echo "Found libray '$LIB'."

        echo ""

        for OUT in $PWD/.stack-work/install/*; do
            echo "Copying found library at '$LIB'"
            echo "                      to '$OUT/bin'"

            cp "$LIB" "$OUT/bin"

            echo ""
        done
    fi
done