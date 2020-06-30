#!/bin/bash

function copy {
    LIB=$1

    echo "Found libray '$LIB'."
    echo ""

    for OUT in $PWD/.stack-work/install/*; do
        echo "Copying found library at '$LIB'"
        echo "                      to '$OUT/bin'"

        cp "$LIB" "$OUT/bin"

        echo ""
    done
}

IN_PATH="$PWD/src/runtime/target/release"

for EXT in "dll" "so" "dylib"; do
    if [ -f "$IN_PATH/runtime.$EXT" ]; then
        copy "$IN_PATH/runtime.$EXT"
    elif [ -f "$IN_PATH/libruntime.$EXT" ]; then
        copy "$IN_PATH/libruntime.$EXT"
    fi
done