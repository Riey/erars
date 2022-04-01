#!/usr/bin/env bash

echo "mod test_util"

echo "mod exprs {"

for f in ./tests/parse_tests/exprs/*.erb; do
    echo $f
done

echo "}"
