#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Usage: gen_empty_test.sh <test_name>"
    return
fi

echo "@SYSTEM_TITLE" > tests/run_tests/basic/$1.erb
echo "[]" > tests/run_tests/basic/$1.ron
