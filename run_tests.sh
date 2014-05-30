#!/bin/bash

readonly progname=$(basename $0)
readonly inputdir="$1"

print_usage() {
    echo "usage: $progname inputdir" >&2
    exit 1
}

if [[ -z $inputdir ]]; then
    print_usage
fi

readonly now=$(date +%Y%m%d-%H-%M-%S)
readonly testdir="test-run-$now"

mkdir "$testdir"

for x in "$inputdir"/*.hisp; do
    name=$(basename $x)
    ./run_one_test.sh "$inputdir" "$testdir" "${name%.hisp}"
done


