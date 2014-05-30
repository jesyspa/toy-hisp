#!/bin/bash

readonly inputdir="$1"
readonly outputdir="$2"
readonly name="$3"
readonly testdir="$outputdir/$name"

set errexit

mkdir "$testdir"
mkdir "$testdir/output"

cp "$inputdir/${name}.expected" "$testdir"
cp "$inputdir/${name}.hisp" "$testdir"

./main < "$testdir/${name}.hisp"
mv out.hic "$testdir/${name}.hic"
./hisp --debug-output-dir "$testdir/output" --dump-graph "$testdir/${name}.hic" > "$testdir/${name}.actual"

set noerrexit

diff "$testdir/${name}.actual" "$testdir/${name}.expected" > "$testdir/${name}.diff"
if [[ $? -ne 0 ]]; then
    echo "fail: $name" >&2
    cat "$testdir/${name}.diff" >&2
fi
