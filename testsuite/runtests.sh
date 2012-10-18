#!/bin/bash

concuerror="../concuerror"
results="./results"
prevDir=`pwd`

# Extract scenarios from the specified test
# Args:
#   $1   : Test
runTest() {
    local temp1=(`echo $1 | sed -e 's/suites\/\(\w\+\)\/src\/\(\w\+\)\(\.erl\)\?/\1 \2/'`)
    local suite="${temp1[0]}"
    local name="${temp1[1]}"
    if [ -d $1 ]; then
        # Our test is a multi module directory
        local dir=$1
        local mod="test"
        local files=(`ls $dir/*.erl`)
    else
        # Our test is a single module file
        local dir=${1%/*}
        local mod=$name
        local files=$1
    fi
    # Create a dir to save the results
    mkdir -p $results/$suite
    # Compile it
    erlc -W0 -o $dir $dir/$mod.erl
    # And extract scenarios from it
    local scenarios="`erl -noinput -pa . -pa $dir -s scenarios extract $mod -s init stop`"
    local line
    for line in ${scenarios//\\n/ }; do
        # Get function and preemption bound
        local temp2=(`echo $line | sed -e 's/{\w\+,\(\w\+\),\(\w\+\)}/\1 \2/'`)
        local fun="${temp2[0]}"
        local preb="${temp2[1]}"
        # And run the test
        runScenario "$suite" "$name" "$mod" "$fun" "$preb" "${files[@]}" &
    done
    wait
}

# Run the specified scenario and print the results
# Args:
#   $1   : Suite
#   $2   : Name
#   $3   : Module
#   $4   : Fun
#   $5   : Preb
#   Rest : Files
runScenario() {
    # Run concuerror
    $concuerror --target $3 $4 --files "${@:6}" \
        --output $results/$1/$2-$4-$5.txt \
        --preb $5 --quiet
    diff -I '<[0-9]\+\.[0-9]\+\.[0-9]\+>' \
        -I '#Ref<[0-9\.]\+>' \
        -uw suites/$1/results/$2-$4-$5.txt \
        $results/$1/$2-$4-$5.txt &> /dev/null
    if [ $? -eq 0 ]; then
        printf "%-10s %-20s %-30s  \033[01;32mok\033[00m\n" \
            "$1" "$2" "($4, $5)"
    else
        printf "%-10s %-20s %-30s  \033[01;31mfailed\033[00m\n" \
            "$1" "$2" "($4, $5)"
    fi
}


# Cleanup temp files
cd `dirname $0`
find . -name '*.beam' -exec rm {} \;
rm -rf $results/*

# Compile scenarios.erl
erlc scenarios.erl

# If we have arguments we should use this as
# tests, otherwise check them all
if [ $# -eq 0 ]; then
    tests=(`ls -d suites/*/src/*`)
else
    tests=("$@")
fi

# For every test do
for test in "${tests[@]}"; do
    runTest "$test" &
done

# Wait for all processes to finish
wait

# Cleanup temp files
find . -name '*.beam' -exec rm {} \;
cd $prevDir
