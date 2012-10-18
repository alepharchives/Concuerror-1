#!/usr/bin/env python

import sys
import os
import glob
import subprocess
from multiprocessing import Process, Lock, BoundedSemaphore


#---------------------------------------------------------------------
# Extract scenarios from the specified test

def runTest(sema, test):
    global dirname
    global results
    # test has the format of '.*/suites/<suite_name>/src/<test_name>(.erl)?'
    # Split the test in suite and name components using pattern matching
    t = test.split("/")
    suite = t[-3]
    name = os.path.splitext(t[-1])[0]
    if os.path.isdir(test):
        # Our test is a multi module directory
        dirn = test     # directory
        modn = "test"   # module name
        files = glob.glob(dirn + "/*.erl")
    else:
        dirn = os.path.dirname(test)
        modn = name
        files = [test]
    # Create a dir to save the results
    try:
        os.mkdir(results + "/" + suite)
    except OSError:
        pass
    # Compile it
    os.system("erlc -W0 -o %s %s/%s.erl" % (dirn, dirn, modn))
    # And extract scenarios from it
    pout = subprocess.Popen(
            ["erl -noinput -pa %s -pa %s -s scenarios extract %s -s init stop"
            % (dirname, dirn, modn)], stdout=subprocess.PIPE, shell=True)
    procS = []
    for scenario in pout.stdout:
        # scenario has the format of {<mod_name>,<func_name>,<preb>}\n
        scen = scenario.strip("{}\n").split(",")
        # And run the test
        p = Process(target=runScenario,
                args=(sema, suite, name, modn, scen[1], scen[2], files))
        p.start()
        procS.append(p)
    pout.stdout.close()
    # Wait
    for p in procS:
        p.join()

#---------------------------------------------------------------------
# Run the specified scenario and print the results

def runScenario(sema, suite, name, modn, funn, preb, files):
    global concuerror
    global results
    global dirname
    IGNORE = open('/dev/null', 'w')
    sema.acquire()
    # Run concuerror
    os.system("%s --target %s %s --files %s --output %s/%s/%s-%s-%s.txt --preb %s --quiet"
            % (concuerror, modn, funn, ' '.join(files), results, suite, name,
               funn, preb, preb))
    try:
        subprocess.check_call(("diff -I '<[0-9]\+\.[0-9]\+\.[0-9]\+>' " +
                               "-I '#Ref<[0-9\.]\+>' " +
                               "-uw %s/suites/%s/results/%s-%s-%s.txt " +
                               "%s/%s/%s-%s-%s.txt") % \
                              (dirname, suite, name, funn, preb, results, suite, name, funn, preb),
                              shell=True, stdout=IGNORE, stderr=IGNORE)
        print "%-10s %-20s %-40s  \033[01;32mok\033[00m" % \
                (suite, name, "("+funn+", "+preb+")")
    except subprocess.CalledProcessError:
        print "%-10s %-20s %-40s  \033[01;31mfailed\033[00m" % \
                (suite, name, "("+funn+", "+preb+")")
    sema.release()
    IGNORE.close()

#---------------------------------------------------------------------
# Main program

# Get the directory of Concuerror's testsuite
dirname = os.path.normpath(os.path.dirname(sys.argv[0]))
concuerror = dirname + "/../concuerror"
results = dirname + "/results"

# Cleanup temp files
os.system("find %s -name '*.beam' -exec rm {} \;" % dirname)
os.system("rm -rf %s/*" % results)

# Compile scenarios.erl
os.system("erlc %s/scenarios.erl" % dirname)

# If we have arguments we should use them as tests,
# otherwise check them all
if len(sys.argv) > 1:
    tests = sys.argv[1:]
    tests = [item.rstrip('/') for item in tests] 
else:
    tests = glob.glob(dirname + "/suites/*/src/*")

# For every test do
procT = []
sema = BoundedSemaphore(2)
for test in tests:
    p = Process(target=runTest, args=(sema, test))
    p.start()
    procT.append(p)
# Wait
for p in procT:
    p.join()

# Cleanup temp files
os.system("find %s -name '*.beam' -exec rm {} \;" % dirname)
