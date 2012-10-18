#!/usr/bin/env python

import os
import re
import sys
import glob
import difflib
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
    sema.acquire()
    # Run concuerror
    os.system("%s --target %s %s --files %s --output %s/%s/%s-%s-%s.txt --preb %s --quiet"
            % (concuerror, modn, funn, ' '.join(files), results, suite, name,
               funn, preb, preb))
    a = "%s/suites/%s/results/%s-%s-%s.txt" % (dirname, suite, name, funn, preb)
    b = "%s/%s/%s-%s-%s.txt" % (results, suite, name, funn, preb)
    if isDiff(a, b):
        print "%-10s %-20s %-40s  \033[01;31mfailed\033[00m" % \
                (suite, name, "("+funn+", "+preb+")")
    else:
        print "%-10s %-20s %-40s  \033[01;32mok\033[00m" % \
                (suite, name, "("+funn+", "+preb+")")
    sema.release()

def isDiff(a, b):
    try:
        diff = difflib.SequenceMatcher(ignoreLines,
                open(a).readlines(), open(b).readlines())
        if diff.ratio() == 1.0:
            return False
        else:
            return True
    except:
        return True

def ignoreLines(line):
    global match_pids
    global match_refs
    if not re.search(match_pids, line):
        print line, "Here 1"
        return True
    elif not re.search(match_refs, line):
        print line, "Here 2"
        return True
    else:
        print line, "Here 3"
        return False

#---------------------------------------------------------------------
# Main program

# Compile some regular expressions
match_pids = re.compile("<\d+\.\d+\.\d+>")
match_refs = re.compile("#Ref<.*>")

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
sema = BoundedSemaphore(2000)
for test in tests:
    p = Process(target=runTest, args=(sema, test))
    p.start()
    procT.append(p)
# Wait
for p in procT:
    p.join()

# Cleanup temp files
os.system("find %s -name '*.beam' -exec rm {} \;" % dirname)
