#!/bin/bash
make clean && make


declare -i count
declare -i failures
count=0
failures=0

./acc < addTest.acc > addTest.cpp
g++ -0 addTest addTest.cpp
./addTest > addTestOutput.txt

count+=1
if diff "addTestOutput.txt" "addTestExpected.txt" > /dev/null; then
	echo addTest passed
else
	echo addTest failed
	failurs+=1
fi

echo ====================================================
echo Results
echo ====================================================
echo Test Suite finished
echo Failed Tests: $failures
echo Total Tests: $count
echo
echo ====================================================
echo Cleaning up
echo ====================================================
make clean
echo
