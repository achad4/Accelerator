#!/bin/bash
make clean && make

# Count number of tests and failures
declare -i count
declare -i failures
count=0
failures=0

# Testing Addition
./acc < addTest.acc > addTest.cpp
g++ -o addTest addTest.cpp
./addTest > addTestOutput.txt
count+=1
if diff "addTestOutput.txt" "addTestExpected.txt" > /dev/null; then
	echo addTest passed
else
	echo addTest failed
	failures+=1
fi

# Testing Subtraction
./acc < subTest.acc > subTest.cpp
g++ -o subTest subTest.cpp
./subTest > subTestOutput.txt
count+=1
if diff "subTestOutput.txt" "subTestExpected.txt" > /dev/null; then
	echo subTest passed
else
	echo subTest failed
	failures+=1
fi

# Testing Multiplication
./acc < multTest.acc > multTest.cpp
g++ -o multTest multTest.cpp
./multTest > multTestOutput.txt
count+=1
if diff "multTestOutput.txt" "multTestExpected.txt" > /dev/null; then
	echo multTest passed
else
	echo multTest failed
	failures+=1
fi

echo ====================================================
echo Results
echo ====================================================
echo Test Suite finished
echo Failed Tests: $failures
echo Total Tests: $count
make clean
