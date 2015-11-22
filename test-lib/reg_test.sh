#!/bin/bash
make clean && make

# Count number of tests and failures
declare -i count
declare -i failures
count=0
failures=0

# Testing Addition
./acc < sourceFiles/addTest.acc > compiledCpp/addTest.cpp
g++ -o executables/addTest compiledCpp/addTest.cpp
./executables/addTest > output/addTestOutput.txt
count+=1
if diff "output/addTestOutput.txt" "expected/addTestExpected.txt" > /dev/null; then
	echo addTest passed
else
	echo addTest failed
	failures+=1
fi

# Testing Subtraction
./acc < sourceFiles/subTest.acc > compiledCpp/subTest.cpp
g++ -o executables/subTest compiledCpp/subTest.cpp
./executables/subTest > output/subTestOutput.txt
count+=1
if diff "output/subTestOutput.txt" "expected/subTestExpected.txt" > /dev/null; then
	echo subTest passed
else
	echo subTest failed
	failures+=1
fi

# Testing Multiplication
./acc < sourceFiles/multTest.acc > compiledCpp/multTest.cpp
g++ -o executables/multTest compiledCpp/multTest.cpp
./executables/multTest > output/multTestOutput.txt
count+=1
if diff "output/multTestOutput.txt" "expected/multTestExpected.txt" > /dev/null; then
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
