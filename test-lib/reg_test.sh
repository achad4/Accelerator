#!/bin/bash
echo
echo ====================================================
echo ===============ACCELERATOR TEST SUITE===============
echo ====================================================
echo
echo
echo ====================================================
echo Cleaning and making...
echo ====================================================
echo
make clean && make
echo
echo ====================================================
echo Running Tests...
echo ====================================================
echo

./acc < addTest.acc > addTest.cpp

declare -i count
declare -i failures
count=0
failures=0

count+=1
echo Running test: $count
echo
if diff "addTest.cpp" "addTestExpected.cpp" > /dev/null; then
	echo Test $count passed
else
	echo Test $count failed
	failurs+=1
fi

echo
echo Accelerator input:
cat addTest.acc
echo
echo Expected C++ output:
cat addTestExpected.cpp
echo
echo Actual C++ output:
cat addTest.cpp
echo

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