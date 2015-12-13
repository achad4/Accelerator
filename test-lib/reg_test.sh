#!/bin/bash
make clean && make

# Count number of tests and failures
declare -i count
declare -i failures
count=0
failures=0

runTest() {
	./acc < sourceFiles/$1.acc > compiledCpp/$1.cpp
	g++ -o executables/$1 compiledCpp/$1.cpp
	./executables/$1 > output/$1.txt
	count+=1
	if diff "output/"$1".txt" "expected/"$1"Expected.txt" > /dev/null; then
		echo $1 passed
	else
	echo $1 failed
	failures+=1
	fi
}

runTest "addTest"
runTest "subTest"
runTest "multTest"
runTest "divTest"
runTest "expoTest"
runTest "modTest"
runTest "assignTest"
runTest "trueLitTest"
runTest "falseLitTest"
runTest "andTest"
runTest "orTest"
runTest "notTest"
runTest "multiStatementTest"
runTest "floatLitTest"
runTest "floatOpTest"
runTest "assignAndPrintTest"
runTest "vectorTest"
runTest "ifElseTest"
runTest "forTest"
runTest "matrixTest"
runTest "stringTest"

echo ====================================================
echo Results
echo ====================================================
echo Test Suite finished
echo Failed Tests: $failures
echo Total Tests: $count
make clean
