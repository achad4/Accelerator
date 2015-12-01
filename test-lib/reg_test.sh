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

# Testing Division
./acc < sourceFiles/divTest.acc > compiledCpp/divTest.cpp
g++ -o executables/divTest compiledCpp/divTest.cpp
./executables/divTest > output/divTestOutput.txt
count+=1
if diff "output/divTestOutput.txt" "expected/divTestExpected.txt" > /dev/null; then
	echo divTest passed
else
	echo divTest failed
	failures+=1
fi

# Testing Exponentiation
./acc < sourceFiles/expoTest.acc > compiledCpp/expoTest.cpp
g++ -o executables/expoTest compiledCpp/expoTest.cpp
./executables/expoTest > output/expoTestOutput.txt
count+=1
if diff "output/expoTestOutput.txt" "expected/expoTestExpected.txt" > /dev/null; then
	echo expoTest passed
else
	echo expoTest failed
	failures+=1
fi

# Testing Modulus
./acc < sourceFiles/modTest.acc > compiledCpp/modTest.cpp
g++ -o executables/modTest compiledCpp/modTest.cpp
./executables/modTest > output/modTestOutput.txt
count+=1
if diff "output/modTestOutput.txt" "expected/modTestExpected.txt" > /dev/null; then
	echo modTest passed
else
	echo modTest failed
	failures+=1
fi

# Testing Assignment
./acc < sourceFiles/assignTest.acc > compiledCpp/assignTest.cpp
g++ -o executables/assignTest compiledCpp/assignTest.cpp
./executables/assignTest > output/assignTestOutput.txt
count+=1
if diff "output/assignTestOutput.txt" "expected/assignTestExpected.txt" > /dev/null; then
	echo assignTest passed
else
	echo assignTest failed
	failures+=1
fi

# Testing boolean literal true
./acc < sourceFiles/trueLitTest.acc > compiledCpp/trueLitTest.cpp
g++ -o executables/trueLitTest compiledCpp/trueLitTest.cpp
./executables/trueLitTest > output/trueLitTestOutput.txt
count+=1
if diff "output/trueLitTestOutput.txt" "expected/trueLitTestExpected.txt" > /dev/null; then
	echo trueLitTest passed
else
	echo trueLitTest failed
	failures+=1
fi

# Testing boolean literal false
./acc < sourceFiles/falseLitTest.acc > compiledCpp/falseLitTest.cpp
g++ -o executables/falseLitTest compiledCpp/falseLitTest.cpp
./executables/falseLitTest > output/falseLitTestOutput.txt
count+=1
if diff "output/falseLitTestOutput.txt" "expected/falseLitTestExpected.txt" > /dev/null; then
	echo falseLitTest passed
else
	echo falseLitTest failed
	failures+=1
fi

# Testing boolean and
./acc < sourceFiles/andTest.acc > compiledCpp/andTest.cpp
g++ -o executables/andTest compiledCpp/andTest.cpp
./executables/andTest > output/andTestOutput.txt
count+=1
if diff "output/andTestOutput.txt" "expected/andTestExpected.txt" > /dev/null; then
	echo andTest passed
else
	echo andTest failed
	failures+=1
fi

# Testing boolean or


g++ -o executables/orTest compiledCpp/orTest.cpp
./executables/orTest > output/orTestOutput.txt
count+=1
if diff "output/orTestOutput.txt" "expected/orTestExpected.txt" > /dev/null; then
	echo orTest passed
else
	echo orTest failed
	failures+=1
fi

# Testing boolean not
./acc < sourceFiles/notTest.acc > compiledCpp/notTest.cpp
g++ -o executables/notTest compiledCpp/notTest.cpp
./executables/notTest > output/notTestOutput.txt
count+=1
if diff "output/notTestOutput.txt" "expected/notTestExpected.txt" > /dev/null; then
	echo notTest passed
else
	echo notTest failed
	failures+=1
fi

# Testing boolean not
./acc < sourceFiles/multiStatementTest.acc > compiledCpp/multiStatementTest.cpp
g++ -o executables/multiStatementTest compiledCpp/multiStatementTest.cpp
./executables/multiStatementTest > output/multiStatementTest.txt
count+=1
if diff "output/multiStatementTest.txt" "expected/multiStatementTestExpected.txt" > /dev/null; then
	echo multiStatementTest passed
else
	echo multiStatementTest failed
	failures+=1
fi

# Testing float literal
./acc < sourceFiles/floatLitTest.acc > compiledCpp/floatLitTest.cpp
g++ -o executables/floatLitTest compiledCpp/floatLitTest.cpp
./executables/floatLitTest > output/floatLitTest.txt
count+=1
if diff "output/floatLitTest.txt" "expected/floatLitTestExpected.txt" > /dev/null; then
	echo floatLitTest passed
else
	echo floatLitTest failed
	failures+=1
fi

# Testing float addition
./acc < sourceFiles/floatOpTest.acc > compiledCpp/floatOpTest.cpp
g++ -o executables/floatOpTest compiledCpp/floatOpTest.cpp
./executables/floatOpTest > output/floatOpTest.txt
count+=1
if diff "output/floatOpTest.txt" "expected/floatOpTestExpected.txt" > /dev/null; then
	echo floatOpTest passed
else
	echo floatOpTest failed
	failures+=1
fi

# Testing assign and print
./acc < sourceFiles/assignAndPrintTest.acc > compiledCpp/assignAndPrintTest.cpp
g++ -o executables/assignAndPrintTest compiledCpp/assignAndPrintTest.cpp
./executables/assignAndPrintTest > output/assignAndPrintTest.txt
count+=1
if diff "output/assignAndPrintTest.txt" "expected/assignAndPrintTestExpected.txt" > /dev/null; then
	echo assignAndPrintTest passed
else
	echo assignAndPrintTest failed
	failures+=1
fi

echo ====================================================
echo Results
echo ====================================================
echo Test Suite finished
echo Failed Tests: $failures
echo Total Tests: $count
make clean
