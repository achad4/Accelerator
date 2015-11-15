#!/bin/bash
make clean && make
./acc < addTest.acc > addTest.c
diff addTest.c addTestExpected.c

make clean
