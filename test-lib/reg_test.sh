#!/bin/bash
make clean && make
./acc < addTest.acc > addTest.c

make clean
