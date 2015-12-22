import random
import time
import sys
import numpy as np
from subprocess import call

# Random vector for genreating matrices
def random_vector(size, max_val):
	a = []
	for i in range(0,size):
		a.append(random.randrange(0,max_val))
	return(a)

# Create input for accelerator's matrix assignment
def generate_matrix_string(id, rvec, r, c):
	mat = id + " <- matrix(c("
	for i in range(0, r*c):
		if i != (len(rvec) - 1):
			mat += str(rvec[i]) + ", "
		else:
			mat += str(rvec[len(rvec)-1]) + "), nrow=" + str(r) + ",ncol=" + str(c) + ")\n"
	return(mat)

# Use NumPy to simulate a random matrix multiplication problem
def elapsed_matrix_op_time(r1, c1, r2, c2, max_val):
	py_start = time.clock()
	B = np.random.randn(r1,c1)
	C = np.random.randn(r2,c2)
	 
	A = np.dot(B.T, np.linalg.inv(C))
	B = np.mat(B)
	C = np.mat(C)
	 
	A = B.T*C.I

	py_fin = time.clock()
	py_elpased = py_fin - py_start

	print("Python run time: " + str(py_elpased) + " seconds\n")

# Testing Accelerator's speed
def test_matrices(id1, r1, c1, id2, r2, c2, max_val):
	
	f_in = open("acceleratorSource.acc", "w")
	f_out = open("acceleratorTest.cpp", "w")
	exc = open("acceleratorTest", "w")
	output = open("acceleratorTestOut.txt", "w")

	rvec1 = random_vector(r1*c1, max_val)
	rvec2 = random_vector(r2*c2, max_val)

	mstring1 = generate_matrix_string(id1, rvec1, r1, c1)
	mstring2 = generate_matrix_string(id2, rvec2, r2, c2)

	f_in.write("f <- function(){\n")
	f_in.write(mstring1)
	f_in.write(mstring2)
	f_in.write("c <- a + b\n")
	f_in.write("}\n")
	f_in.write("print(f())\n")
	f_in.close()

	source = open("acceleratorSource.acc", "r")

	call("cd .. && make clean && make", shell=True)
	call("chmod +x acceleratorTest", shell=True)

	acc_start = time.clock()
	call("../acc", stdin=source, stdout=f_out, shell=True)
	call("g++ -o acceleratorTest acceleratorTest.cpp",shell=True)
	call("./acceleratorTest", stdout=output, shell=True)
	acc_end = time.clock()
	acc_elapsed = acc_end - acc_start

	print("\nAccelerator run time: " + str(acc_elapsed) + " seconds")

	elapsed_matrix_op_time(r1, c1, r2, c2, max_val)

	source.close()
	f_out.close()
	exc.close()
	output.close()

if len(sys.argv) != 2:
	print '\nMissing argument n (# of rows / cols for each matrix operand)\n'
	sys.exit()

test_matrices("a", int(sys.argv[1]), int(sys.argv[1]), "b", int(sys.argv[1]), int(sys.argv[1]), 100)

