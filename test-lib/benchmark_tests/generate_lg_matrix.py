import random
import time
from subprocess import call

def random_vector(size, max_val):
	a = []
	for i in range(0,size):
		a.append(random.randrange(0,max_val))
	return(a)

def vect_to_matrix(r,c,v):
	m = []
	count = 0
	for i in range(0,r):
		row = []
		for j in range(0,c):
			row.append(++count)
		m.append(row)
	return(m)

def generate_matrix_string(id, rvec, r, c):
	mat = id + " <- matrix(c("
	for i in range(0, r*c):
		if i != (len(rvec) - 1):
			mat += str(rvec[i]) + ", "
		else:
			mat += str(rvec[len(rvec)-1]) + "), nrow=" + str(r) + ",ncol=" + str(c) + ")\n"
	return(mat)

def elapsed_matrix_op_time(m1, r1, c1, m2, r2, c2, max_val):
	py_start = time.clock()
	res_v = random_vector(r1 * c2, max_val)
	res_m = vect_to_matrix(r1, c1, res_v)

	for i in range(len(m1)):
		for j in range(len(m2[0])):
			for k in range(len(m2)):
				res_m[i][j] += m1[i][k] * m2[k][j]

	py_fin = time.clock()
	py_elpased = py_fin - py_start
	print("Python run time: " + str(py_elpased) + " seconds")

def test_matrices(id1, r1, c1, id2, r2, c2, max_val):
	
	f_in = open("acceleratorSource.acc", "w")
	f_out = open("acceleratorTest.cpp", "w")
	exc = open("acceleratorTest", "w")
	output = open("acceleratorTestOut.txt", "w")

	rvec1 = random_vector(r1*c1, max_val)
	rvec2 = random_vector(r2*c2, max_val)

	m1 = vect_to_matrix(r1,c1,rvec1)
	m2 = vect_to_matrix(r2,c2,rvec2)

	mstring1 = generate_matrix_string("a", rvec1, r1, c1)
	mstring2 = generate_matrix_string("b", rvec2, r2, c2)

	f_in.write(mstring1)
	f_in.write(mstring2)
	f_in.write("c <- a + b\n")
	f_in.close()

	source = open("acceleratorSource.acc", "r")

	call("cd .. && make clean && make", shell=True)
	call("chmod +x acceleratorTest", shell=True)
	call("../acc", stdin=source, stdout=f_out, shell=True)
	call("g++ -o acceleratorTest acceleratorTest.cpp",shell=True)

	acc_start = time.clock()
	call("./acceleratorTest", stdout=output, shell=True)
	acc_end = time.clock()
	acc_elapsed = acc_end - acc_start

	print("Accelerator run time: " + str(acc_elapsed) + " seconds")

	elapsed_matrix_op_time(m1, r1, c1, m2, r2, c2, max_val)

	source.close()
	f_out.close()
	exc.close()
	output.close()

test_matrices("a", 100, 100, "b", 100, 100, 100)

