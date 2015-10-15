//
//  Hello.cpp
//  openmp
//
//  Created by Avi Chad-Friedman on 10/13/15.
//
//


#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <iostream>

using namespace std;

void fill(float ** cols){
    for(int i = 0; i<4; i++){
        //allocate memory for each row
        cols[i] = (float *)malloc(4*sizeof(float));
    }
}

void populate(float ** A, int cols, int rows){
    for(int i = 0; i<cols; i++){
        for(int j = 0; j<rows; j++){
           float f = static_cast <float> (rand()) / static_cast <float> (RAND_MAX/10.0);
           A[i][j] = floor(f);
        }
    }
}

//Source: http://www.appentra.com/parallel-matrix-matrix-multiplication/
int alg_matmul2D(int m, int n, int p, float** a, float** b, float** c)
{
    int i,j,k;
#pragma omp parallel shared(a,b,c) private(i,j,k)
    {
#pragma omp for  schedule(static)
        for (i=0; i<m; i=i+1){
            for (j=0; j<n; j=j+1){
                a[i][j]=0.;
                for (k=0; k<p; k=k+1){
                    a[i][j]=(a[i][j])+((b[i][k])*(c[k][j]));
                }
            }
        }
    }
    return 0;
}

void print_matrix(float ** A, int cols, int rows){
    for(int i = 0; i<cols; i++){
        cout<<endl;
        for(int j = 0; j<rows; j++){
            cout << A[i][j] << " ";
        }
    }
}

int main()
{
    float **a = (float **)malloc(4*sizeof(float *));
    float **b = (float **)malloc(4*sizeof(float *));
    float **c = (float **)malloc(4*sizeof(float *));
    fill(a);
    fill(b);
    fill(c);
    populate(a, 4, 4);
    populate(b, 4, 4);
    populate(c, 4, 4);
    print_matrix(a, 4, 4);
    cout << endl << "x";
    print_matrix(b, 4, 4);
    cout <<endl <<"=";
    alg_matmul2D(4, 4, 4, a, b, c);
    print_matrix(c, 4, 4);
    cout << endl;
    return 0;
}