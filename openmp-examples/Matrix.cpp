#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include<vector>

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


vector<vector<int> > matrix_mult(vector<vector<int> > a, vector<vector<int> > b){
    vector<vector<int> > c;
    int i,j,k;
#pragma omp parallel shared(a,b,c) private(i,j,k)
    {
#pragma omp for  schedule(static)
        for (i=0; i<a.size(); i=i+1){
            vector<int> row;
            for (j=0; j<b[0].size(); j=j+1){
                row.push_back(0);
                for (k=0; k<b.size(); k=k+1){
                    row[j] += ((a[i][k])*(b[k][j]));
                }
            }
            c.push_back(row);
        }
    }
    return c;
}





void print_matrix(float ** A, int cols, int rows){
    for(int i = 0; i<cols; i++){
        cout<<endl;
        for(int j = 0; j<rows; j++){
            cout << A[i][j] << " ";
        }
    }
}



vector<vector<int> > matrix_add(vector<vector<int> > a, vector<vector<int> > b){
    vector<vector<int> > c;
    cout<<a[0].size()<<endl;
    for(int i = 0; i < a.size(); i++){
        vector<int> row;
         for(int j = 0; j < a[0].size(); j++){
             row.push_back(a[i][j] + b[i][j]);
         }
        c.push_back(row);
    }
    return c;
}






void print_matrix(vector<vector<int> > a){
    for(int i = 0; i<a.size(); i++){
        cout<<endl;
        for(int j = 0; j<a[0].size(); j++){
            cout << a[i][j] << " ";
        }
    }
}

void testAdd(){
    int mHolder[] = {1, 2, 3, 4};
    vector<int> mVector (mHolder, mHolder + 2 / sizeof(int));
    vector<vector<int> > m (2);
    int mcount=0;
    for(int i=0; i<2; i++) {
        m[i].resize(2);
        for(int j=0; j<2; j++) {
            m[i][j] = mHolder[mcount++];
        }
    }
   
    print_matrix(m);
    vector<vector<int> > result =  matrix_add(m, m);
    print_matrix(result);
}

void testMult(){
    int mHolder[] = {1, 2, 3, 4};
    vector<int> mVector (mHolder, mHolder + 2 / sizeof(int));
    vector<vector<int> > m (2);
    int mcount=0;
    for(int i=0; i<2; i++) {
        m[i].resize(2);
        for(int j=0; j<2; j++) {
            m[i][j] = mHolder[mcount++];
        }
    }
    vector<vector<int> > result =  matrix_mult(m, m);
    print_matrix(result);
}



int main()
{
    float **a = (float **)malloc(4*sizeof(float *));
    float **b = (float **)malloc(4*sizeof(float *));
    float **c = (float **)malloc(4*sizeof(float *));
    fill(a);
    fill(b);
    fill(c);
//    populate(a, 4, 4);
//    populate(b, 4, 4);
//    print_matrix(a, 4, 4);
//    cout << endl << "x";
//    print_matrix(b, 4, 4);
//    cout <<endl <<"=";
//    alg_matmul2D(4, 4, 4, c, a, b);
//    print_matrix(c, 4, 4);
//    cout << endl;
//    testAdd();
    testMult();

    
    
    
    return 0;
}