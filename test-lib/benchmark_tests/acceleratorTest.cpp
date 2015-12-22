#include<iostream>
#include<stdio.h>
#include<math.h>
#include<vector>
#include<string>
#include<string.h>
using namespace std;
template<typename t>
   vector<vector<t> > matrix_add(vector<vector<t> > a, vector<vector<t> > b){
       vector<vector<t> > c;
       for(int i = 0; i < a.size(); i++){
           vector<t> row;
            for(int j = 0; j < a[0].size(); j++){
                row.push_back(a[i][j] + b[i][j]);
            }
           c.push_back(row);
       }
       return c;
   }

   template<typename t>
   void print_matrix(vector<vector<t> > a){
       for(int i = 0; i<a.size(); i++){
           cout<<endl;
           for(int j = 0; j<a[0].size(); j++){
               cout << a[i][j] << " ";
           }
       }
   }
int main(){
int aHolder[] = {47, 44, 99, 95, 26, 4, 21, 83, 77, 12, 4, 2, 6, 19, 64, 82, 10, 40, 46, 21, 19, 43, 87, 81, 89, 62, 37, 56, 48, 37, 60, 0, 66, 33, 58, 36, 21, 71, 14, 90, 21, 76, 79, 68, 52, 35, 42, 29, 53, 80, 82, 56, 67, 33, 36, 81, 17, 82, 38, 67, 22, 24, 10, 98, 77, 21, 9, 93, 40, 5, 9, 50, 14, 75, 34, 84, 2, 40, 65, 44, 87, 21, 49, 20, 48, 36, 50, 39, 93, 98, 88, 32, 51, 96, 39, 12, 0, 25, 32, 80};
vector<int> aVector (aHolder, aHolder + 10 / sizeof(int));
vector<vector<int> > a (10);
int acount=0;
for(int i=0; i<10; i++) { 
a[i].resize(10);
for(int j=0; j<10; j++) { 
a[i][j] = aHolder[acount++];
}
};
int bHolder[] = {51, 87, 97, 21, 17, 82, 47, 79, 55, 95, 92, 56, 24, 64, 58, 55, 34, 67, 3, 51, 27, 32, 9, 26, 3, 83, 18, 85, 86, 43, 68, 65, 86, 60, 0, 41, 0, 34, 95, 69, 82, 74, 85, 4, 63, 78, 86, 45, 20, 91, 71, 65, 45, 91, 1, 49, 64, 78, 16, 16, 97, 9, 69, 69, 87, 55, 53, 46, 72, 15, 1, 18, 34, 35, 64, 72, 61, 27, 4, 59, 75, 19, 93, 8, 71, 68, 59, 90, 27, 37, 68, 61, 56, 46, 1, 13, 16, 28, 6, 94};
vector<int> bVector (bHolder, bHolder + 10 / sizeof(int));
vector<vector<int> > b (10);
int bcount=0;
for(int i=0; i<10; i++) { 
b[i].resize(10);
for(int j=0; j<10; j++) { 
b[i][j] = bHolder[bcount++];
}
};
vector<vector<int> > result = matrix_add(a, b);print_matrix(result);
}

