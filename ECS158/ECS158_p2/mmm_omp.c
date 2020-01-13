#define _POSIX_C_SOURCE 199309L

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <pthread.h>
#include <omp.h>
#include "matrix_checksum.c"



void timeittook(struct timespec start, struct timespec end);

//http://www.guyrutenberg.com/2007/09/22/profiling-code-using-clock_gettime/
//finds the difference in time. end-start
void timeittook(struct timespec start, struct timespec end)
{
    struct timespec temp;

    if ((end.tv_nsec-start.tv_nsec)<0) //if negative
    {
        temp.tv_sec = end.tv_sec-start.tv_sec-1;
        temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
    }
    else //normal
    {
        temp.tv_sec = end.tv_sec-start.tv_sec;
        temp.tv_nsec = end.tv_nsec-start.tv_nsec;
    }
    printf("Running time: %ld.%ld secs\n",temp.tv_sec, temp.tv_nsec);
    // return temp;
}

//prints out the matrices using the matrix_checksum hash
void printMats(double *matA, double *matB, double* matC, int n)
{
    printf("A: %u\n", matrix_checksum(n, matA));
    printf("B: %u\n", matrix_checksum(n, matB));
    printf("C: %u\n", matrix_checksum(n, matC));
}
//ikj
void matrixMultiplyikj(double *matA,double *matB,double *matC, int n)
{

    double r; //sum to hold the element in matrix A while traversing B
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time

#pragma omp parallel for schedule(static, n/8)
for (unsigned int i=0; i<n; i++)
    {
//#pragma omp task
//{
//#pragma omp parallel for
	#pragma omp task
        for (unsigned int k=0; k<n; k++)
	 {
//#pragma omp parallel sections
//{
//#pragma omp section 
           r = matA[i*n+k]; 
#pragma omp task          
 for (unsigned int j=0; j<n; j++)
            {
		
		
                matC[i*n+j] += r * matB[k*n+j];
		

            }
//}
        }
}



    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum

}

// 6 possible permutations of the i-j-k loops: “ijk”, “ikj”, “jik”, “jki”, “kij” and “kji”.
int main(int argc, char **argv)
{
//    #pragma omp parallel
    //5.2.1 command line error checking
    if(argc != 2)
    {
        fprintf(stderr,"Usage: ./mmm_omp N\n");
        exit(1);
    }


    int n = atoi(argv[1]); //argv[2] is the second argument
    if(n > 2000 || n <= 0)
    {
        fprintf(stderr,"Error: wrong matrix order (0 < N <= 2000)\n");
        exit(1);
    }


    //initialize matA, matB, matC and malloc
    double *matA ;
    double *matB;
    double *matC;

    matA = malloc((2000*2000+2000)*sizeof(double));
    matB = malloc((2000*2000+2000)*sizeof(double));
    matC = malloc((2000*2000+2000)*sizeof(double));

    //fill in values for matA and matB as given in the prompt
    for(unsigned int i = 0; i < n; i ++)
    {
        for(unsigned int j = 0; j<n; j++)
        {
            matA[i*n+j] = i+j;
            matB[i*n+j] = i+j*2;
        }
    }

    //initialize threads

    matrixMultiplyikj(matA,matB,matC,n);
  





    return 0;
}
