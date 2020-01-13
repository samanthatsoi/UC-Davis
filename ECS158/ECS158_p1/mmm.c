#define _POSIX_C_SOURCE 199309L
#define MIN(x, y) (((x) < (y)) ? (x) : (y)) //https://stackoverflow.com/questions/3437404/min-and-max-in-c

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <pthread.h>
#include "matrix_checksum.c"

//https://www.geeksforgeeks.org/multiplication-matrix-using-pthreads/


//each thread
typedef struct TCB
{
    pthread_t TID; //unique number
} TCB;

//argument to be passed into matrixMultiplypthread for each thread
typedef struct threadArgs {
    double *a; //matrix A
    double *b; //matrix B
    double *c; //matrix C
    int n; //size
    int threadno; //which thread number
    double sizeOfThread; //2000/8 =250
} threadArgs;

/* Define function prototype somewhere in your C file */
unsigned int matrix_checksum(int N, double *M);


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

//ijk
void matrixMultiplyijk(double *matA,double *matB,double *matC, int n) //good
{
    double sum; //sum to hold the element for each C element
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    for (unsigned int i=0; i<n; i++)
    {
        for (unsigned int j=0; j<n; j++) {
            sum = 0.0;
            for (unsigned int k=0; k<n; k++)
            {
                sum += (matA[(i*n)+k] * matB[(k*n)+j]);
            }
            matC[(i*n)+j] = sum;
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end

    timeittook(start, end); //self-defined function

    printMats(matA,matB,matC,n); //calls matrix_checksum

}

//ikj
void matrixMultiplyikj(double *matA,double *matB,double *matC, int n)
{
    double r; //sum to hold the element in matrix A while traversing B
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    for (unsigned int i=0; i<n; i++)
    {
        for (unsigned int k=0; k<n; k++) {
            r = matA[i*n+k];
            for (unsigned int j=0; j<n; j++)
            {
                matC[i*n+j] += r * matB[k*n+j];
            }
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum

}

//jik
void matrixMultiplyjik(double *matA,double *matB,double *matC, int n) //good
{
    double sum; //sum to hold the element for each C element
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    for (unsigned int j=0; j<n; j++)
    {
        for (unsigned int i=0; i<n; i++) {
            sum = 0.0;
            for (unsigned int k=0; k<n; k++)
            {
                sum += matA[i*n + k] * matB[k*n+j];
            }
            matC[i*n+j] = sum;
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum

}

//jki
void matrixMultiplyjki(double *matA,double *matB,double *matC, int n)
{
    double r; //sum to hold the element in matrix B while traversing A
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    for (unsigned int j=0; j<n; j++)
    {
        for (unsigned int k=0; k<n; k++) {
            r = matB[k*n+j];
            for (unsigned int i=0; i<n; i++)
            {
                matC[i*n+j] += matA[i*n+k] *r;
            }
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum

}

//kij
void matrixMultiplykij(double *matA,double *matB,double *matC, int n)
{
    double r; //sum to hold the element in matrix A while traversing B
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    for (unsigned int k=0; k<n; k++)
    {
        for (unsigned int i=0; i<n; i++) {
            r = matA[i*n+k];
            for (unsigned int j=0; j<n; j++)
            {
                matC[i*n+j] += r * matB[k*n+j];
            }
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum
}

//kji
void matrixMultiplykji(double *matA,double *matB,double *matC, int n)
{
    double r; //sum to hold the element in matrix B while traversing A
    struct timespec start, end; //for time
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    for (unsigned int k=0; k<n; k++)
    {
        for (unsigned int j=0; j<n; j++) {
            r = matB[k*n+j];
            for (unsigned int i=0; i<n; i++)
            {
                matC[i*n+j] += matA[i*n+k] *r;
            }
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum
}


//phase 3
void *matrixMultiplypthread(void * arg)
{
//defining these variables to be used
    threadArgs *threadInfo =  (struct threadArgs *)arg;
    double *matA = threadInfo->a;
    double *matB = threadInfo->b;
    double *matC = threadInfo->c;
    int n = threadInfo->n;
    int threadnum = threadInfo->threadno;
    double threadsize = threadInfo->sizeOfThread;
    double r;

//splitting the matrix in 8
    for (unsigned int i=threadnum*threadsize; i<((threadnum+1)*threadsize); i++)
    {
        for (unsigned int k=0; k<n; k++) {
            r = matA[i*n+k];
            for (unsigned int j=0; j<n; j++)
            {
                matC[i*n+j] += r * matB[k*n+j];
            }
        }
    }

    return 0;

}

//we chose ikj to do threads on
void initializeThreadsikj(double *matA,double *matB,double *matC, int size)
{
   int noOfThreads = 8;

    //initialize threadInfo
    threadArgs *threadInfo = malloc(sizeof(struct threadArgs));
    threadInfo->a = matA;
    threadInfo->b = matB;
    threadInfo->c = matC;
    threadInfo->n = size;
    threadInfo->threadno = 0;
    threadInfo->sizeOfThread = size/noOfThreads;

    TCB threads[noOfThreads]; //8 threads
//for (unsigned int i =0;i<noOfThreads;i++)
//  threadInfo->threadno = i;
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    //creating 8 threads, each evaluating its own part
    for (unsigned int i =0; i < noOfThreads; i++)
    {
  threadArgs *threadInfo = malloc(sizeof(struct threadArgs));
    threadInfo->a = matA;
    threadInfo->b = matB;
    threadInfo->c = matC;
    threadInfo->n = size;
    threadInfo->threadno = i;
    threadInfo->sizeOfThread = size/noOfThreads;//        threadInfo->threadno = i;
        //create the thread, passing into matrixMultiplypthread with threadInfo as argument
        pthread_create(&threads[i].TID, NULL, matrixMultiplypthread, (void *)threadInfo);
    }

//join the threads after computation
    for (unsigned int i = 0; i < noOfThreads; i++)
        pthread_join(threads[i].TID, NULL);

    clock_gettime(CLOCK_MONOTONIC, &end); //end

    timeittook(start, end); //self-defined function

    printMats(threadInfo->a,threadInfo->b,threadInfo->c,size); //calls matrix_checksum

}

//phase 4
//http://www.cs.rochester.edu/~sandhya/csc252/lectures/lecture-memopt.pdf
void bijk(double *matA,double *matB,double *matC,int n) {
//ijk
    int B = 8; //size of the sub block
    double sum;
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
//going through each sub block
    for(unsigned int jj = 0; jj < n; jj +=B)
    {
//traversing down matrix A
        for(unsigned int i = 0; i<n; i++)
        {
//each widht of the sub block
            for (unsigned int j = jj; j < MIN(jj+B,n); j++)
            {
                matC[i*n+j]  = 0.0;
            }
        }
//going through each sub block
        for(unsigned int kk = 0; kk <n; kk+=B)
        {
//for all rows of the sub block
            for(unsigned int i= 0; i<n; i++)
            {
//traverse the width of matrix B sub block here
                for(unsigned int j =jj; j<MIN(jj+B, n); j++) //MIN is used for if end of the last sub block != n
                {
                    sum = 0.0;
//traverse the width of matrix A sub block here
                    for(unsigned int k = kk; k< MIN(kk+B, n); k++)
                    {
                        sum += matA[i*n+k] * matB[k*n+j];
                    }
                    matC[i*n+j] += sum;
                }
            }
        }
    }


    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printMats(matA,matB,matC,n); //calls matrix_checksum

}

// 6 possible permutations of the i-j-k loops: “ijk”, “ikj”, “jik”, “jki”, “kij” and “kji”.
int main(int argc, char **argv)
{

    //5.2.1 command line error checking
    if(argc != 3)
    {
        fprintf(stderr,"Usage: ./mmm <ijk_order> N\n");
        exit(1);
    }

    char* approach = argv[1]; //argv[1] is the first argument “ijk”, “ikj”, “jik”, “jki”, “kij” and “kji”
    int n = atoi(argv[2]); //argv[2] is the second argument
    if(strcmp(approach, "ijk") != 0 && strcmp(approach, "ikj") != 0 && strcmp(approach, "jik") != 0 && strcmp(approach, "jki") != 0 && strcmp(approach, "kij") != 0 && strcmp(approach, "kji") != 0
            && strcmp(approach, "pthread") != 0 && strcmp(approach, "bijk") != 0)
    {
        fprintf(stderr,"Error: wrong <ijk> order\n");
        exit(1);
    }
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
    if(strcmp(approach, "ijk") == 0)
        matrixMultiplyijk(matA,matB,matC,n);
    else if(strcmp(approach, "ikj") == 0)
        matrixMultiplyikj(matA,matB,matC,n);
    else if(strcmp(approach, "jik") == 0)
        matrixMultiplyjik(matA,matB,matC,n);
    else if(strcmp(approach, "jki") == 0)
        matrixMultiplyjki(matA,matB,matC,n);
    else if(strcmp(approach, "kij") == 0)
        matrixMultiplykij(matA,matB,matC,n);
    else if(strcmp(approach, "kji") == 0)
        matrixMultiplykji(matA,matB,matC,n);
    else if(strcmp(approach, "pthread") == 0)
        initializeThreadsikj(matA,matB,matC,n); //initialize threads
    else if(strcmp(approach, "bijk") == 0) //phase 4, blocking method
        bijk(matA,matB,matC,n);





    return 0;
}