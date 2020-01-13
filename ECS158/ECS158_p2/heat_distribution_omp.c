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
#include <math.h>

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

int main(int argc, char **argv) {
	
	if(argc != 5) {
		fprintf(stderr, "Usage: ./heat_distribution_omp N fire_temp wall_temp epsilon\n");
		exit(1);
	}

	int matrix_order = atoi(argv[1]);
	double north_wall = atof(argv[2]);
	double remain_walls = atof(argv[3]);
	double epsilon = atof(argv[4]);
	/*Check if user enters wrong input*/
	if(matrix_order < 1 || matrix_order > 2000) {
		fprintf(stderr,"Error: wrong map order (3 <= N <= 2000)\n");
        exit(1);
	}

	else if(north_wall < 0 || north_wall > 100) {
		fprintf(stderr,"Error: wrong fire temperature (0.000000 <= N <= 100.000000)\n");
        exit(1);
	}

	else if(remain_walls < 0 || remain_walls > 100) {
		fprintf(stderr,"Error: wrong wall temperature (0.000000 <= N <= 100.000000)\n");
        exit(1);
	}

	else if(epsilon < 1e-6 || epsilon > 100) {
		fprintf(stderr,"Error: wrong epsilon (0.000001 <= N <= 100.000000)\n");
        exit(1);
	}
    /*declare pointer for 2D arrays*/
	double *h ;
    double *g;

    h = malloc((2000*2000+2000)*sizeof(double));
    g = malloc((2000*2000+2000)*sizeof(double));
 
 	double perimeter = (matrix_order -1)*4;
    double init_interior = ((north_wall * matrix_order) + (remain_walls * (perimeter - matrix_order)))/perimeter;
    /*fill in values*/

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start); //start time
    /*Parallelize for loop for faster time, initializin the H matrix*/
#pragma omp parallel for
    for(unsigned int i = 0; i < matrix_order; i ++) {
    	
        for(unsigned int j = 0; j< matrix_order; j++) {
        	//corner cases
 			if((i == 0 && j==0) || (i== 0 && j == matrix_order - 1)) { //hitting the corners
 				h[i * matrix_order + j] = north_wall;
 			}
        	else if(i == 0) {
        		h[i * matrix_order + j] = north_wall;
        	
        	}
            else if((j == 0 || j == (matrix_order - 1))) {
        		h[i * matrix_order + j] = remain_walls;
        	}

            else if(i == matrix_order - 1) {
        		h[i * matrix_order + j] = remain_walls;
        	}
        	else //initialize interior points to the mean value of edge points
        	{
        		h[i * matrix_order + j] = init_interior;
        	}

        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end); //end
    timeittook(start, end); //self-defined function
    printf("mean: %f\n", init_interior);
    printf("hmap: %u\n", matrix_checksum(matrix_order, h));

    double sumMean = 0;
    struct timespec start2, end2;
    double max = 0;
    clock_gettime(CLOCK_MONOTONIC, &start2); //start time
    unsigned int iteration = 0;
    unsigned int powerOfTwo = 1;
    /*Computer the new hmap and stop looping when max has 
    exceeded the threshold*/
    while(max >= epsilon || max == 0) 
    {
    	max = 0;
    	iteration++;
        
    #pragma omp parallel for collapse(2) reduction(+:sumMean)
	    for(unsigned int i = 1; i < matrix_order -1; i ++) {
	        for(unsigned int j = 1; j< matrix_order -1; j++) {
	        	double newval = ((h[(i-1) * matrix_order + j]) + (h[(i+1) * matrix_order + j]) + (h[i * matrix_order + (j-1)]) + (h[i * matrix_order + (j+1)]))/4;
	        	g[i*matrix_order + j] = newval;

	        	sumMean += g[i*matrix_order + j];
	        }
	     }
    #pragma omp for
	    for(unsigned int i = 1; i < matrix_order -1; i ++) {
	        for(unsigned int j = 1; j< matrix_order -1; j++) {
	        	double compare = fabs(g[i*matrix_order + j] - h[i*matrix_order + j]);
	        	if(compare > max)
	        		max = compare;
	        	h[i*matrix_order + j] = g[i*matrix_order + j];
	        }
	     }
 
    //#pragma omp parallel reduction(*:powerOfTwo)
	     if(iteration == powerOfTwo)
	     {
	     	printf("%d\t%f\n", iteration, max);
	     	powerOfTwo *= 2;
	     }
	 }
	printf("%d\t%f\n", iteration, max);
    clock_gettime(CLOCK_MONOTONIC, &end2); //end
    timeittook(start2, end2); //self-defined function
    printf("hmap: %u\n", matrix_checksum(matrix_order, h));
    
	return 0;
}