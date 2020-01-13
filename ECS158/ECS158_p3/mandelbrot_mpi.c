#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <complex.h>
#include <math.h>
#include <mpi.h>
#include "matrix_checksum.c"


MPI_Status status;
MPI_Request request;
unsigned int matrix_checksum(int N, void *M, unsigned int size);

int main(int argc, char **argv) {
  struct timespec start, end;

	MPI_Init(&argc, &argv);
	int comm_size, comm_rank;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size); // returns the number of processes in the communicator
	MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank); // returns the rank of the calling proces

	// master thread
	if (comm_rank == 0){
		clock_gettime(CLOCK_MONOTONIC, &start);
		//Error Checking
		if(comm_size < 2){
			fprintf(stderr, "Error: not enough tasks\n");
			exit(1);
		}
		if(argc != 5)
		{
			fprintf(stderr, "Usage: mandelbrot_mpi xcenter ycenter zoom cutoff\n");
			exit(1);
		}
		int extraRows, numRows, offset;
		int workers = comm_size - 1;  //Number of worker
		double x_center = atof(argv[1]); //x coordinate of the plot’s area center. This parameter can be between -10.0 and 10.0.
		double y_center = atof(argv[2]); // y coordinate of the plot’s area center. This parameter can be between -10.0 and 10.0.
		int z = atoi(argv[3]); // zoom level. It can be between 0 and 100.
		int cutoff = atoi(argv[4]); //cutoff, that is the maximum number of iterations to run the series on a complex number, to determine whether the series stays bounded or escapes to infinity.
		int *M;
		int size = 1024;
		double distance = pow(2,(-z));
		M = (int*)malloc((size*size)*sizeof(int));

		if(x_center < -10.0 || x_center > 10.0){
			fprintf(stderr,"Error: wrong x-center (-10.000000 <= N <= 10.000000)\n");
      			exit(1);
		}
		else if(y_center < -10.0 || y_center > 10.0){
			fprintf(stderr,"Error: wrong y-center (-10.000000 <= N <= 10.000000)\n");
      			exit(1);
		}
		else if(z < 0 || z > 100){
			fprintf(stderr,"Error: wrong zoom (0 <= N <= 100)\n");
      			exit(1);
		}

		// Assignment
		extraRows = size % workers; // Extra to assign to worker threads
    offset = 0; // keep track of which rows to assign
		double x_min = x_center - (distance * (size/2));
		double y_min = y_center - (distance * (size/2));
		double x_curr = x_min;
		// double y_curr = y_min;

		for (int i = 1; i <= workers; i++){
			numRows = size/workers; // number of rows for workers to compute
			if(i <= extraRows){
				numRows+=1;
			}
			// I need to send in x_min, y_curr, numRows, and send back
			MPI_Send(&y_min, 1, MPI_DOUBLE, i, 1, MPI_COMM_WORLD);
			MPI_Send(&x_curr, 1, MPI_DOUBLE, i, 1, MPI_COMM_WORLD);
			MPI_Send(&numRows, 1, MPI_INT, i, 1, MPI_COMM_WORLD);
			MPI_Send(&cutoff, 1, MPI_INT, i, 1, MPI_COMM_WORLD);
			MPI_Send(&size, 1, MPI_INT, i, 1, MPI_COMM_WORLD);
			MPI_Send(&distance, 1, MPI_DOUBLE, i, 1, MPI_COMM_WORLD);
			MPI_Send(&offset, 1, MPI_INT, i, 1, MPI_COMM_WORLD);

      offset = offset + numRows;
			x_curr += distance*numRows;
		}

		MPI_Barrier(MPI_COMM_WORLD);

		for (int i=1; i<=workers; i++){
      MPI_Recv(&offset, 1, MPI_INT, i, 2, MPI_COMM_WORLD, &status);
      MPI_Recv(&numRows, 1, MPI_INT, i, 2, MPI_COMM_WORLD, &status);
      MPI_Recv(&M[offset*size], numRows*size, MPI_DOUBLE, i, 2, MPI_COMM_WORLD, &status);
    }
		clock_gettime(CLOCK_MONOTONIC, &end);
    double diff = end.tv_sec - start.tv_sec + (end.tv_nsec - start.tv_nsec)/1000000000.0;
    printf("Running time: %f secs\n", diff);
		printf("M: %u\n", matrix_checksum(size, M,sizeof(int)));

		FILE* pgmimg;
		char filename[40];
		sprintf(filename, "mandel_%f_%f_%d_%d.pgm", x_center, y_center, z, cutoff);
		pgmimg = fopen(filename, "wb");
		// Writing Magic Number to the File
		fprintf(pgmimg, "P2\n");
		// Writing Width and Height
		fprintf(pgmimg, "%d %d\n", size, size);
		// Writing the maximum gray value
		fprintf(pgmimg, "%d\n", cutoff);
		for (unsigned int i = 0; i < size; i++) {
			 for (unsigned int j = 0; j < size; j++) {
					 int temp = M[(i*size)+j];
					 // Writing the gray values in the 2D array to the file
					 fprintf(pgmimg, "%d ", temp);
			 }
			 fprintf(pgmimg, "\n");
		}
		fclose(pgmimg);

	}
	else{
		int source = 0;
		int *M;
		int numRows, cutoff, size, offset;
		double x_curr, y_min, distance;
		MPI_Recv(&y_min, 1, MPI_DOUBLE, source, 1, MPI_COMM_WORLD, &status);
		MPI_Recv(&x_curr, 1, MPI_DOUBLE, source, 1, MPI_COMM_WORLD, &status);
    MPI_Recv(&numRows, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
		MPI_Recv(&cutoff, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
		MPI_Recv(&size, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
		MPI_Recv(&distance, 1, MPI_DOUBLE, source, 1, MPI_COMM_WORLD, &status);
		MPI_Recv(&offset, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
    // printf(" worker/ x_curr: %f, offset: %d numRows: %d\n", x_curr, offset, numRows);
		M = (int*) malloc(size*numRows*sizeof(int));

		double y_curr = y_min;

		for(int i = 0; i < numRows; i++){
			for(int j = 0; j < size; j++){
				double complex output = 0;//((x_curr) + y_curr*I); //a new result for every point
        int diverge = 0;
				int k = 0;
      	for(; k < cutoff; k++){ //until a predefined maximum number of iteration
          if(cabs(output) <= 2.0) //magnitude of output //until the magnitude becomes larger than 2 (which means that the mandelbrot set is not bounded)
          {
              double complex temp  = (output*output) + ((x_curr) + y_curr*I);  //next = curr^2 + (x+yi)
              output = temp;
          }
          else
          {
              diverge = 1;
              break;
          }
      	}
				if(diverge == 1){
					// printf("[%d][%d] => (%f+%fi) => %d\n",i,j,x_curr,y_curr,k);
	    		M[(i*size)+j] = k;
	  		}
	  		else{ // diverges
//					 printf("[%d][%d] => (%f+%fi) => %d\n",i,j,x_curr,y_curr,cutoff);
	  			M[(i*size)+j] = cutoff;
				}
	       	y_curr += distance;
			}
			x_curr += distance;
			y_curr = y_min;
		}

		MPI_Barrier(MPI_COMM_WORLD);
    MPI_Send(&offset, 1, MPI_INT, 0, 2, MPI_COMM_WORLD);
    MPI_Send(&numRows, 1, MPI_INT, 0, 2, MPI_COMM_WORLD);
    MPI_Send(M, numRows*size, MPI_INT, 0, 2, MPI_COMM_WORLD);
	}
	MPI_Finalize();
	return 0;
}
