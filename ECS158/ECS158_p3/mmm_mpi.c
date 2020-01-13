#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <mpi.h>
#include "matrix_checksum.c"

MPI_Status status;
MPI_Request request;
unsigned int matrix_checksum(int N, void *M, unsigned int size);

int main(int argc, char **argv){

  MPI_Init(&argc, &argv);
  int comm_size, comm_rank, numRows, offset, extraRows;
  MPI_Comm_size(MPI_COMM_WORLD, &comm_size); // returns the number of processes in the communicator
  MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank); // returns the rank of the calling proces

  // master thread
  struct timespec start, end;

  if (comm_rank == 0) {
    clock_gettime(CLOCK_MONOTONIC, &start);

    //Error checking
    if(argc != 2){
      fprintf(stderr,"Usage: mmm_mpi N");
  		return 1;
    }


    int N = atoi(argv[1]);
	if(N <= 0)
{
fprintf(stderr,"Error: wrong matrix order (N > 0)");
return 1;
}

    int workers = comm_size - 1;  //Number of worker
    double* A = (double*) malloc(N*N*sizeof(double));
    double* B = (double*) malloc(N*N*sizeof(double));
    double* C = (double*) malloc(N*N*sizeof(double));
    //Initializing A and B matrices
    for(int i = 0; i < N; i++) {
      for(int j = 0; j < N; j++) {
        A[i*N+j] = i + j;
      }
    }
    for(int i = 0; i < N; i++) {
      for(int j = 0; j < N; j++){
        B[i*N+j] = i + j * 2;
      }
    }

    extraRows = N % workers;  // Extra to assign to worker threads
    offset = 0; // keep track of which rows to assign
    for (int i = 1; i <= workers; i++){
      numRows = N/workers; // number of rows for workers to compute
      if(i <= extraRows){
        numRows+=1;
      }
      // printf("number of row to compute is is %d \n",numRows);
      // printf("offset is %d\n",offset);
      MPI_Send(&offset, 1, MPI_INT, i, 1, MPI_COMM_WORLD);
      MPI_Send(&numRows, 1, MPI_INT, i, 1, MPI_COMM_WORLD);
      
MPI_Send(&N, 1,  MPI_INT,i,1, MPI_COMM_WORLD);
MPI_Send(&A[offset*N], numRows*N, MPI_DOUBLE,i,1, MPI_COMM_WORLD);
      MPI_Send(B, N*N, MPI_DOUBLE, i, 1, MPI_COMM_WORLD);
      offset = offset + numRows;
    }
    //wait until all the workers to finish
    MPI_Barrier(MPI_COMM_WORLD);
    for (int i=1; i<=workers; i++)
    {
      MPI_Recv(&offset, 1, MPI_INT, i, 2, MPI_COMM_WORLD, &status);
      MPI_Recv(&numRows, 1, MPI_INT, i, 2, MPI_COMM_WORLD, &status);
      MPI_Recv(&C[offset*N], numRows*N, MPI_DOUBLE, i, 2, MPI_COMM_WORLD, &status);
    }
    clock_gettime(CLOCK_MONOTONIC, &end);
    double diff = end.tv_sec - start.tv_sec + (end.tv_nsec - start.tv_nsec)/1000000000.0;
    printf("Running time: %f secs\n", diff);
    printf("A: %u\n", matrix_checksum(N, A, sizeof(double)));
    printf("B: %u\n", matrix_checksum(N, B, sizeof(double)));
    printf("C: %u\n", matrix_checksum(N, C, sizeof(double)));

  }
  // worder threads
  if (comm_rank > 0) {
    int source = 0;
	int N;
    MPI_Recv(&offset, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
   MPI_Recv(&numRows, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
 MPI_Recv(&N, 1, MPI_INT, source, 1, MPI_COMM_WORLD, &status);
    
double* A = (double*) malloc(numRows*N*sizeof(double));
    double* B = (double*) malloc(N*N*sizeof(double));
    double* C = (double*) malloc(numRows*N*sizeof(double));
    MPI_Recv(A, numRows*N, MPI_DOUBLE, source, 1, MPI_COMM_WORLD, &status);
    MPI_Recv(B, N*N, MPI_DOUBLE, source, 1, MPI_COMM_WORLD, &status);
    for (int i = 0; i < numRows; i++) {
      for (int k = 0; k < N; k++) {
        double r = A[i*N+k];
        for (int j = 0; j < N; j++) {
          C[i*N+j] += r * B[k*N+j];
        }
      }
    }
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Send(&offset, 1, MPI_INT, 0, 2, MPI_COMM_WORLD);
    MPI_Send(&numRows, 1, MPI_INT, 0, 2, MPI_COMM_WORLD);
    MPI_Send(C, numRows*N, MPI_DOUBLE, 0, 2, MPI_COMM_WORLD);
  }

  MPI_Finalize();
	return 0;

}
