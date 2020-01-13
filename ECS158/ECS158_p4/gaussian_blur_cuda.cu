#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <math.h>


#define DIV_ROUND_UP(n, d)  (((n) + (d) - 1) / (d))
#define cuda_check(ret) _cuda_check((ret), __FILE__, __LINE__)
inline void _cuda_check(cudaError_t ret, const char *file, int line)
{
  if (ret != cudaSuccess) {
    fprintf(stderr, "CudaErr: %s (%s:%d)\n", cudaGetErrorString(ret), file, line);
    exit(1);
  }
}
void print_matrix(float* matrix, int order){
  for (int row=0; row<order; row++)
  {
      for(int columns=0; columns<order; columns++)
          {
           printf("%f ", matrix[row*order + columns]);
          }
      printf("\n");
   }
}
float Gaussian(int x, int y, float sigma){
  return (1.0 / (2.0 * M_PI * sigma * sigma)) * exp(-((x*x+y*y)/(2*sigma*sigma)));
}

__global__ void calculation_kernel(float *Gaussian_matrix, unsigned char* image_buffer, unsigned char*output_matrix, int order, int width, int height)
{
  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;
      float sum = 0;
      //traversing the Gaussian_matrix
      for(unsigned int k = 0; k < order; k++)
      {
        for(unsigned int z = 0; z < order; z++)
        {
          int locX = (col - (floor((float)order / 2))) + z;
          int locY = (row - (floor((float)order / 2))) + k;
          // Getting nearest pixel
          if(locX < 0)
          {
            locX = 0;
          }
          else if(locX >= width)
          {
            locX = width-1;
          }

          if(locY < 0)
          {
            locY = 0;
          }
          else if(locY >= height)
          {
            locY = height-1;
          }
          float temp =  Gaussian_matrix[k*order+z] *image_buffer[(locY * width  + locX)];
          sum += temp;
        }
      }
  output_matrix[row*width+col] = (unsigned char)sum;
}

// Send every Pixel to GPU for computation
void calculation(float *Gaussian_matrix_h, unsigned char* image_buffer_h, unsigned char * output_matrix_h, int order, int width, int height)
{
  float * Gaussian_matrix_d;
  unsigned char *image_buffer_d;
  unsigned char *output_matrix_d;
  int kernel_size = order * order * sizeof(float);
  int image_size = width * height * sizeof(unsigned char);

  cuda_check(cudaMalloc(&Gaussian_matrix_d, kernel_size));
  cuda_check(cudaMalloc(&image_buffer_d, image_size));
  cuda_check(cudaMalloc(&output_matrix_d, image_size));
  cuda_check(cudaMemcpy(Gaussian_matrix_d, Gaussian_matrix_h, kernel_size, cudaMemcpyHostToDevice));
  cuda_check(cudaMemcpy(image_buffer_d, image_buffer_h, image_size, cudaMemcpyHostToDevice));
  dim3 block_dim(32,32);
  dim3 grid_dim(DIV_ROUND_UP(width, block_dim.x), DIV_ROUND_UP(height, block_dim.y));
  calculation_kernel<<<grid_dim, block_dim>>>(Gaussian_matrix_d, image_buffer_d, output_matrix_d, order, width, height);
  cuda_check(cudaPeekAtLastError());      /* Catch configuration errors */
  cuda_check(cudaDeviceSynchronize());    /* Catch execution errors */
  cuda_check(cudaMemcpy(output_matrix_h, output_matrix_d, image_size, cudaMemcpyDeviceToHost));
  cuda_check(cudaFree(Gaussian_matrix_d));
  cuda_check(cudaFree(image_buffer_d));
  cuda_check(cudaFree(output_matrix_d));
}

int main(int argc, char *argv[]){
  struct timespec start, end;
  if(argc != 4) {
    fprintf(stderr, "Usage: ./gaussian_blur_cuda <input_file> <output_file> <sigma>");
    return 1;
  }
  float sigma = atof(argv[3]);
  FILE *fp;
  fp = fopen(argv[1], "rb");
  char buffer[4];
  unsigned  int width;
  unsigned int height;
  int cutoff;

  if (!fp) {
    fprintf(stderr, "Error: cannot open file %s", argv[1]);
    return 1;
  }
  if(fscanf(fp,"%s\n",buffer) != 1){
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }
  if(strcmp(buffer,"P5") != 0){
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }
  if( fscanf(fp, "%d %d\n", &width, &height) != 2){         //taking in two values
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }
  if(width < 256 || height < 256){
    fprintf(stderr, "Error: invalid PGM pixels");
    return 1;
  }
  if( fscanf(fp, "%d\n", &cutoff)!= 1){
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }
  if(cutoff != 255){
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }
  if(sigma == 0){
    fprintf(stderr, "Error: invalid sigma value");
    return 1;
  }
  unsigned  int order = ceil(6*sigma);
  if(order % 2 == 0){
    order++;
  }
  if(order > height ||order  > width){
     fprintf(stderr, "Error: sigma value too big for image size");
     return 1;
  }

  float *Gaussian_matrix;
  unsigned char *image_buffer;
  unsigned char *output_matrix;
  Gaussian_matrix = (float*)malloc(order * order * sizeof(float));
  image_buffer = (unsigned char*)malloc(width * height * sizeof(unsigned char));
  output_matrix = (unsigned char*)malloc(width * height * sizeof(unsigned char));
  for(unsigned int x = 0, i = 0; x <= 2*floor(order/2) && i < order; x++, i++){
    for(unsigned int y = 0, j = 0; y <= 2*floor(order/2) && j < order; y++, j++){
      Gaussian_matrix[i*order+j] = Gaussian(x-floor(order/2),y-floor(order/2),sigma);
    }
  }
  if(fread(image_buffer, sizeof(unsigned char), height * width, fp) != width*height){
    fprintf(stderr, "Error: invalid PGM pixels");
    return 1;
  }
  clock_gettime(CLOCK_MONOTONIC, &start);
  calculation(Gaussian_matrix, image_buffer, output_matrix, order, width, height);
  clock_gettime(CLOCK_MONOTONIC, &end);
  double diff = end.tv_sec - start.tv_sec + (end.tv_nsec - start.tv_nsec)/1000000000.0;
  printf("Running time: %f secs\n", diff);
  FILE* pgmimg;
  char filename[40];
  strcpy(filename, argv[2]);
  pgmimg = fopen( filename , "wb" );
  if(pgmimg == NULL){
    fprintf(stderr, "cannot open file to write");
    return 1;
  }
  fprintf(pgmimg, "P5\n");
  fprintf(pgmimg, "%d %d\n", width, height);
  fprintf(pgmimg, "%d\n", cutoff);
  fwrite(output_matrix, sizeof(unsigned char), height * width, pgmimg);
  fclose(pgmimg);
  fclose(fp);
  return 0;
}
