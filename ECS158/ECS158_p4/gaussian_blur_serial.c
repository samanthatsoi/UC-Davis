#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <math.h>


float Gaussian(int x, int y, float sigma){
  return (1.0 / (2.0 * M_PI * sigma * sigma)) * exp(-((x*x+y*y)/(2*sigma*sigma)));
}

int main(int argc, char *argv[]){
  struct timespec start, end;
  if(argc != 4) {
      fprintf(stderr, "Usage: ./gaussian_blur_serial <input_file> <output_file> <sigma>");
      return 1;
  }

  float sigma = atof(argv[3]);
  FILE *fp;
  fp = fopen(argv[1], "rb");
  char buffer[4];
  int width, height;
  int cutoff;

  if (!fp) {
    fprintf(stderr, "Error: cannot open file %s", argv[1]);
    return 1;
  }

  if(fscanf(fp,"%s\n",buffer) != 1)
  {
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }

  if(strcmp(buffer,"P5") != 0){
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }

  if(fscanf(fp, "%d %d\n", &width, &height) != 2)   //taking in two values
  {
    fprintf(stderr, "Error: invalid PGM information");
    return 1;
  }

  if(width < 256 || height < 256){
    fprintf(stderr, "Error: invalid PGM pixels");
    return 1;
  }

  if(fscanf(fp, "%d\n", &cutoff)!= 1){
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
  int order = ceil(6*sigma);
  if(order % 2 == 0){
    order++;
  }
  if(order > height ||order  > width)
  {
     fprintf(stderr, "Error: sigma value too big for image size");
     return 1;
  }


// Initialize kernel matrix
  float* Gaussian_matrix;
  Gaussian_matrix = (float *)malloc(order * order * sizeof (float));
  for(int x = 0 - floor(order/2), i = 0; x <= floor(order/2) && i < order; x++, i++){
    for(int y = 0 - floor(order/2), j = 0; y <= floor(order/2) && j < order; y++, j++){
      Gaussian_matrix[i*order+j] = Gaussian(x,y,sigma);
    }
  }

// Read in image_buffer
  unsigned char* image_buffer;
  image_buffer = (unsigned char *)malloc(width * height * sizeof(unsigned char));
  if(fread(image_buffer, sizeof(unsigned char), height * width, fp) != width*height)
  {
    fprintf(stderr, "Error: invalid PGM pixels");
    return 1;
  }

  unsigned char* output_matrix;
  output_matrix = (unsigned char *)malloc(width * height * sizeof(unsigned char));
  clock_gettime(CLOCK_MONOTONIC, &start);
  for(int i = 0; i < height; i++){      //height and width of the output_matrix
    for(int j = 0; j < width; j++){
      float sum = 0;
      //traversing the Gaussian_matrix
      for(unsigned int k = 0; k < order; k++){
        for(unsigned int z = 0; z < order; z++){
          int locX = (j - (floor(order / 2))) + z;
          int locY = (i - (floor(order / 2))) + k;
          //Getting nearest pixel
          if(locX < 0){
            locX = 0;
          }
          else if(locX >= width){
            locX = width-1;
          }

          if(locY < 0){
            locY = 0;
          }
          else if(locY >= height){
            locY = height-1;
          }
        	float temp =  Gaussian_matrix[k*order+z] *image_buffer[(locY * width  + locX)];
        	sum += temp;
        }
      }
      output_matrix[i*width+j] = (unsigned char)sum;
    }
  }
  clock_gettime(CLOCK_MONOTONIC, &end);
  double diff = end.tv_sec - start.tv_sec + (end.tv_nsec - start.tv_nsec)/1000000000.0;
  printf("Running time: %f secs\n", diff);
  FILE* pgmimg;
  char filename[50];
  strcpy(filename, argv[2]);
  printf("%s",filename);
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
