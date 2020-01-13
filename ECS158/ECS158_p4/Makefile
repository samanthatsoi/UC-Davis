CC := gcc
NVCC := nvcc
CFLAGS := -Wall -Werror -O2
NFLAGS := -Xcompiler -Wall -Xcompiler -Werror -O2
LIBS= -lm

ifneq ($(V), 1)
Q = @
endif

all: gaussian_blur_serial gaussian_blur_cuda

gaussian_blur_serial: gaussian_blur_serial.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $< $(LIBS)

gaussian_blur_cuda: gaussian_blur_cuda.cu
	@echo "NVCC $@"
	$(Q)$(NVCC) $(NFLAGS) -o $@ $< $(LIBS)

clean:
	@echo "CLEAN "
	$(Q)rm -f gaussian_blur_serial gaussian_blur_cuda
