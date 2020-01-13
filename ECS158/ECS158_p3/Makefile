CC := mpicc
CFLAGS := -Wall -Werror
CFLAGS += -O2

ifneq ($(V), 1)
Q = @
endif


all: mandelbrot_mpi mmm_mpi

mmm_mpi: mmm_mpi.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $<

mandelbrot_mpi: mandelbrot_mpi.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $< -lm

clean:
	@echo "CLEAN "
	$(Q)rm -f mmm_mpi mandelbrot_mpi