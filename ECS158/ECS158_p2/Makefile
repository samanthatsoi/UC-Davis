CC := gcc
CFLAGS := -Wall -Werror
CFLAGS += -fopenmp

ifneq ($(V), 1)
Q = @
endif
ifneq ($(D), 1)
CFLAGS += -O2
else
CFLAGS += -O0
CFLAGS += -g
endif

all: mmm_omp heat_distribution_serial heat_distribution_omp

mmm_omp: mmm_omp.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $<

heat_distribution_serial: heat_distribution_serial.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $<

heat_distribution_omp: heat_distribution_omp.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $<

clean:
	@echo "CLEAN "
	$(Q)rm -f mmm_omp heat_distribution_serial heat_distribution_omp