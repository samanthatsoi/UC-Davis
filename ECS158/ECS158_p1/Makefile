ifneq ($(V), 1)
Q = @
endif

CC := gcc
CFLAGS := -O2 -Wall -Werror
CFLAGS += -g

mmm: mmm.c
	@echo "CC $@"
	$(Q)$(CC) $(CFLAGS) -o $@ $< -lpthread

clean:
	@echo "CLEAN"
	$(Q)rm -f mmm