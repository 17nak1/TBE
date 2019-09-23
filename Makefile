# CFLAGS += -D_LSODA_MAIN -Wall
# LOADLIBES += -lm
subplex: subplex.o

clean:
	$(RM) subplex subplex.o
