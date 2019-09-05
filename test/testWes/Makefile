LOADLIBES += -lm
CFLAGS += -D_LSODA_MAIN

all:	lsoda lsoda-lib.js

lsoda-lib.js: lsoda.c
	emcc $^ -s EXPORTED_FUNCTIONS='["_lsoda"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -o $@

lsoda: lsoda.o

clean:
	rm -f lsoda lsoda.o lsoda-lib.js
