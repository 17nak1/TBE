EXPORTED_FUNCTIONS = "_run_me"
EXTRA_FUNCTIONS = "ccall", "cwrap","getValue", "setValue"

all:
	emcc lsoda.c -o lsoda.js --pre-js pre.js \
		-s EXPORTED_FUNCTIONS='[$(EXPORTED_FUNCTIONS)]' \
		-s EXTRA_EXPORTED_RUNTIME_METHODS='[$(EXTRA_FUNCTIONS)]'\
		-s EXIT_RUNTIME=1
		