# WASI sysroot location:
SYSROOT = ${WASI_LIBC}

# Compiler options:
CC     = clang
CFLAGS = -Wall                 \
         -O3                   \
         -fPIC                 \
         --target=wasm32-wasi  \
         --sysroot=$(SYSROOT)  \
         -nostartfiles         \
         -Wl,--import-memory   \
         -Wl,--no-entry        \
         -Wl,--export-all      \
         -Wl,--allow-undefined \
         -Wl,--lto-O3          \
         -I./f2c/src
LIBS   = -lwasi-emulated-signal

# C source file:
WASM_SOURCE = box_model.c slsode/slsode.c f2c/src/*.o

# WASM output:
WASM_OUT = ../wasm/box_model.wasm

all: $(WASM_SOURCE)
	$(CC) $(CFLAGS) -o $(WASM_OUT) $(WASM_SOURCE) $(LIBS)
	chmod 644 $(WASM_OUT)

clean:
	\rm -f $(WASM_OUT)
