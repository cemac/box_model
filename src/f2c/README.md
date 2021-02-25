## f2c

The `get_f2c.sh` script in this directory can be used to retrieve the libf2c source, and will then edit some files, so that it can be compiled to web assembly.

Once the code has been retrieved it can be compiled by running `make` in the `src/` directory.

This has only been tested with clang and llvm version 11.

The compilation requires the [WASI Libc](https://github.com/WebAssembly/wasi-libc) to be available in the location pointed to by the `WASI_LIBC` variable.
