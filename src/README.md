## Box Model

### Sources

The main model code is found in `box_model.c`.

The `main.c` file contains a program which can be used for testing on the command line.

### Dependencies

The code depends on the `slsode` function from [ODEPACK](https://computing.llnl.gov/casc/odepack/).

This code has been converted to C using the `f2c` program and is available in the [slsode](slsode/) directory.

As `f2c` is used, the `f2c` library also need to be compiled to web assembly. There is a script in the [f2c](f2c/) directory which can be used to retrieve the source and make the required edits so that it can be compiled.

The code has been compiled / tested with clang / llvm version 11. [WASI Libc](https://github.com/WebAssembly/wasi-libc) is also required to be available in the location pointed to by the `WASI_LIBC` variable.

### Compilation

If all dependencies are available, the web assembly code can be compiled by runnin `make`.

To compile the test program, which can be run from the command line:

```
clang -o box_model main.c box_model.c slsode/slsode.c -lf2c -lm
```

The test program should also compile with `gcc`.
