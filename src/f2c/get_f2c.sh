#!/bin/bash

# directory for f2c extraction / building:
F2C_DIR='./src'
# f2c zip file:
F2C_ZIP='libf2c.zip'
# wasi sysroot:
SYSROOT='$(WASI_LIBC)'
# c flags:
CFLAGS='-O3 -fPIC --target=wasm32-wasi --sysroot=$(SYSROOT) -D_WASI_EMULATED_SIGNAL'

# get_file function:
function get_file() {
  URL=${1}
  OUTFILE=${2}
  if [ -z "${OUTFILE}" ] ; then
    OUTFILE=$(echo "${URL}" | awk -F '/' '{print $NF}')
  fi
  if [ ! -e "${OUTFILE}" ] ; then
    echo "downloading file : ${URL}"
    wget --no-cache -N -q -O ${OUTFILE} "${URL}"
  fi
}

# get source:
get_file "https://www.netlib.org/f2c/libf2c.zip" ${F2C_ZIP}

# make build directoy:
if [ -e "${F2C_DIR}" ] ; then
  rm -fr ./${F2C_DIR}
fi
mkdir -p ${F2C_DIR}

# extract:
unzip ${F2C_ZIP} -d ${F2C_DIR}

# change to source directory or give up:
cd ${F2C_DIR} || exit

# updates:
\cp makefile.u Makefile
sed -i "s|^CC = cc|SYSROOT = ${SYSROOT}\nCC = clang|g" Makefile
sed -i "s|^\(CFLAGS = \).*$|\1${CFLAGS}|g" Makefile
sed -i 's|\(ld -r -x -o \$\*\.xxx \$\*\.o\)|#\1|g' Makefile
sed -i 's|\(mv \$\*\.xxx \$\*\.o\)|#\1|g' Makefile
sed -i 's|ar r \(libf2c\.a \$\?\)|llvm-ar crs \1|g' Makefile
sed -i 's|\(-ranlib libf2c\.a\)|#\1|g' Makefile
sed -i 's|\(-ranlib\)|llvm\1|g' Makefile
sed -i 's|\($(CC)\) \(-c f77vers.c\)|\1 $(CFLAGS) \2|g' Makefile
sed -i 's|\($(CC)\) \(-c i77vers.c\)|\1 $(CFLAGS) \2|g' Makefile
sed -i 's|\(\./a\.out >arith\.h\)|#\1|g' Makefile
sed -i 's|\(rm -f a\.out arithchk\.o\)|#\1|g' Makefile
\cp open.c open.c.original
sed -i '/\(if (!(b->ufd = tmpfile()))\)/,+1d' open.c
\cp s_paus.c s_paus.c.original
sed -i '/^\t\tfprintf(stderr,$/,+2d' s_paus.c
sed -i '/pause();$/d' s_paus.c
\cp system_.c system_.c.original
sed -i '/rv = system(buff);$/d' system_.c
touch arith.h
