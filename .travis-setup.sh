#!/bin/sh

set -eux

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_stack_osx() {
  curl -skL https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin;
}

fetch_stack_linux() {
  curl -sL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
}

fetch_blas_osx() {
  curl -skL http://www.netlib.org/blas/blas-3.7.0.tgz | tar xz
}

fetch_blas_linux() {
  curl -sL http://www.netlib.org/blas/blas-3.7.0.tgz | tar xz
}

# We need stack to generate cabal files with precise bounds, even for cabal
# builds.
mkdir -p ~/.local/bin;
if [ `uname` = "Darwin" ]; then
  travis_retry fetch_stack_osx
  travis_retry fetch_blas_osx
  # INSTALL FORTRAN
  wget http://coudert.name/software/gfortran-6.1-ElCapitan.dmg
  hdiutil mount gfortran-6.1-ElCapitan.dmg
  sudo installer -package /Volumes/gfortran-6.1-ElCapitan/gfortran-6.1-ElCapitan/gfortran.pkg -target "/Volumes/Macintosh HD"
else
  travis_retry fetch_stack_linux
  travis_retry fetch_blas_linux
fi

travis_retry stack --no-terminal setup;
# build the blas library and install it so that the haskell code can statically
# link it later
cd BLAS-3.7.0
gfortran -c -O3 *.f               # Compile, but don't link
if [ `uname` = "Darwin" ]; then
    /Library/Developer/CommandLineTools/usr/bin/ar rcs libblas.a *.o
    sudo cp libblas.a /usr/local/lib/
else
    ar rcs libblas.a *.o      # Create a static library from the object files
    sudo cp libblas.a /usr/local/lib/  # Install the library for use.
fi
