# ![](logo.png) λ-BLAS

## Description
The λ-BLAS (lambda-BLAS) project is an attempt to implement the Basic
Linear Algebra Subroutines (BLAS) library in Haskell.   This work differs
from previous attempts by providing native Haskell implementations of the
linear algebra subroutines rather than utilizing the foreign function interface to call a C library.  As a result the exposed linear algebra functions exposed strongly and statically typed, deterministic, thread safe and more likely to benefit from compiler optimization (e.g. stream fusion )

## Getting Started
### Prerequisites
This package uses the git source control system and the stack build system.  For information on how to install these tools for your platform, please consult the following url:

[Haskell Stack Documentation](https://docs.haskellstack.org/en/stable/README/)

[GIT installation](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)

You will also need a git-hub account in order to access the source
code repository.

### Installing
Run the following commands to download and install the HEAD version.

```bash
$ git clone git@github.com:dlewissandy/lambda-blas.git
$ cd lambda-blas
$ stack build
```
### Running the tests
You can run the unit tests and benchmarks from the command line using the following commands:
```bash
$ stack test
$ stack bench
```

Both the unit-tests and benchmarks can be invoked with options that fine tune the test behavior.   For a complete list of testing options, you can invoke the following:
```bash
$ stack test --test-arguments "--help"
$ stack test --benchmark-arguments "--help"
```

For advanced features of the benchmark tool, please consult the [criterion](http://www.serpentine.com/criterion/) web site for more information.

# Example Usage
Consider the following simple example to compute the dot product of
two vectors:
```haskell
import Numerical.BLAS.Types.Vector
import Numerical.BLAS.Single

-- Construct two vectors and compute their dot product
example :: IO ()
example = do
    let u = fromList [1..3] :: Vector Float
        v = fromList [4..6] :: Vector Float
    print $ sdot 3 u 1 v 1
```

# Contributing
Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.
