# Production Haskell Demystified

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

  - [Prerequisites](#prerequisites)
  - [Run the Demo API](#run-the-demo-api)
  - [Development Tooling](#development-tooling)

<!-- markdown-toc end -->

## Prerequisites

Install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

From the project root, run:
    
    stack test --fast
    
...which will download and set up GHC, retrieve package dependencies, build the
project, and run the test suite (with `-O0` optimizations to speed things up).

## Run the Demo API

From the project root, run

    make build-fast && stack exec demo
    
...which will recompile the project and start the web server on port `8080`.

Check out the following routes to see the server in action:

- Swagger documentation: http://localhost:8080/docs/index.html
- Users: http://localhost:8080/v1/users
- Admins: http://localhost:8080/v1/users/admins
- Moderators: http://localhost:8080/v1/users/moderators
- Basic Users: http://localhost:8080/v1/users/basic

## Development Tooling

I've included a `Makefile` with this project that captures a lot of my common
development flows within a Haskell project.

Since some of these commands are dependent on 
[ghcid](https://github.com/ndmitchell/ghcid), so before running any of them
make sure its installed and available on your path with `stack install ghcid`.

To build the project quickly:

    make build-fast

To enter the project REPL: 

    make ghci

To run `ghcid`, which will recompile the project on changes and output any type
errors to the console:

    make ghcid

To run `ghcid` and have it rerun the test suite after the project successfully
type checks:

    make ghcid-test

To run the test suite on its own:

    make test
