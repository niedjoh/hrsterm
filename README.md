# hrsterm - termination of beta-eta normal higher-order rewrite systems

The tool `hrsterm` is a prototype implementation of termination techniques for
higher-order rewrite systems (HRSs) a la [Nipkow](https://doi.org/10.1016/S0304-3975(97)00143-6),
i.e., higher-order  rewriting modulo beta/eta. The following termination
methods are currently supported:

* the normalized higher-order recursive path order (`-t nhorpo`) based on this 
  [paper](https://doi.org/10.1145/2699913)
* NHORPO with neutralization (`-t nhorpo_n`) as described in the same paper
* the normalized computability path order (`-t ncpo`) based on 
  [CPO](https://doi.org/10.2168/LMCS-11(4:3)2015) 
  which is theoretically based on my recent CADE submission

The tool outputs `YES` if the input system can be proven terminating with
the corresponding method, otherwise `MAYBE` is reported.
For a human-checkable output, use the flag `-v`.

Currently, the only supported input format is a fragment of
[TPTP THF0](https://tptp.org/UserDocs/TPTPLanguage/TPTPLanguage.shtml) 
where axioms / conjectures are of the form `∀...∀ l = r` where free variables
of the equation are locally universally quantified.
Type checking is performed automatically after parsing.

The tool makes heavy use of SMT solvers using the awesome new library 
[hasmtlib](https://github.com/bruderj15/Hasmtlib). 
Via the `-s` command-line argument, you can choose your favorite SMT solver to solve the
constraints computed by `hoco`. (Currently available if installed on your system: `cvc5`, `z3` and `yices2`.)

## Installation / Run

Use `cabal run hrsterm -- <ARGS>` or install it via `cabal build` or `cabal install`.

## Documentation

Run `cabal haddock` to generate the documentation including
some examples.

## Experiments

The experiments that come with my recent CADE submission can be reproduced by
running `./cade_experiments.sh` from the main directory. Note that if you
run this for the first time, cabal will fetch the necessary packages and
compile the application before it starts with the experiments. Time measurement
is built-in and therefore not affected by the compilation process.

## Testing

Run `cabal repl --with-compiler=doctest` for the tests in the documentation.
Note that for the time being, I did not create a test suite from the doctest as this
seems to require too much manual work regarding dependencies while
cabal can just deal with it out of the box.