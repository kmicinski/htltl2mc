# Model checker for HyperLTL_2 

This is an implmentation of a model checker for the fragment of
HyperLTL that allows one quantifier alternation between forall (A) and
exists (E).  The implementation is automata based, and includes a
number of useful facilities for manipulating Büchi automata via OCaml
(this API is module based and includes facilities for chaining
together various automata constructions).

## Dependencies

You need an OCaml compiler.  An easy way to get one is to
install OPAM, available from:

  http://opam.ocamlpro.com/

Run this command to install some libraries that our model checker depends on:

  opam install xml-light ounit
  
Also install the GOAL toolkit, available at:

    http://goal.im.ntu.edu.tw/wiki/doku.php  
    
Install the Linux/Windows .zip distribution of GOAL, not the Mac .dmg 
distribution.

We tested these instructions with version 4.01.0 of the OCaml compiler, and
with with GOAL2, version 2013-07-11.
  
## Building

To build, run from the src/ directory: 

    make

## Running the model checker

To run the model checker on an input model and formula, run this from the src/
directory:

    ./main.native model_file formula path_to_goal

- model_file is the name of a file containing a Kripke structure.
  Some examples are provided in the test_models/ directory.
- formula_file is a string containing a HyperLTL formula.
  Some examples are provided in the test_formulas/ directory.
- path_to_goal is the path to the GOAL runtime as installed 
  on your system.
  
The ultimate output from GOAL will either be (true, null), indicating that
the model satisfies the formula, or (false, ctex), where ctex is a
counterexample showing that model does not satisfy the formula.
  
An invocation on one of the author's machines looks like this:

    $ ./main.native ../test_models/1.mod "A ^ 1 E ^ 1 G ( [T, ~ \"p\"] ^ \"LE\")" ~/Downloads/GOAL-20130123/goal
    Running the model checker with ../test_models/1.mod as the input file..
    Verifying property:
    A ^ 1 E ^ 1 G ( [T, ~ "p"] ^ "LE")
    Model name: ../test_models/1.mod
    Simplified formula: A E ~ (T U (T,p,) v ~ (LE))
    Parsing model...
    The maximally consistent sets are:
    
    ...

    The constructed automaton has 60 states.
    Now to check the formula, we check containment of these two automata
    Written to files...
    Calling GOAL's solver...
    Goal says...
    (true, null)

    Finished checking all properties.

