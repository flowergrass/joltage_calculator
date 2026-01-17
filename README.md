Advent of FPGA 2025 Day 3
===========================

Advent of Code 2025 Problem 3 Solver in Hardcaml.

This solution can calculate a "joltage" for a flexible number of batteries 
by simply changing the `battery_count` constant in `joltage_calculator.ml`.

Only 4 bits are required to store each battery digit in a register vector and 
calculations are complete within a single clock cycle. This is made possible 
by using a greedy algorithmic approach in storing only the best batteries 
seen and popping the first inferior battery from left to right.

Input is fed through as ASCII chars into `data_in`. The program tallies the total and starts a new 
"joltage" count at new line char inputs. Output is a 64 bit unsigned integer in `total_joltage`.
Large `battery_count` parameters will create integer overflow issues and fail to compile.

Run with:

```
opam switch 5.2.0+ox

eval $(opam env)

opam install -y hardcaml hardcaml_test_harness hardcaml_waveterm ppx_hardcaml

opam install -y core core_unix ppx_jane rope re dune

dune build bin/generate.exe @runtest
```

I was hoping to have multiple input streams since it is absolutely possible calculate
the solution for multiple lines in parallel but this will have to do for now.
Apologies for the sloppy code this is my first experience with rtl, ocaml and hardcaml!

This project was built on top of the range finder template at
https://github.com/janestreet/hardcaml_template_project/tree/with-extensions

...idk how this license stuff works so I'll just leave it all there. Probably should've forked?