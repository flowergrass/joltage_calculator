"Advent of FPGA 2025 Day 3"
===========================

Advent of Code 2025 Problem 3 Solver in Hardcaml.

This can handle a flexible number of batteries until overflow issues are hit.

Only 4 bit registers are required to store each battery digit and
calculations are complete within a single clock cycle. This is made possible
by using a greedy approach in maintaining only the best batteries seen and popping
the first inferior battery from left to right.

Takes ASCII input through data_in. Only tallies and starts a new count at new line char inputs.
Currently using 64 bit unsigned integer output. Certainly vulnerable to overflow. 

I was hoping to have multiple input streams since it is absolutely possible calculate
the solution for multiple lines in parallel but this will have to do for now.
Apologies for the sloppy code this is my first experience with rtl, ocaml and hardcaml!

This project was built on top of the range finder template at
https://github.com/janestreet/hardcaml_template_project/tree/with-extensions

...idk how this license stuff works so I'll just leave it all there