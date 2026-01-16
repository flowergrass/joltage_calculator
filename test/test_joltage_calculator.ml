open! Core
open! Core_unix
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Joltage_calculator = Hardcaml_demo_project.Joltage_calculator
module Harness = Cyclesim_harness.Make (Joltage_calculator.I) (Joltage_calculator.O)

let ( <--. ) = Bits.( <--. )

let sample_input_values = In_channel.read_all (Core_unix.getcwd() ^ "/../../../../../test/input")

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n =
    inputs.data_in <--. int_of_char n;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  String.iter sample_input_values ~f:(fun x -> feed_input x);
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.total_joltage.valid)) do
    cycle ()
  done;
  let total_joltage = Bits.to_unsigned_int !(outputs.total_joltage.value) in
  print_s [%message "Result" (total_joltage : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
(* let waves_config = Waves_config.no_waves *)

let waves_config =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(* |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced
    ~waves_config
    ~create:Joltage_calculator.hierarchical
    simple_testbench;
  [%expect
    {|
    (Result (total_joltage 17554))
    Saved waves to /tmp/test_joltage_calculator_ml_Simple_test__optionally_saving_waveforms_to_disk.hardcamlwaveform
    |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  Harness.run_advanced
    ~create:Joltage_calculator.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    (Result (total_joltage 17554))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock                       ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear                       ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │                            ││────────────┬───────┬───────┬───────────────┬───────┬───────│
    │data_in                     ││ 00         │35     │33     │32             │35     │33     │
    │                            ││────────────┴───────┴───────┴───────────────┴───────┴───────│
    │data_in_valid               ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │finish                      ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │start                       ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │total_joltage$valid         ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │total_joltage$value         ││ 0000000000000000                                           │
    │                            ││────────────────────────────────────────────────────────────│
    │gnd                         ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────┬───────┬───────────────────────────────────│
    │joltage_calculator$counter  ││ 00             │01     │02                                 │
    │                            ││────────────────┴───────┴───────────────────────────────────│
    │                            ││────────────────┬───────┬───────────────────────┬───────────│
    │joltage_calculator$digit_vec││ 00             │05     │53                     │55         │
    │                            ││────────────────┴───────┴───────────────────────┴───────────│
    │joltage_calculator$i$clear  ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │joltage_calculator$i$clock  ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───────┬───────┬───────────────┬───────┬───────│
    │joltage_calculator$i$data_in││ 00         │35     │33     │32             │35     │33     │
    │                            ││────────────┴───────┴───────┴───────────────┴───────┴───────│
    │joltage_calculator$i$data_in││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │joltage_calculator$i$finish ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │joltage_calculator$i$start  ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │joltage_calculator$o$total_j││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │joltage_calculator$o$total_j││ 0000000000000000                                           │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │joltage_calculator$running_s││ 0000000000000000                                           │
    │                            ││────────────────────────────────────────────────────────────│
    │vdd                         ││────────────────────────────────────────────────────────────│
    │                            ││                                                            │
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
