(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal

let in_bits = 8
let out_bits = 64
let counter_bits = 8
let digit_bits = 4
let batteries = 2
let vector_bits = digit_bits * batteries

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits in_bits]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      total_joltage : 'a With_valid.t [@bits out_bits]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    (* Note that the state machine defaults to initializing to the first state *)
    State_machine.create (module States) spec
  in
  (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
     will show up in waveforms. The [_var] version is used when working with the Always
     DSL. *)
  let%hw_var counter = Variable.reg spec ~width:counter_bits in
  let%hw_var digit_vector = Variable.reg spec ~width:vector_bits in
  let%hw_var running_sum = Variable.reg spec ~width:out_bits in
  (* We don't need to name the range here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
  let total_joltage = Variable.wire ~default:(zero out_bits) () in
  let total_joltage_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_ start [ counter <--. 0; digit_vector <--. 0; running_sum <--. 0; sm.set_next Accepting_inputs ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ if_
                    (data_in ==: of_char '\n')
                    [ running_sum
                      <-- running_sum.value
                        +: uextend ~width:out_bits (digit_vector.value.:[7, 4] *: of_unsigned_int ~width:4 10)
                        +: uextend ~width:out_bits digit_vector.value.:[3, 0]
                    ; digit_vector <--. 0
                    ; counter <--. 0
                    ]
                    [ let data_in_digit = (data_in -: of_char '0').:[digit_bits - 1, 0] in
                      if_
                        (counter.value <: of_unsigned_int ~width:counter_bits batteries)
                        [ digit_vector
                          <-- sll digit_vector.value ~by:digit_bits +: uextend ~width:vector_bits data_in_digit
                        ; counter <-- counter.value +:. 1
                        ]
                        [ if_
                            (digit_vector.value.:[7, 4] <: digit_vector.value.:[3, 0])
                            [ digit_vector
                              <-- sll digit_vector.value ~by:digit_bits
                                  +: uextend ~width:vector_bits data_in_digit
                            ]
                            [ when_
                                (digit_vector.value.:[3, 0] <: data_in_digit)
                                [ digit_vector
                                  <-- digit_vector.value.:[7, 4] @: data_in_digit
                                ]
                            ]
                        ]
                    ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; ( Done
          , [ total_joltage <-- running_sum.value
            ; total_joltage_valid <-- vdd
            ; when_ finish [ sm.set_next Accepting_inputs ]
            ]
          )
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { total_joltage = { value = total_joltage.value; valid = total_joltage_valid.value } }
;;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"joltage_calculator" create
;;
