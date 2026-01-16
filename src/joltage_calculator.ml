(* Advent of Code 2025 Problem 3 Solver.
   This can handle a flexible number of batteries until overflow issues are hit.
   Only 4 bit registers are required to store each battery digit and
   calculations are complete within a single clock cycle.
   
   Takes ASCII input through data_in. Only tallies and starts a new count at new line chars.
   Currently using 64 bit unsigned integer output. Certainly vulnerable to overflow. *)

open! Core
open! Hardcaml
open! Signal

let in_bits = 8
let out_bits = 64
let counter_bits = 8
let digit_bits = 4
let batteries = 12
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

let rec pow x y =
  if y = 0 then 1 else x * pow x (y - 1)

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
                      <-- List.fold2_exn
                            (split_lsb ~part_width:digit_bits digit_vector.value)
                            (List.init batteries ~f:(pow 10))
                            ~init:running_sum.value
                            ~f:(fun acc x i -> acc +: (x *: of_unsigned_int ~width:(out_bits - digit_bits) i))
                      ; digit_vector <--. 0
                      ; counter <--. 0
                    ]
                    [ let data_in_digit = (data_in -: of_char '0').:[digit_bits - 1, 0] in
                      let pushed_vector = sll digit_vector.value ~by:digit_bits +: uextend ~width:vector_bits data_in_digit in
                      if_
                        (counter.value <: of_unsigned_int ~width:counter_bits batteries)
                        [ digit_vector <-- pushed_vector
                        ; counter <-- counter.value +:. 1
                        ]
                        [ proc
                          (* [ let low_mask 
                              = uresize
                                ~width:vector_bits
                                (
                                  (
                                    List.fold2_exn
                                      (split_msb ~part_width:digit_bits digit_vector.value)
                                      (split_msb ~part_width:digit_bits pushed_vector)
                                      ~init:(zero (vector_bits + 1))
                                      ~f:
                                      (
                                        fun acc x y ->
                                        (
                                          sll ~by:digit_bits
                                            (
                                              acc +: (uextend ~width:(vector_bits + 1) ((x <: y) &: ((popcount acc) ==:. 0)))
                                            )
                                        )
                                      )
                                  )
                                  -:. 1
                                )
                            in *)
                          [ let pop_index
                              = List.fold2_exn
                                      (split_msb ~part_width:digit_bits digit_vector.value)
                                      (split_msb ~part_width:digit_bits pushed_vector)
                                      ~init:(zero (vector_bits + 1))
                                      ~f:
                                      (
                                        fun acc x y ->
                                        (
                                          sll ~by:digit_bits
                                            (
                                              acc +: (uextend ~width:(vector_bits + 1) ((x <: y) &: ((popcount acc) ==:. 0)))
                                            )
                                        )
                                      )
                            in
                            let low_mask = uresize ~width:vector_bits (pop_index -: uextend ~width:(vector_bits + 1) (popcount pop_index &:. 1)) in
                            let high_mask = (ones vector_bits) -: low_mask in
                            digit_vector <-- (digit_vector.value &: high_mask) +: (pushed_vector &: low_mask)
                            (* total_joltage <-- uextend ~width:out_bits low_mask *)
                          ]
                          (* if_
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
                            ] *)
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
