(* Advent of Code 2025 Problem 3 Solver.

   This can handle a flexible number of battery_count until overflow issues are hit.

   Only 4 bit registers are required to store each battery digit and
   calculations are complete within a single clock cycle. This is made possible
   by using a greedy approach in maintaining only the best batteries seen and popping
   the first inferior battery from left to right.
   
   Takes ASCII input through data_in. Only tallies and starts a new count at new line chars.
   Currently using 64 bit unsigned integer output. Certainly vulnerable to overflow. 
   
   This project was built on top of the range finder template at
   https://github.com/janestreet/hardcaml_template_project/tree/with-extensions *)

open! Core
open! Hardcaml
open! Signal

let in_bits = 8
let out_bits = 64
let counter_bits = 8 (* Only needs to count up to the number of battery_count *)
let digit_bits = 4 (* Half the register use of holding ASCII chars *)
let battery_count = 12 (* PARAMETER: change this to 2 for a part 1 solution! *)
let digit_vector_bits = digit_bits * battery_count

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
    {
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
    State_machine.create (module States) spec
  in
  let%hw_var counter = Variable.reg spec ~width:counter_bits in
  let%hw_var digit_vector = Variable.reg spec ~width:digit_vector_bits in
  let%hw_var running_sum = Variable.reg spec ~width:out_bits in
  let total_joltage = Variable.wire ~default:(zero out_bits) () in
  let total_joltage_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ counter <--. 0
                ; digit_vector <--. 0
                ; running_sum <--. 0
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ if_
                    (* Tally digit vector before clearing on new line input *)
                    (data_in ==: of_char '\n')
                    (* This fold converts the digit vector into a binary integer *)
                    [ running_sum
                      <-- List.fold2_exn
                            (split_lsb ~part_width:digit_bits digit_vector.value)
                            (List.init battery_count ~f:(pow 10))
                            ~init:running_sum.value
                            ~f:(fun acc x i ->
                                  acc +: (x *: of_unsigned_int ~width:(out_bits - digit_bits) i))

                      ; digit_vector <--. 0
                      ; counter <--. 0
                    ]
                      (* Reduce ASCII char input to 4 bit unsigned int *)
                    [ let data_in_digit = (data_in -: of_char '0').:[digit_bits - 1, 0] in
                      (* Copy of the digit vector with the new char pushed in from the right *)
                      let pushed_vector = 
                            sll digit_vector.value ~by:digit_bits
                            +: uextend ~width:digit_vector_bits data_in_digit in
                      if_
                        (* Populate the digit vector pushing from the right until it is full *)
                        (counter.value <: of_unsigned_int ~width:counter_bits battery_count)
                        [ digit_vector <-- pushed_vector
                        ; counter <-- counter.value +:. 1
                        ]
                        (* When the vector is full perform the greedy pop *)
                        [ proc
                          (* Calculate the bit above where we want to pop a digit *)
                          [ let pop_index
                              = List.fold2_exn
                                      (split_msb ~part_width:digit_bits digit_vector.value)
                                      (split_msb ~part_width:digit_bits pushed_vector)
                                      ~init:(zero (digit_vector_bits + 1))
                                      ~f:(fun acc x y
                                            -> sll ~by:digit_bits
                                                (acc 
                                                  +: (uextend 
                                                        ~width:(digit_vector_bits + 1)
                                                        ((x <: y) &: ((popcount acc) ==:. 0))))) in

                            (* Create bit masks in order to merge the two input vectors appropriately *)
                            let low_mask = uresize
                                             ~width:digit_vector_bits
                                             (pop_index
                                                -: uextend
                                                     ~width:(digit_vector_bits + 1)
                                                     (popcount pop_index &:. 1)) in
                            let high_mask = (ones digit_vector_bits) -: low_mask in
                            digit_vector <-- (digit_vector.value &: high_mask) +: (pushed_vector &: low_mask)
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
  { total_joltage = { value = total_joltage.value; valid = total_joltage_valid.value } }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"joltage_calculator" create
;;
