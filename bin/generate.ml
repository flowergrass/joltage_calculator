open! Core
open! Hardcaml
open! Hardcaml_demo_project

let generate_joltage_calculator_rtl () =
  let module C = Circuit.With_interface (Joltage_calculator.I) (Joltage_calculator.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"joltage_calculator_top" (Joltage_calculator.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let joltage_calculator_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_joltage_calculator_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "joltage-calculator", joltage_calculator_rtl_command ])
;;
