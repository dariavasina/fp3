open Alcotest
open FP3.Runner

let test_generate_x_values () =
  let x_min = 0. in
  let x_max = 2.5 in
  let step = 1. in
  let result = generate_x_values x_min x_max step |> List.of_seq in
  check (list (float 0.01)) "generate x values" [ 0.; 1.; 2. ] result

