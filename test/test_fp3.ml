open Alcotest
open Test_interpolation
open Test_runner

let () =
  let interpolation_tests = [
    test_case "Linear interpolation" `Quick test_linear_interpolation;
    test_case "Lagrange interpolation" `Quick test_lagrange_interpolation;
  ] in
  let runner_tests = [
    test_case "Generate x values" `Quick test_generate_x_values;
    test_case "Perform interpolation" `Quick test_perform_interpolation;
  ] in
  run "Interpolation and Runner Tests" [
    "Interpolation", interpolation_tests;
    "Runner", runner_tests;
  ]
