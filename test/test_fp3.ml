open Alcotest
open Test_interpolation
open Test_runner

let () =
  let interpolation_tests = [
    test_case "Linear interpolation" `Quick test_linear_interpolation;
    test_case "Lagrange interpolation" `Quick test_lagrange_interpolation;
    test_case "Generate x values" `Quick test_generate_x_values;
  ] in
  run "Interpolation Tests" [
    "Interpolation", interpolation_tests;
  ]
