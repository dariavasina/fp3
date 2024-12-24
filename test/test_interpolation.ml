open Alcotest
open Interpolation

let float_pair = pair (float 0.0001) (float 0.0001)

let test_linear_interpolation () =
  let points = [(0.0, 0.0); (1.0, 1.0)] in
  let step = 0.5 in
  let result = linear_interpolation points step |> List.of_seq in
  let expected = [(0.0, 0.0); (0.5, 0.5); (1.0, 1.0)] in
  check (list float_pair) "linear interpolation" expected result

let test_lagrange_interpolation () =
  let points = [(0.0, 0.0); (1.0, 2.0); (4.0, 8.0)] in
  let step = 1.0 in
  let result = lagrange_interpolation points step |> List.of_seq in
  let expected = [(0.0, 0.0); (1.0, 2.0); (2.0, 4.0); (3.0, 6.0); (4.0, 8.0)] in
  check (list float_pair) "lagrange interpolation" expected result

let () =
  let open Alcotest in
  run "Interpolation tests" [
    "linear_interpolation", [test_case "linear interpolation" `Quick test_linear_interpolation];
    "lagrange_interpolation", [test_case "lagrange interpolation" `Quick test_lagrange_interpolation];
  ]