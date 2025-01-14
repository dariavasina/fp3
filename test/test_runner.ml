open Alcotest
open FP3.Runner
open FP3.Interpolation

let test_generate_x_values () =
  let x_min = 0. in
  let x_max = 2. in
  let step = 1. in
  let result = generate_x_values x_min x_max step |> List.of_seq in
  check (list (float 0.01)) "generate x values" [ 0.; 1.; 2. ] result

let perform_interpolation_with_custom_print points interpolation_types step custom_print =
  List.iter (fun interpolation ->
    let result =
      generate_x_values (fst (List.hd points)) (fst (List.hd (List.rev points))) step
      |> Seq.map (fun x -> (x, interpolation.interpolate points x))
      |> List.of_seq
    in
    let x_values = List.map (fun (x, _) -> Printf.sprintf "%.2f" x) result in
    let y_values = List.map (fun (_, y) -> Printf.sprintf "%.2f" y) result in
    custom_print interpolation.name;
    custom_print (String.concat "\t" x_values);
    custom_print (String.concat "\t" y_values);
  ) interpolation_types

let test_perform_interpolation () =
  let points = [ (0., 0.); (2., 4.) ] in
  let interpolation_types = [ linear_interpolation ] in
  let step = 1. in
  let output = ref [] in
  let mock_print str = output := str :: !output in
  perform_interpolation_with_custom_print points interpolation_types step mock_print;
  let expected_output = [
    "Linear";
    "0.00\t1.00\t2.00";
    "0.00\t2.00\t4.00";
  ] in
  check (list string) "perform interpolation output" expected_output (List.rev !output)
