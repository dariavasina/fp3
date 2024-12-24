open Interpolation
open Io

let print_interpolation_result points interpolation_types step =
  List.iter (fun interpolation_type ->
    let result =
      match interpolation_type with
      | `Linear -> linear_interpolation points step
      | `Lagrange -> lagrange_interpolation points step
    in
    let result_list = List.of_seq result in
    let x_values = List.map (fun (x, _) -> Printf.sprintf "%.2f" x) result_list in
    let y_values = List.map (fun (_, y) -> Printf.sprintf "%.2f" y) result_list in
    let header = 
      match interpolation_type with
      | `Linear -> "Линейная"
      | `Lagrange -> "Лагранж"
    in
    print_endline header;
    print_endline (String.concat "\t" x_values);
    print_endline (String.concat "\t" y_values);
  ) interpolation_types

let rec run_interpolation points interpolation_types step =
  print_interpolation_result points interpolation_types step;
  let new_point = read_point () in
  let updated_points = points @ [new_point] in
  let required_points = if List.mem `Linear interpolation_types then 2 else 3 in
  let updated_points =
    if List.length updated_points > required_points then List.tl updated_points else updated_points
  in
  run_interpolation updated_points interpolation_types step

let parse_args args current_step interpolation_types =
  let rec aux args current_step interpolation_types =
    match args with
    | [] -> interpolation_types, current_step
    | "-m" :: "linear" :: rest -> aux rest current_step (`Linear :: interpolation_types)
    | "-m" :: "lagrange" :: rest -> aux rest current_step (`Lagrange :: interpolation_types)
    | "-s" :: step_value :: rest -> aux rest (float_of_string step_value) interpolation_types
    | _ :: rest -> aux rest current_step interpolation_types
  in
  aux args current_step interpolation_types

let () =
  let interpolation_types, step =
    let args = Array.to_list Sys.argv |> List.tl in
    parse_args args 1.0 []
  in
  let required_points = if List.mem `Linear interpolation_types then 2 else 3 in
  let rec read_initial_points n acc =
    if n = 0 then List.rev acc
    else read_initial_points (n - 1) (read_point () :: acc)
  in
  let initial_points = read_initial_points required_points [] in
  run_interpolation initial_points interpolation_types step