open Interpolation
open Io

let print_interpolation_result points interpolation_type step =
  let result =
    match interpolation_type with
    | `Linear -> linear_interpolation points step
    | `Lagrange -> lagrange_interpolation points step
  in
  let result_list = List.of_seq result in  
  let x_values = List.map (fun (x, _) -> Printf.sprintf "%.2f" x) result_list in
  let y_values = List.map (fun (_, y) -> Printf.sprintf "%.2f" y) result_list in
  print_endline (String.concat "\t" x_values);
  print_endline (String.concat "\t" y_values)


let rec run_interpolation points interpolation_type step =
  print_interpolation_result points interpolation_type step;
  let new_point = read_point () in
  let updated_points = points @ [new_point] in
  let required_points = if interpolation_type = `Linear then 2 else 3 in
  let updated_points =
    if List.length updated_points > required_points then List.tl updated_points else updated_points
  in
  run_interpolation updated_points interpolation_type step
  
let () =
  let interpolation_type, step =
    let rec parse_args args current_step =
      match args with
      | [] -> failwith "Укажите аргумент -m с типом интерполяции (linear или lagrange)."
      | "-m" :: "linear" :: _ -> (`Linear, current_step)
      | "-m" :: "lagrange" :: _ -> (`Lagrange, current_step)
      | "-s" :: step_value :: rest -> parse_args rest (float_of_string step_value)
      | _ :: rest -> parse_args rest current_step
    in
    parse_args (Array.to_list Sys.argv |> List.tl) 1.0
  in
  let required_points = if interpolation_type = `Linear then 2 else 3 in
  (* Printf.printf "Введите начальные точки (минимум %d):\n" required_points; *)
  let rec read_initial_points n acc =
    if n = 0 then List.rev acc
    else read_initial_points (n - 1) (read_point () :: acc)
  in
  let initial_points = read_initial_points required_points [] in
  run_interpolation initial_points interpolation_type step
