open Interpolation
open Io

type runner = {
  step : float;
  points_stream : (float * float) Seq.t;
  interpolation_types : interpolation list;
}

let generate_x_values x_min x_max step =
  let rec aux current =
    if current > x_max then Seq.Nil
    else Seq.Cons (current, fun () -> aux (current +. step))
  in
  fun () -> aux x_min

let perform_interpolation points interpolation_types step =
  List.iter (fun interpolation ->
    let result =
      generate_x_values (fst (List.hd points)) (fst (List.hd (List.rev points))) step
      |> Seq.map (fun x -> (x, interpolation.interpolate points x))
      |> List.of_seq
    in
    let x_values = List.map (fun (x, _) -> Printf.sprintf "%.2f" x) result in
    let y_values = List.map (fun (_, y) -> Printf.sprintf "%.2f" y) result in
    print_endline interpolation.name;
    print_endline (String.concat "\t" x_values);
    print_endline (String.concat "\t" y_values);
  ) interpolation_types

let rec update_runner runner =
  let points = List.of_seq runner.points_stream in

  perform_interpolation points runner.interpolation_types runner.step;
  let new_point = read_point () in
  let updated_points = Seq.append runner.points_stream (Seq.return new_point) in
  let required_points = List.fold_left (fun acc interpolation -> max acc interpolation.window_size) 0 runner.interpolation_types in
  let updated_points =
    if Seq.length updated_points > required_points then Seq.drop 1 updated_points else updated_points
  in
  update_runner { runner with points_stream = updated_points }

let initialize_runner runner =
  let required_points = List.fold_left (fun acc interpolation -> max acc interpolation.window_size) 0 runner.interpolation_types in
  let rec read_initial_points n acc =
    if n = 0 then acc
    else read_initial_points (n - 1) (Seq.append acc (Seq.return (read_point ())))
  in
  let initial_points = read_initial_points required_points Seq.empty in
  update_runner { runner with points_stream = initial_points }