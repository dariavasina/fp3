open Interpolation
open Io
open Utils

type runner = {
  step : float;
  points_stream : (float * float) Seq.t;
  interpolation_types : interpolation list;
}

let generate_x_values start_x end_x step =
  let rec aux current =
    if current >= end_x then Seq.Nil
    else 
      let next = current +. step in
      if next > end_x then Seq.Cons (current, fun () -> Seq.Nil)
      else Seq.Cons (current, fun () -> aux next)
  in
  fun () -> aux start_x

let perform_interpolation points interpolation_types step =
  List.iter (fun interpolation ->
    let relevant_points = take interpolation.window_size points in
    if List.length relevant_points >= interpolation.window_size then
      let start_point = List.hd relevant_points in
      let end_point = 
        if List.length relevant_points >= 2 then
          List.nth relevant_points 1
        else
          List.hd relevant_points
      in
      let result =
        generate_x_values (fst start_point) (fst end_point) step
        |> Seq.map (fun x -> (x, interpolation.interpolate relevant_points x))
        |> List.of_seq
      in
      print_endline interpolation.name;
      print_points result
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

