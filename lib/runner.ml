open Interpolation
open Io
open Utils

type runner = {
  step : float;
  points_stream : (float * float) Seq.t;
  interpolation_types : interpolation list;
  last_interpolated_x : float option;
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

let perform_interpolation points interpolation_types step last_x =
  let interpolate_one interpolation =
    let relevant_points = take interpolation.window_size points in
    if List.length relevant_points >= 2 then
      let end_point = List.hd (List.rev relevant_points) in
      let start_x = 
        match last_x with
        | None -> fst (List.hd relevant_points)
        | Some x -> x +. step
      in
      if start_x < fst end_point then
        let result =
          generate_x_values start_x (fst end_point) step
          |> Seq.map (fun x -> (x, interpolation.interpolate relevant_points x))
          |> List.of_seq
        in
        print_endline interpolation.name;
        print_points result;
        Some (fst (List.hd (List.rev result)))
      else
        Some start_x
    else
      last_x
  in
  List.fold_left (fun _ interp -> interpolate_one interp) last_x interpolation_types

let rec update_runner runner =
  let points = List.of_seq runner.points_stream in
  
  let last_interpolated = 
    if List.length points >= 2 then
      perform_interpolation points runner.interpolation_types runner.step runner.last_interpolated_x
    else
      None
  in
  
  let new_point = read_point () in
  let updated_points = Seq.append runner.points_stream (Seq.return new_point) in
  
  let required_points = List.fold_left (fun acc interpolation -> max acc interpolation.window_size) 0 runner.interpolation_types in
  let updated_points =
    if Seq.length updated_points > required_points then Seq.drop 1 updated_points else updated_points
  in
  
  update_runner { runner with 
    points_stream = updated_points;
    last_interpolated_x = last_interpolated
  }

let initialize_runner runner =
  let required_points = List.fold_left (fun acc interpolation -> max acc interpolation.window_size) 0 runner.interpolation_types in
  let rec read_initial_points n acc =
    if n = 0 then acc
    else read_initial_points (n - 1) (Seq.append acc (Seq.return (read_point ())))
  in
  let initial_points = read_initial_points required_points Seq.empty in
  update_runner { runner with 
    points_stream = initial_points;
    last_interpolated_x = None
  }

