open Interpolation
open Io

type runner = {
  step : float;
  points_stream : (float * float) Seq.t;
  interpolation_types : interpolation list;
}

let generate_x_values x_min x_max step =
  let rec aux current =
    if current >= x_max then Seq.Cons (current, fun () -> Seq.Nil)
    else Seq.Cons (current, fun () -> aux (current +. step))
  in
  fun () -> aux x_min

let take n lst =
  let rec aux n lst acc =
    match lst, n with
    | _, 0 -> List.rev acc
    | [], _ -> List.rev acc
    | x :: xs, _ -> aux (n - 1) xs (x :: acc)
  in
  aux n lst []

let perform_interpolation points interpolation_types step =
  List.iter (fun interpolation ->
    let relevant_points = take interpolation.window_size points in
    if List.length relevant_points >= interpolation.window_size then
      let result =
        generate_x_values (fst (List.hd relevant_points)) (fst (List.hd (List.rev relevant_points))) step
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

