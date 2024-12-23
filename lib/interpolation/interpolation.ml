let linear_interpolation points ~step =
  let sorted_points = List.sort (fun (x1, _) (x2, _) -> Float.compare x1 x2) points in
  let rec generate_x_sequence x_min x_max step acc =
    if x_min > x_max then List.rev acc
    else generate_x_sequence (x_min +. step) x_max step (x_min :: acc)
  in
  let interpolate_y x points =
    let rec find_interval points =
      match points with
      | (x1, y1) :: (x2, y2) :: _ when x1 <= x && x <= x2 ->
          Some (x1, y1, x2, y2)
      | _ :: rest -> find_interval rest
      | [] -> None
    in
    match find_interval points with
    | Some (x1, y1, x2, y2) ->
        y1 +. (y2 -. y1) *. (x -. x1) /. (x2 -. x1)
    | None ->
        let (x1, y1), (x2, y2) =
          if x < fst (List.hd points) then (List.hd points, List.hd (List.tl sorted_points))
          else (List.hd (List.rev (List.tl (List.rev points)))), List.hd (List.rev points)
        in
        y1 +. (y2 -. y1) *. (x -. x1) /. (x2 -. x1)
  in
  let x_min, _ = List.hd sorted_points in
  let x_max, _ = List.hd (List.rev sorted_points) in
  let x_sequence = generate_x_sequence x_min (x_max +. step) step [] in
  List.map (fun x -> (x, interpolate_y x sorted_points)) x_sequence


let lagrange_interpolation points ~step =
  let rec generate_x_sequence x_min x_max step acc =
    if x_min > x_max then List.rev acc
    else generate_x_sequence (x_min +. step) x_max step (x_min :: acc)
  in
  let lagrange_polynomial x points =
    List.fold_left
      (fun acc (xi, yi) ->
        let li =
          List.fold_left
            (fun prod (xj, _) ->
              if Float.equal xi xj then prod
              else prod *. (x -. xj) /. (xi -. xj))
            1.0 points
        in
        acc +. yi *. li)
      0.0 points
  in
  let x_min, _ = List.hd points in
  let x_max, _ = List.hd (List.rev points) in
  let x_sequence = generate_x_sequence x_min (x_max +. step) step [] in
  List.map (fun x -> (x, lagrange_polynomial x points)) x_sequence
  