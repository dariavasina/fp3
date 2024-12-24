let generate_points x_min x_max step =
  let rec aux current =
    if current > x_max then Seq.Nil
    else Seq.Cons (current, fun () -> aux (current +. step))
  in
  fun () -> aux x_min

let linear_interpolation points step =
  let interpolate_y x points =
    let rec find_interval = function
      | (x1, y1) :: (x2, y2) :: _ when x1 <= x && x <= x2 -> Some (x1, y1, x2, y2)
      | _ :: rest -> find_interval rest
      | [] -> None
    in
    match find_interval points with
    | Some (x1, y1, x2, y2) ->
        y1 +. (y2 -. y1) *. (x -. x1) /. (x2 -. x1)
    | None -> failwith "Не удалось интерполировать точку"
  in
  let x_min, _ = List.hd points in
  let x_max, _ = List.hd (List.rev points) in
  generate_points x_min x_max step
  |> Seq.map (fun x -> (x, interpolate_y x points))

let lagrange_interpolation points step =
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
  generate_points x_min x_max step
  |> Seq.map (fun x -> (x, lagrange_polynomial x points))
