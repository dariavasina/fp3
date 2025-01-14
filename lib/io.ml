open Printf

let print_points points =
  let x_values = List.map fst points in
  let y_values = List.map snd points in
  List.iter (printf "%.2f\t") x_values;
  print_newline ();
  List.iter (printf "%.2f\t") y_values;
  print_newline ()

let read_point () =
  print_endline "Введите точку (X Y через пробел):";
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [x; y] -> (float_of_string x, float_of_string y)
  | _ -> failwith "Некорректный ввод!"
