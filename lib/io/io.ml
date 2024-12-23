open Printf

let parse_point line =
  match String.split_on_char ';' line with
  | [x_str; y_str] -> (float_of_string x_str, float_of_string y_str)
  | _ -> failwith "Invalid input format. Expected 'X;Y'."

let read_points_from_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (parse_point line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

(* Форматированный вывод *)
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

