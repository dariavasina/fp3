open FP3.Runner
open FP3.Args

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let runner = parse_arguments args in
  initialize_runner runner