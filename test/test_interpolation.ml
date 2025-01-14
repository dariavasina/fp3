open Alcotest
open FP3.Interpolation

let test_linear_interpolation () =
  let points = [ (0., 0.); (2., 4.) ] in
  let x = 1. in
  let y = linear_interpolate points x in
  check (float 0.01) "linear interpolation" 2. y

let test_lagrange_interpolation () =
  let points = [ (0., 0.); (1., 1.); (2., 4.) ] in
  let x = 1.5 in
  let y = lagrange_interpolate points x in
  check (float 0.01) "lagrange interpolation" 2.25 y
