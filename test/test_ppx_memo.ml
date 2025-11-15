let[@memo] double x =
  Printf.printf "Calculating (double %d)\n" x;
  x * 2

let[@memo] add x y =
  Printf.printf "Calculating (add %d %d)\n" x y;
  x + y

let%expect_test "double test" =
  let test n = print_endline (Int.to_string (double n)) in
  test 5;
  test 5;
  test 10;
  test 10;
  test 10;
  [%expect
    {|
    Calculating (double 5)
    10
    10
    Calculating (double 10)
    20
    20
    20
    |}]

let%expect_test "add test" =
  let test x y = print_endline (Int.to_string (add x y)) in
  test 1 1;
  test 1 1;
  test 2 5;
  test 1 5;
  test 2 5;
  [%expect
    {|
    Calculating (add 1 1)
    2
    2
    Calculating (add 2 5)
    7
    Calculating (add 1 5)
    6
    7
    |}]
