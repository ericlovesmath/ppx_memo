let[@memo] double x =
  Printf.printf "Calculating %d\n" x;
  x * 2

let%expect_test "Test" =
  let test n = print_endline (Int.to_string (double n)) in
  test 5;
  test 5;
  test 10;
  test 10;
  test 10;
  [%expect
    {|
    Calculating 5
    10
    10
    Calculating 10
    20
    20
    20
    |}]
