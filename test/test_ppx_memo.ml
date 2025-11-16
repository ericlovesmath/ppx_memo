open Core

let[@memo] double x =
  Printf.printf "Calculating (double %d)\n" x;
  x * 2
;;

let[@memo] add x y =
  Printf.printf "Calculating (add %d %d)\n" x y;
  x + y
;;

let[@memo.rec Int.hashable] rec fib x =
  if x % 10 = 0 then Printf.printf "Calculating (fib %d)\n" x;
  if x < 2 then x else fib (x - 1) + fib (x - 2)
;;

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
;;

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
;;

let%expect_test "fib test" =
  print_endline (Int.to_string (fib 100));
  [%expect
    {|
    Calculating (fib 100)
    Calculating (fib 90)
    Calculating (fib 80)
    Calculating (fib 70)
    Calculating (fib 60)
    Calculating (fib 50)
    Calculating (fib 40)
    Calculating (fib 30)
    Calculating (fib 20)
    Calculating (fib 10)
    Calculating (fib 0)
    3736710778780434371
    |}]
;;
