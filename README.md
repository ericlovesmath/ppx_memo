# ppx_memo

PPX rewriter to memoize functions in OCaml (in progress)

```ocaml
let[@memo] double x =
  Printf.printf "Calculating %d\n" x;
  x * 2

let%expect_test "Test" =
  let test n = print_endline (Int.to_string (double n)) in
  test 5;
  test 5;
  test 10;
  [%expect
    {|
    Calculating 5
    10
    10
    Calculating 10
    20
    |}]
```

## TODO

- Add `let%expect_test`'s to dump AST form directly
- Attempt to remove full `Core` dependency
- `[@memo]` only works on first argument, rest must be un/packaged into a tuple
- Implement `[@memo.recursive]` to use `Core.Memo.recursive`
