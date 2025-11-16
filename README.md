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
- Implement `[@memo.rec]` to use `Core.Memo.recursive`
- Allow `let[@memo] f (x : int) = ...` typing syntax
- Allow `function` form
- Derive the `hashable` function if arguments are annotated with a type

    - For example, `let[@memo.rec] rec f (x : int)` derives the type

    ```ocaml
    module M = Hashable.Make (struct
      type t = int [@@deriving hash, compare, sexp]
    end)

    let[@memo.rec M.hashable] rec fib (x : int) = ...
    ;;
    ```
