let rec ackermann = function
  | 0, n -> n + 1
  | m, 0 -> ackermann (m - 1, 1)
  | m, n -> ackermann (m - 1, ackermann (m, n - 1))

let () =
  print_int (ackermann (4, 1));
  print_newline ()
