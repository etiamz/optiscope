let rec tak x y z =
  if y >= x then z
  else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

let () =
  print_int (tak 36 17 8);
  print_newline ()
