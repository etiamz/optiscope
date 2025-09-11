type 'a scott_list = { unlist : 'r. 'r -> ('a -> 'a scott_list -> 'r) -> 'r }

let scott_nil = { unlist = (fun n c -> n) }

let scott_cons h t = { unlist = (fun n c -> c h t) }

let scott_singleton x = scott_cons x scott_nil

let rec scott_insert y list =
  list.unlist (scott_singleton y) (fun z zs ->
      if y <= z then scott_cons y (scott_cons z zs)
      else scott_cons z (scott_insert y zs))

let rec scott_insertion_sort list =
  list.unlist scott_nil (fun x xs -> scott_insert x (scott_insertion_sort xs))

let rec scott_sum_list list = list.unlist 0 (fun x xs -> x + scott_sum_list xs)

let generate_list n =
  let rec go i acc = if i < n then go (i + 1) (scott_cons i acc) else acc in
  go 0 scott_nil

let () =
  print_int (scott_sum_list (scott_insertion_sort (generate_list 1000)));
  print_newline ()
