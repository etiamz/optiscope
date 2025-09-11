type 'a scott_list = { unlist : 'r. 'r -> ('a -> 'a scott_list -> 'r) -> 'r }

let scott_nil = { unlist = (fun n c -> n) }

let scott_cons h t = { unlist = (fun n c -> c h t) }

let rec scott_filter f list =
  list.unlist scott_nil (fun x xs ->
      if f x then scott_cons x (scott_filter f xs) else scott_filter f xs)

let rec scott_append xs ys =
  xs.unlist ys (fun x xss -> scott_cons x (scott_append xss ys))

let rec scott_quicksort list =
  list.unlist scott_nil (fun x xs ->
      scott_append
        (scott_quicksort (scott_filter (fun y -> y < x) xs))
        (scott_cons x (scott_quicksort (scott_filter (fun z -> z >= x) xs))))

let rec scott_sum_list list = list.unlist 0 (fun x xs -> x + scott_sum_list xs)

let generate_list n =
  let rec go i acc = if i < n then go (i + 1) (scott_cons i acc) else acc in
  go 0 scott_nil

let () =
  print_int (scott_sum_list (scott_quicksort (generate_list 1000)));
  print_newline ()
