type 'a scott_list = { unlist : 'r. 'r -> ('a -> 'a scott_list -> 'r) -> 'r }

let scott_nil = { unlist = (fun n c -> n) }

let scott_cons h t = { unlist = (fun n c -> c h t) }

let rec scott_bubble_swap list n =
  if n = 0 then list
  else
    list.unlist list (fun x xs ->
        xs.unlist list (fun y ys ->
            if x < y then scott_cons x (scott_bubble_swap xs (n - 1))
            else scott_cons y (scott_bubble_swap (scott_cons x ys) (n - 1))))

let rec scott_bubble_go list n =
  if n = 0 then list else scott_bubble_go (scott_bubble_swap list n) (n - 1)

let rec scott_list_length list =
  list.unlist 0 (fun x xs -> 1 + scott_list_length xs)

let scott_bubble_sort list =
  list.unlist scott_nil (fun x xs ->
      scott_bubble_go list (scott_list_length list - 1))

let rec scott_sum_list list = list.unlist 0 (fun x xs -> x + scott_sum_list xs)

let generate_list n =
  let rec go i acc = if i < n then go (i + 1) (scott_cons i acc) else acc in
  go 0 scott_nil

let () =
  print_int (scott_sum_list (scott_bubble_sort (generate_list 1000)));
  print_newline ()
