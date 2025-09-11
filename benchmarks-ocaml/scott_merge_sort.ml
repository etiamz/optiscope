type 'a scott_list = { unlist : 'r. 'r -> ('a -> 'a scott_list -> 'r) -> 'r }

let scott_nil = { unlist = (fun n c -> n) }

let scott_cons h t = { unlist = (fun n c -> c h t) }

let scott_singleton x = scott_cons x scott_nil

let rec scott_split list k =
  list.unlist (k scott_nil scott_nil) (fun x xs ->
      xs.unlist
        (k (scott_singleton x) scott_nil)
        (fun y ys ->
          scott_split ys (fun left right ->
              k (scott_cons x left) (scott_cons y right))))

let rec scott_merge xs ys =
  ys.unlist xs (fun y yss ->
      xs.unlist ys (fun x xss ->
          if x < y then scott_cons x (scott_merge xss ys)
          else scott_cons y (scott_merge xs yss)))

let rec scott_merge_sort list =
  list.unlist scott_nil (fun x xs ->
      xs.unlist (scott_singleton x) (fun dummy dummyx ->
          scott_split list (fun left right ->
              scott_merge (scott_merge_sort left) (scott_merge_sort right))))

let rec scott_sum_list list = list.unlist 0 (fun x xs -> x + scott_sum_list xs)

let generate_list n =
  let rec go i acc = if i < n then go (i + 1) (scott_cons i acc) else acc in
  go 0 scott_nil

let () =
  print_int (scott_sum_list (scott_merge_sort (generate_list 100)));
  print_newline ()
