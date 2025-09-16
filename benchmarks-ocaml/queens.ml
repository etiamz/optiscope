type 'a scott_list = { unlist : 'r. 'r -> ('a -> 'a scott_list -> 'r) -> 'r }

let scott_nil = { unlist = (fun n c -> n) }

let scott_cons h t = { unlist = (fun n c -> c h t) }

let rec scott_member x list =
  list.unlist false (fun y ys -> x = y || scott_member x ys)

let rec scott_length list = list.unlist 0 (fun x xs -> 1 + scott_length xs)

let rec scott_append xs ys =
  xs.unlist ys (fun x xss -> scott_cons x (scott_append xss ys))

let rec scott_threat k m list =
  list.unlist false (fun x xs ->
      k = x - m || k = m - x || scott_threat (k + 1) m xs)

let rec scott_queen_aux m b n =
  if m = 0 then scott_nil
  else if scott_member m b || scott_threat 1 m b then
    scott_queen_aux (m - 1) b n
  else if scott_length b = n - 1 then
    scott_append
      (scott_cons (scott_cons m b) scott_nil)
      (scott_queen_aux (m - 1) b n)
  else
    scott_append
      (scott_queen_aux n (scott_cons m b) n)
      (scott_queen_aux (m - 1) b n)

let scott_queen n = scott_queen_aux n scott_nil n

let () =
  print_int (scott_length (scott_queen 12));
  print_newline ()
