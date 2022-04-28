include Stdlib.List
module List = Stdlib.List

(* List.fold_left2 which stops when end of one of the lists is reached *)
let rec fold_left2_opt f acc l1 l2 =
  match (l1, l2) with
  | ([], _) -> acc
  | (_, []) -> acc
  | (a1 :: l1, a2 :: l2) -> fold_left2_opt f (f acc a1 a2) l1 l2

(* List.fold_left for 3 lists of same size *)
let rec fold_left3 f acc l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> acc
  | (a1 :: l1, a2 :: l2, a3 :: l3) -> fold_left3 f (f acc a1 a2 a3) l1 l2 l3
  | _ ->
      raise (Invalid_argument "fold_left3 : lists don’t have the same size.")

(* List.fold_left for 4 lists of same size *)
let rec fold_left4 f acc l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
  | ([], [], [], []) -> acc
  | (a1 :: l1, a2 :: l2, a3 :: l3, a4 :: l4) ->
      fold_left4 f (f acc a1 a2 a3 a4) l1 l2 l3 l4
  | _ ->
      raise (Invalid_argument "fold_left4 : lists don’t have the same size.")

(* List.fold_left for 5 lists of same size *)
let rec fold_left5 f acc l1 l2 l3 l4 l5 =
  match (l1, l2, l3, l4, l5) with
  | ([], [], [], [], []) -> acc
  | (a1 :: l1, a2 :: l2, a3 :: l3, a4 :: l4, a5 :: l5) ->
      fold_left5 f (f acc a1 a2 a3 a4 a5) l1 l2 l3 l4 l5
  | _ ->
      raise (Invalid_argument "fold_left5 : lists don’t have the same size.")

(* List.fold_left for 6 lists of same size *)
let rec fold_left6 f acc l1 l2 l3 l4 l5 l6 =
  match (l1, l2, l3, l4, l5, l6) with
  | ([], [], [], [], [], []) -> acc
  | (a1 :: l1, a2 :: l2, a3 :: l3, a4 :: l4, a5 :: l5, a6 :: l6) ->
      fold_left6 f (f acc a1 a2 a3 a4 a5 a6) l1 l2 l3 l4 l5 l6
  | _ ->
      raise (Invalid_argument "fold_left6 : lists don’t have the same size.")

let split_n n l =
  let rec aux acc k l =
    if k = n then (List.rev acc, l)
    else
      match l with
      | h :: t -> aux (h :: acc) (k + 1) t
      | [] ->
          raise
            (Invalid_argument
               (Printf.sprintf "split_n: n=%d >= List.length l=%d" n k))
  in
  aux [] 0 l

let split_in_half l =
  let len = List.length l in
  match len mod 2 with
  | 0 -> split_n (len / 2) l
  | _ ->
      raise
        (Invalid_argument
           (Printf.sprintf "split_in_half: length %d not even." len))

let map f l = rev (rev_map f l)

let map2 f l1 l2 = rev (rev_map2 f l1 l2)

let rev_mapi f l =
  let rec rmap_f i accu = function
    | [] -> accu
    | a :: l -> rmap_f (i + 1) (f i a :: accu) l
  in
  rmap_f 0 [] l

let mapi f l = rev (rev_mapi f l)

(* not tail-recursive *)
let rec map2_opt merge l1 l2 =
  match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (h1 :: t1, h2 :: t2) -> merge h1 h2 :: map2_opt merge t1 t2

(* same as List.combine but allows lists of different sizes *)
let safe_combine l1 l2 =
  let rec aux acc l1 l2 =
    match (l1, l2) with
    | ([], _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (h1 :: t1, h2 :: t2) -> aux ((h1, h2) :: acc) t1 t2
  in
  aux [] l1 l2

exception Internal

(* Separate the heads and the tails of a list of list.
   raises if one of the lists is empty *)
let get_heads_and_tails ll =
  let rec aux heads tails = function
    | [] -> (List.rev heads, List.rev tails)
    | (h :: t) :: ll -> aux (h :: heads) (t :: tails) ll
    | _ -> raise (Invalid_argument "get_firsts_and_tails: one list is empty")
  in
  aux [] [] ll

(* fold_left for n lists of same size grouped in ln *)
(* the use of get_firsts_and_tails ensures all lists in ln have the same size *)
let fold_leftn f acc = function
  | [] -> acc
  | ln ->
      let rec aux acc ln =
        match List.hd ln with
        | [] -> acc
        | _ ->
            let (heads, tails) = get_heads_and_tails ln in
            let acc = f acc heads in
            aux acc tails
      in
      aux acc ln

(* equivalent to List.rev (List.flatten l) but tail recursive*)
let rev_flatten l =
  let rec aux res l =
    match l with [] -> res | h :: t -> aux (List.rev_append h res) t
  in
  aux [] l

let flatten l = List.rev @@ rev_flatten l

(* Pad list to given size with the last element of the list *)
let pad list ~size ~final_size =
  let array = Array.of_list list in
  let last_element = array.(size - 1) in
  let padding = Array.make (final_size - size) last_element in
  Array.(to_list (append array padding))

(* Resize list: return the list, sublist or pad it with its last element *)
let resize list ~size ~final_size =
  if size = final_size then list
  else if final_size - size < 0 then
    Array.(to_list (sub (of_list list) 0 final_size))
  else pad list ~final_size ~size

let rec equal_lists ~equal l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x :: xs, y :: ys) -> equal x y && equal_lists ~equal xs ys
  | _ -> false
