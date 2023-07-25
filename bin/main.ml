open Bd_b_plus.Add
open Bigarray

exception BadTree

let iter_bigarr (f: (int -> unit) ) (arr: barr) = 
  for i = 0 to ((Array1.dim arr)-1) do
    f arr.{i}
  done


let arrays_differ_at_pos ar1 ar2 = 
  let len1 = Bigarray.Array1.dim ar1 in
  let len2 = Bigarray.Array1.dim ar2 in
  if len1 = 0 || len2 = 0 then
    1
  else
    let found = ref false in
    let mn = min len1 len2 in
    let pos = ref (-1) in
    let () = for i = 0 to mn-1 do
      if not (ar1.{i} = ar2.{i}) then found := true; pos := i
    done
    in
    if !found then !pos
    else mn

let rec update_leafs n t inserted_key = 
  match t with
  | (Node [(Nil, _, ind)]) as c when ind < n -> c
  | Node [(Nil, key, ind)] -> 
    if not (inserted_key = key) then
      Node [(Nil, key, ind+1)]
    else Node [(Nil, key, ind)]

  | Node ((Nil, _, ind) as h :: l) when ind < n -> 
    let updated = match update_leafs n (Node l) inserted_key with 
      | Node s -> s
      | _ -> raise BadTree
    in
    Node (h::updated)

  | Node ((Nil, key, ind) as h :: l) ->
    let updated = match update_leafs n (Node l) inserted_key with
      | Node s -> s
      | _ -> raise BadTree
    in
    if not (inserted_key = key) then
      Node ((Nil, key, ind+1)::updated)
    else Node (h::updated)
  
  | Node [(ch, k, i)] ->
    let ch_updated = update_leafs n ch inserted_key in
    Node [(ch_updated, k, i)]
    
  | Node ((ch, k, i) :: l) -> 
    let ch_updated = update_leafs n ch inserted_key in
    let rest_updated = match update_leafs n (Node l) inserted_key with 
      | Node s -> s
      | _ -> raise BadTree
    in
    Node ((ch_updated, k, i) :: rest_updated)
  
  | Nil -> t

  | _ -> t

module Db = struct 
  
  let empty = (Nil, Array1.create int c_layout 0)
    
  let add t v = 
    let key = get_pkey (Nil, v, 0) in
    let (prev_t, prev_arr) = t in
    match Bd_b_plus.Add.add t v with
    | ((Some t', _), arr) -> 
      (* update the leaf nodes after the added one *)
      let final_tree = update_leafs (arrays_differ_at_pos prev_arr arr) t' key in
      (final_tree, arr)
    | ((None, _), arr) -> (prev_t, arr)

  let _delete _ tree = tree
  (* min_key tiene que estar en t, de lo contrario devolverá None, aunque haya en el rango
     (min_key, max_key] *)
  let rec search (t, arr) (min_key, max_key) =
    let traverse arr from =
      let rec aux (l: int list) i =
        if i >= (Bigarray.Array1.dim arr) then 
          l
        else  
          let key = get_pkey (Nil, arr.{i}, 0) in
          if key > max_key then l
          else
            aux (l@[arr.{i}]) (i+1)
      in
      aux [] from
    in
    match t with
    | Nil
    | Node [(Nil, _, _)] -> None
    | Node ((Nil, key, ind) :: _) when min_key = key ->
      (* search through array from min_key to max_key *)
      Some (traverse arr ind)
    | Node ((Nil, _, _) :: l) -> search ((Node l), arr) (min_key, max_key)
    | Node [(ch, _, _)] -> search (ch, arr) (min_key, max_key)
    | Node ((ch, key, _) :: _) when min_key < key -> search (ch, arr) (min_key, max_key)
    | Node ((_, _, _) :: l) -> search ((Node l), arr) (min_key, max_key)
    | _ -> None
      
  let rec draw_tree t = 
    match t with
    | Nil
    | Node [(Nil, _, _)] -> Printf.printf "\n"
    | Node ((Nil, key, _) :: l) ->
      Printf.printf "Hoja %d " key;
      draw_tree (Node l)
    | Node [(ch, _, _)] ->
        Printf.printf "---- Hijos de centinela ----\n";
        draw_tree ch;
        Printf.printf "---- Fin hijos de centinela ----\n"
    | Node ((ch, key, _) :: l) ->
        Printf.printf "---- Hijos de %d ----\n" key;
        draw_tree ch;
        Printf.printf "---- Fin hijos de %d ----\n" key;
        draw_tree (Node l)
    | _ -> raise BadTree
  
  let draw_leafs a = 
    iter_bigarr (fun x -> Printf.printf "%d -> " x) a;
    print_endline ""
end

exception StopMain

let () = 
  let (tree, arr) = Db.empty in
  let (tree, arr) = (ref tree, ref arr) in
  try
    while true do
      let tokenized = String.split_on_char ' ' (read_line ()) in
      let code = int_of_string (List.nth tokenized 0) in
      if code = 1 then 
        let key_or_value = (
          int_of_string (List.nth tokenized 1),
          int_of_string (List.nth tokenized 2)
        ) in
        match Db.search (!tree, !arr) key_or_value with
        | Some l -> Printf.printf "["; List.iter (fun v -> Printf.printf "%d, " v) l; Printf.printf "]\n"
        | None -> Printf.printf "No se han encontrado valores con esas clave\n"
      else if code = 0 then
        let v = int_of_string (List.nth tokenized 1) in
        let (t, a) = Db.add (!tree, !arr) v in
        tree := t; arr := a;    
        Db.draw_tree !tree;
        Db.draw_leafs !arr
      else raise StopMain 
    done
  with StopMain -> print_endline "stopped execution";

