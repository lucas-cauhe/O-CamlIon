open Bd_b_plus.Insert
open Bigarray

exception BadTree

(* --- DELETE OPERATIONS --- *)

(* val visit_leaves : 'a database -> 'a -> 'a list *)
(* Recorre todo el árbol <t> almacenando en una lista las claves de las hojas visitadas *)
(* exceptuando aquella que sea igual a <key_to_omit> *)
let visit_leaves t key_to_omit = 
  let rec aux t' l = 
    match t' with
    | Nil
    | Node []
    | Node [(Nil, _, _)] -> l
    | Node ((Nil, key, _) :: tl) -> 
      if key = key_to_omit then aux (Node tl) l
      else aux (Node tl) (l @ [key])
    | Node ((ch, _, _) :: tl) ->
      aux (Node tl) (aux ch l)
  in
  aux t []

(* val delete_ind : barr -> int -> barr *)
(* Elimina el elemento <ind>-ésimo del Bigarray <arr> *)
let delete_ind arr ind = 
  Array1.init int c_layout ((Array1.dim arr)-1) (fun x -> 
    if x >= ind then arr.{x+1}
    else arr.{x})

exception BuildTreeError

(* val build_tree_from_array : barr -> 'a list -> 'a database * barr *)
(* Construye un árbol B+ a partir de un array <ar> y una lista de claves <keys> *)
let build_tree_from_array ar keys = 
  let t = (Nil, Array1.create int c_layout 0) in
  let rec aux i tr = 
    if i = Array1.dim ar then tr
    else
      let result_b_plus = match insert tr ar.{i} (List.nth keys i) with
      | ((Some t, _), a) -> (t, a)
      | _ -> raise BuildTreeError 
      in
      aux (i+1) result_b_plus
  in
  aux 0 t

(* --- DRAW OPERATIONS --- *)

(* val iter_bigarr : (int -> unit) -> barr -> unit  *)
(* Implementación del método iter de Array o List para Bigarray *)
let iter_bigarr (f: (int -> unit) ) (arr: barr) = 
  for i = 0 to ((Array1.dim arr)-1) do
    f arr.{i}
  done

(* --- INSERT OPERATIONS --- *)

(* val arrays_differ_at_pos : barr -> barr -> ind *)
(* Devuelve el índice del primer elemento que difiere entre los Bigarray <ar1> y <ar2> *)
let arrays_differ_at_pos ar1 ar2 = 
  let len1 = Bigarray.Array1.dim ar1 in
  let len2 = Bigarray.Array1.dim ar2 in
  if len1 = 0 || len2 = 0 then
    1
  else
    let mn = min len1 len2 in
    let rec aux i = 
      if i = mn then mn
      else
        if not (ar1.{i} = ar2.{i}) then i
        else aux (i+1)
    in
    aux 0

(* val update_leaves : int -> 'a database -> 'a -> 'a database *)
(* Recorre todo el árbol <t> y actualiza el tercer campo de la tupla asociada al constructor Node del tipo 'a database *)
(* de tal manera que si el índice <n> es menor que el de una hoja dada y la clave de la hoja es distinta a <inserted_key> *)
(* el valor del índice de la hoja se incrementa en 1 *)
let rec update_leaves n t inserted_key = 
  match t with
  | (Node [(Nil, _, ind)]) as c when ind < n -> c
  | Node [(Nil, key, ind)] -> 
    if not (inserted_key = key) then
      Node [(Nil, key, ind+1)]
    else Node [(Nil, key, ind)]

  | Node ((Nil, _, ind) as h :: l) when ind < n -> 
    let updated = match update_leaves n (Node l) inserted_key with 
      | Node s -> s
      | _ -> raise BadTree
    in
    Node (h::updated)

  | Node ((Nil, key, ind) as h :: l) ->
    let updated = match update_leaves n (Node l) inserted_key with
      | Node s -> s
      | _ -> raise BadTree
    in
    if not (inserted_key = key) then
      Node ((Nil, key, ind+1)::updated)
    else Node (h::updated)
  
  | Node [(ch, k, i)] ->
    let ch_updated = update_leaves n ch inserted_key in
    Node [(ch_updated, k, i)]
    
  | Node ((ch, k, i) :: l) -> 
    let ch_updated = update_leaves n ch inserted_key in
    let rest_updated = match update_leaves n (Node l) inserted_key with 
      | Node s -> s
      | _ -> raise BadTree
    in
    Node ((ch_updated, k, i) :: rest_updated)
  
  | Nil -> t

  | _ -> t

module Db : Bd_b_plus.B_plus_sig.DB = struct 
  type 'a db = 'a database
  type leaf_nodes = barr

  let empty = (Nil, Array1.create int c_layout 0)
    
  let insert t v k = 
    let (prev_t, prev_arr) = t in
    match Bd_b_plus.Insert.insert t v k with
    | ((Some t', _), arr) -> 
      (* update the leaf nodes after the inserted one *)
      let final_tree = update_leaves (arrays_differ_at_pos prev_arr arr) t' k in
      (final_tree, arr)
    | ((None, _), arr) -> (prev_t, arr)

  let rec search (t, arr) k =
    match t with
    | Nil
    | Node [(Nil, _, _)] -> None
    | Node ((Nil, key, ind) :: _) when k = key ->
      Some (arr.{ind}, ind)
    | Node ((Nil, _, _) :: l) -> search ((Node l), arr) k
    | Node [(ch, _, _)] -> search (ch, arr) k
    | Node ((ch, key, _) :: _) when k < key -> search (ch, arr) k
    | Node ((_, _, _) :: l) -> search ((Node l), arr) k
    | _ -> None
        

  let delete (t, arr) key  = 
    match search (t, arr) key with
    | Some (_, ind) ->
      let result_array = delete_ind arr ind in
      if Array1.dim result_array = 0 then (Nil, Array1.create int c_layout 0)
      else
        let leaves = visit_leaves t key in
        build_tree_from_array result_array leaves
    | _ -> (t, arr)
    
  
  let rec draw_tree t printer = 
    match t with
    | Nil
    | Node [(Nil, _, _)] -> Printf.printf "\n"
    | Node ((Nil, key, _) :: l) ->
      Printf.printf "Hoja ";
      printer key;
      draw_tree (Node l) printer
    | Node [(ch, _, _)] ->
        Printf.printf "---- Hijos de centinela ----\n";
        draw_tree ch printer;
        Printf.printf "---- Fin hijos de centinela ----\n"
    | Node ((ch, key, _) :: l) ->
        Printf.printf "---- Hijos de "; printer key; print_endline "----";
        draw_tree ch printer;
        Printf.printf "---- Fin hijos de "; printer key; print_endline "----" ;
        draw_tree (Node l) printer
    | _ -> raise BadTree

  
  
  let draw_leaves a = 
    iter_bigarr (fun x -> Printf.printf "%d -> " x) a;
    print_endline ""
end

(* EJEMPLO DE USO 1 *)

exception StopMain

let () = 
  let (tree, arr) = Db.empty in
  let (tree, arr) = (ref tree, ref arr) in
  let printer k = Printf.printf "%d " k in
  try
    while true do
      let tokenized = String.split_on_char ' ' (read_line ()) in
      let code = int_of_string (List.nth tokenized 0) in
      if code = 0 then
        let v = int_of_string (List.nth tokenized 1) in
        let k = int_of_string (List.nth tokenized 2) in
        let (t, a) = Db.insert (!tree, !arr) v k in
        tree := t; arr := a;    
        Db.draw_tree !tree printer;
        Db.draw_leaves !arr

      else if code = 1 then 
        let key = int_of_string (List.nth tokenized 1) in
        match Db.search (!tree, !arr) key with
        | Some (v, _) -> Printf.printf "%d\n" v
        | None -> Printf.printf "No se han encontrado valores con esas clave\n"

      else if code = 2 then
        let v = int_of_string (List.nth tokenized 1) in
        let (t, a) = Db.delete (!tree, !arr) v in
        tree := t; arr := a;    
        Db.draw_tree !tree printer;
        Db.draw_leaves !arr
        
      else raise StopMain 
    done
  with StopMain -> print_endline "stopped execution";

  (* EJEMPLO DE USO 2 *)
(* let () = 
  let t = Db.empty in
  let t = Db.insert t 2 "hello" in
  let t = Db.insert t 3 "hellothree" in
  let (tree, arr) = t in
  let string_printer s = Printf.printf "\"%s\" " s in
  Db.draw_tree tree string_printer;
  Db.draw_leaves arr;
  let t = Db.delete t "hello" in
  let (tree, arr) = t in
  Db.draw_tree tree string_printer;
  Db.draw_leaves arr
;; *)

