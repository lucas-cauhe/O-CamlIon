(* let to_string =
  format_of_string "%d" *)

(* type database = Nil | Node of (database * int * int option) list *)
open Bd_b_plus.Add

exception BadTree




module Db = struct 
  
  let empty = Nil
  let add t v = 
    match Bd_b_plus.Add.add t v with
    | (Some t', _) -> t'
    | (None, _) -> t

  let _delete _ tree = tree
  let _search _ _ = Some 1
  let rec draw t = 
    match t with
    | Nil
    | Node [(Nil, _, None)] -> Printf.printf "\n"
    | Node ((Nil, key, Some _) :: l) ->
      Printf.printf "Hoja %d " key;
      draw (Node l)
    | Node [(ch, _, None)] ->
        Printf.printf "---- Hijos de centinela ----\n";
        draw ch;
        Printf.printf "---- Fin hijos de centinela ----\n"
    | Node ((ch, key, None) :: l) ->
        Printf.printf "---- Hijos de %d ----\n" key;
        draw ch;
        Printf.printf "---- Fin hijos de %d ----\n" key;
        draw (Node l)
    | _ -> raise BadTree
end

exception StopMain

let process_query c v t =
  match c with
  | -1 -> raise StopMain
  | 0 -> Db.add t v
  | _ -> t

let () = 
  let tree = ref Db.empty in
  try
    while true do
      let tokenized = String.split_on_char ' ' (read_line ()) in
      let code = int_of_string (List.nth tokenized 0) in
      let key_or_value = int_of_string (List.nth tokenized 1) in
      tree := process_query code key_or_value !tree;
      Db.draw !tree
    done
  with StopMain -> print_endline "stopped execution";

