type database = Nil | Node of (database * int * int option) list

(* let get_pkey (v: int) = v
let get_pkey (v: string) = v *)
let get_pkey (v: database * int * int option) = 
  let (_, key, _) = v in
  key

let (>>=) (m : database option * bool) (k : database * bool -> database option * bool) : database option * bool =
  let (datb, moved) = m in
  match datb with
  | None -> (None, moved)
  | Some x -> k (x, moved)

let rec take (l: 'a list) n =
  match l with
  | [] -> []
  | h :: t when n>0 -> h :: (take t (n-1))
  | _ -> []

let rec drop l n = 
  match l with
  | [] -> []
  | _ :: t when n>0 -> drop t (n-1)
  | l' -> l'

(* Splits a list into two halfs and returns the resulting lists as the children
 * of the root node which contains the pkey of the third element from the
 * original list *)
let divide l = 
  let third_pkey = get_pkey (List.nth l 2) in
  let (cent, taker) = match (List.nth l 2) with
  | (Nil, _, _) -> ((Nil, third_pkey, None), 2)
  | (node, _, _) -> ((node, third_pkey, None), 3) in
  let first_half = (take l 2) @ [cent] (*[l.(0), l.(1)] + centinela *) in
  let second_half = drop l taker (*[l.(3), l.(4)]*) in
  Node [
    (Node first_half, third_pkey, None);
    (Node second_half, third_pkey (* this key is never going to beread *), None)
  ]

let add t (v: int) = 
  let key = get_pkey (Nil, v, None) in
  let max_degree = 3 in
  let rec aux t' reg_length : database option * bool (* (árbol resultante,
  ha_cambiado el hijo *) = 
    match t' with
    | Nil (* arbol vacío *) -> (Some (Node [(Nil, key, Some v); (Nil, key, None)]), false)

    | Node ((_, k, _) :: _) when k=key (* clave primaria duplicada *) -> (None, false) 

    | Node ((Nil, k, Some _) as h :: l) when key < k (* Añadir nuevo objeto aquí *) ->
        if (List.length l) + reg_length = max_degree then
          if reg_length = 0 then (* dividir y coger indice 2 *)
            (Some (divide ([(Nil, key, Some v); h]@l)), true)
          else (Some (Node ([(Nil, key, Some v); h]@l)), true)
        else
          if reg_length = 0 then
            (Some (Node ([(Nil, key, Some v); h]@l)), false)
          else (Some (Node ([(Nil, key, Some v); h]@l)), false) 

    | Node ((Nil, _, Some _) as h :: l) (* Avanza en la lista l *) -> 
       aux (Node l) (reg_length + 1) >>= (fun (prt, moved) -> (* resulting tree *)
          let rt = match prt with 
          | Node s -> s
          | _ -> raise B_plus_sig.BadTree
        in
         if moved && reg_length = 0 then (* you are at the first element *)
          (*divide the current tree and take the key of the third element*)
          (Some (divide (h::rt) ), true)
         else if moved && not (reg_length = 0) then (Some (Node (h::rt)), true)
          else if not moved && reg_length = 0 then
            (Some (Node (h::rt)), false)
         else (Some (Node (h::rt)), false) 
        )
    | Node [(Nil, _, None) as lst] (* Centinela, añadir aquí y mover el Centinela *) -> 
        if reg_length = max_degree then (* Divides en dos y propagas *)
          (* El árbol devuelto tendrá como raíz la clave del tercer elemento e
           * hijos el registro acutal a la mitad *)

          (Some (Node [(Nil, key, Some v); lst]) , true)
        else (* Mueves el centinela *)
          (Some (Node [(Nil, key, Some v); lst]), false)
    
    (*-------------------------------*)
    (* Casos para los nodos no hojas *)
    (*-------------------------------*)

    | Node [(s, k, None)] ->
      aux s 0 >>= (fun (prt, moved) ->
        let rt = match prt with 
          | Node s -> Node s
          | _ -> raise B_plus_sig.BadTree
        in
        if moved then
            let (new_element, prev_el_new_tree) = begin
              match rt with 
              | Node [(Node _, _, None) as c'; (sh, _, None)] -> (c', sh)
              | _ -> raise B_plus_sig.BadTree
            end
            in
            let result_list = [new_element; (prev_el_new_tree, k, None)]
            in
            if reg_length = 0 then
              (Some (Node result_list), false)
            else
              (Some (Node result_list), reg_length+1 > 3)
        else
          if reg_length = 0 then
           (Some (Node [(rt, k, None)]), false)
          else 
            (Some (Node [(rt, k, None)]), false)
      ) 

    | Node ((_, k, None) as h :: l) when key > k -> (* Avanza en la lista *)
        aux (Node l) (reg_length+1) >>= (fun (prt, moved) ->
          let rt = match prt with 
          | Node s -> s
          | _ -> raise B_plus_sig.BadTree
        in
          if moved && reg_length = 0 then
            (Some (divide (h::rt)), true)
          else if moved && not (reg_length = 0) then
            (Some (Node (h::rt)), true)
          else if not moved && reg_length = 0 then
            (Some (Node (h::rt)), false)
          else
            (Some (Node (h::rt)), false)
        )
    | Node ((s, k, None) :: l) -> (* Investiga por debajo *)
(*the first to receive moved=true, means its child got moved, update
           * current register with resulting tree and then check if the
           * resulting register needs to get splitted eventually returning a
           * true moved value *)

        aux s 0 >>= (fun (prt, moved) ->
          let rt = match prt with 
          | Node s -> Node s
          | _ -> raise B_plus_sig.BadTree
        in
          if moved then
            let (new_element, prev_el_new_tree) = begin
              match rt with 
              | Node [(Node _, _, None) as c'; (sh, _, None)] -> (c', sh)
              | _ -> raise B_plus_sig.BadTree
            end
            in
            let result_list = [new_element; (prev_el_new_tree, k, None)]@l in
            if reg_length = 0 then
              if List.length result_list > 4 then
                (Some (divide result_list), true)
              else
                (Some (Node result_list), false) (*Aquí cortas la propagación del moved*)
            else
              let moving = reg_length + (List.length result_list) > 4 in
              (Some (Node result_list), moving)
          else 
            if reg_length = 0 then
            (Some (Node ([(rt, k, None)]@l)), false)
            else
            (Some (Node ([(rt, k, None)]@l)), false)
        )
    
    | _ (* Resto de casos por arbol mal construido *) -> raise B_plus_sig.BadTree 

  in
  aux t 0      



