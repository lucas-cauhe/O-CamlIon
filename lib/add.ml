(* tuple contains: (recursive children, key, possible value, number of leafs to the left) *)
type database = Nil | Node of (database * int * int option * int) list

(* you really dont care about the 4th element of the Node list tuple in non leafs nodes
   since only the leafs have useful information there *)

let get_pkey (v: database * int * int option * int) = 
  let (_, key, _, _) = v in
  key

let (>>=) (m : (database option * bool) * int array) (k : (database * bool) * int array -> (database option * bool) * int array) : (database option * bool) * int array =
  let ((datb, moved), res_llist) = m in
  match datb with
  | None -> ((None, moved), res_llist)
  | Some x -> k ((x, moved), res_llist)

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
  | (Nil, _, _, _) -> ((Nil, third_pkey, None, 0), 2)
  | (node, _, _, _) -> ((node, third_pkey, None, 0), 3) in
  let first_half = (take l 2) @ [cent] (*[l.(0), l.(1)] + centinela *) in
  let second_half = drop l taker (*[l.(3), l.(4)]*) in
  Node [
    (Node first_half, third_pkey, None, 0);
    (Node second_half, third_pkey (* this key is never going to beread *), None, 0)
  ]

let insert_at pos ar v = 
  let len = Array.length ar in
  Array.init (len+1) (fun ind -> 
    if ind = pos then v
    else if ind < pos then ar.(ind)
    else ar.(ind-1) )

let add (t, llist) (v: int) : (database option * bool) * int array = 
  let key = get_pkey (Nil, v, None, 0) in
  let max_degree = 3 in
  let rec aux t' reg_length : (database option * bool) * int array (* (árbol resultante,
  ha_cambiado el hijo *) = 
    match t' with
    | Nil (* arbol vacío *) -> ((Some (Node [(Nil, key, Some v, 0); (Nil, key, None, 0)]), false), Array.make 1 v)

    | Node ((_, k, _, _) :: _) when k=key (* clave primaria duplicada *) -> ((None, false),  llist)

    | Node ((Nil, k, Some _, hleft) as h :: l) when key < k (* Añadir nuevo objeto aquí *) ->
        if (List.length l) + reg_length = max_degree then
          if reg_length = 0 then (* dividir y coger indice 2 *)
            ((Some (divide ([(Nil, key, Some v, hleft); h]@l)), true), insert_at hleft llist v)
          else ((Some (Node ([(Nil, key, Some v, hleft); h]@l)), true), insert_at hleft llist v)
        else
          if reg_length = 0 then
            ((Some (Node ([(Nil, key, Some v, hleft); h]@l)), false), insert_at hleft llist v)
          else ((Some (Node ([(Nil, key, Some v, hleft); h]@l)), false), insert_at hleft llist v)

    | Node ((Nil, _, Some _, _) as h :: l) (* Avanza en la lista l *) -> 
       aux (Node l) (reg_length + 1) >>= (fun ((prt, moved), res_llist) -> (* resulting tree *)
          let rt = match prt with 
          | Node s -> s
          | _ -> raise B_plus_sig.BadTree
        in
         if moved && reg_length = 0 then (* you are at the first element *)
          (*divide the current tree and take the key of the third element*)
          ((Some (divide (h::rt) ), true), res_llist)
         else if moved && not (reg_length = 0) then ((Some (Node (h::rt)), true), res_llist)
          else if not moved && reg_length = 0 then
            ((Some (Node (h::rt)), false), res_llist)
         else ((Some (Node (h::rt)), false), res_llist) 
        )
    | Node [(Nil, _, None, hleft)] (* Centinela, añadir aquí y mover el Centinela *) -> 
        if reg_length = max_degree then (* Divides en dos y propagas *)
          (* El árbol devuelto tendrá como raíz la clave del tercer elemento e
           * hijos el registro acutal a la mitad *)

          ((Some (Node [(Nil, key, Some v, hleft+1); (Nil, key, None, hleft+1)]), true), insert_at (hleft+1) llist v)
        else (* Mueves el centinela *)
          ((Some (Node [(Nil, key, Some v, hleft+1); (Nil, key, None, hleft+1)]), false), insert_at (hleft+1) llist v)
    
    (*-------------------------------*)
    (* Casos para los nodos no hojas *)
    (*-------------------------------*)

    | Node [(s, k, None, _)] ->
      aux s 0 >>= (fun ((prt, moved), res_llist) ->
        let rt = match prt with 
          | Node s -> Node s
          | _ -> raise B_plus_sig.BadTree
        in
        if moved then
            let (new_element, prev_el_new_tree) = begin
              match rt with 
              | Node [(Node _, _, None, _) as c'; (sh, _, None,_)] -> (c', sh)
              | _ -> raise B_plus_sig.BadTree
            end
            in
            let result_list = [new_element; (prev_el_new_tree, k, None, 0)]
            in
            if reg_length = 0 then
              ((Some (Node result_list), false), res_llist)
            else
             ((Some (Node result_list), reg_length+1 > 3), res_llist)
        else
          if reg_length = 0 then
           ((Some (Node [(rt, k, None, 0)]), false), res_llist)
          else 
            ((Some (Node [(rt, k, None, 0)]), false), res_llist)
      ) 

    | Node ((_, k, None, _) as h :: l) when key > k -> (* Avanza en la lista *)
        aux (Node l) (reg_length+1) >>= (fun ((prt, moved), res_llist) ->
          let rt = match prt with 
          | Node s -> s
          | _ -> raise B_plus_sig.BadTree
        in
          if moved && reg_length = 0 then
            ((Some (divide (h::rt)), true), res_llist)
          else if moved && not (reg_length = 0) then
            ((Some (Node (h::rt)), true), res_llist)
          else if not moved && reg_length = 0 then
            ((Some (Node (h::rt)), false), res_llist)
          else
            ((Some (Node (h::rt)), false), res_llist)
        )
    | Node ((s, k, None, _) :: l) -> (* Investiga por debajo *)
(*the first to receive moved=true, means its child got moved, update
           * current register with resulting tree and then check if the
           * resulting register needs to get splitted eventually returning a
           * true moved value *)

        aux s 0 >>= (fun ((prt, moved), res_llist) ->
          let rt = match prt with 
          | Node s -> Node s
          | _ -> raise B_plus_sig.BadTree
        in
          if moved then
            let (new_element, prev_el_new_tree) = begin
              match rt with 
              | Node [(Node _, _, None, _) as c'; (sh, _, None, _)] -> (c', sh)
              | _ -> raise B_plus_sig.BadTree
            end
            in
            let result_list = [new_element; (prev_el_new_tree, k, None, 0)]@l in
            if reg_length = 0 then
              if List.length result_list > 4 then
                ((Some (divide result_list), true), res_llist)
              else
                ((Some (Node result_list), false), res_llist) (*Aquí cortas la propagación del moved*)
            else
              let moving = reg_length + (List.length result_list) > 4 in
              ((Some (Node result_list), moving), res_llist)
          else 
            if reg_length = 0 then
            ((Some (Node ([(rt, k, None, 0)]@l)), false), res_llist)
            else
            ((Some (Node ([(rt, k, None, 0)]@l)), false), res_llist)
        )
    
    | _ (* Resto de casos por arbol mal construido *) -> raise B_plus_sig.BadTree 

  in
  aux t 0      



